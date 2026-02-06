/-
  Folder: directory browser with configurable find depth
  Commands: .fld .dup (push), .fld .inc/.dec (depth), .fld .ent (enter)
-/
import Tc.Fzf
import Tc.View
import Tc.Term

namespace Tc.Folder

-- | Global flag: use --no-sign-request for S3 (set via +n arg)
initialize s3NoSign : IO.Ref Bool ← IO.mkRef false

def setS3NoSign (b : Bool) : IO Unit := s3NoSign.set b
def getS3NoSign : IO Bool := s3NoSign.get

-- | Extra args for S3 commands when --no-sign-request is enabled
def s3Extra : IO (Array String) := do
  if ← getS3NoSign then pure #["--no-sign-request"] else pure #[]

variable {T : Type} [TblOps T] [MemConvert MemTable T]

-- | Strip base path prefix to get relative entry name
-- "/home/x/foo/bar" with base "/home/x/foo" → "bar"
private def stripBase (base path : String) : String :=
  if path.startsWith base then
    let rest := path.drop base.length
    if rest.toString.startsWith "/" then (rest.drop 1).toString else rest.toString
  else if path.startsWith "./" then (path.drop 2).toString
  else path

-- | Format date: "2024-01-05+12:34:56.123" → "2024-01-05 12:34:56"
private def fmtDate (s : String) : String :=
  let s := s.replace "+" " "  -- replace + with space
  let parts := s.splitOn "."  -- drop fractional seconds
  (parts.head!.take 19).toString  -- YYYY-MM-DD HH:MM:SS

-- | Format type: f→space, d→d, l→l
private def fmtType (s : String) : String :=
  if s == "f" then " " else s

-- | Check if path is an S3 URI
def isS3 (path : String) : Bool := path.startsWith "s3://"

-- | Get parent S3 prefix: "s3://bucket/a/b/" → "s3://bucket/a/"
-- Returns none at bucket root ("s3://bucket/")
def s3Parent (path : String) : Option String :=
  -- strip trailing slash, split on "/", drop last segment
  let p := if path.endsWith "/" then (path.take (path.length - 1)).toString else path
  let parts := p.splitOn "/"
  -- s3://bucket → ["s3:", "", "bucket"] (3 parts), that's root
  if parts.length ≤ 3 then none
  else some ("/".intercalate (parts.dropLast) ++ "/")

-- | Join S3 prefix with child name
def s3Join (pfx name : String) : String :=
  if pfx.endsWith "/" then s!"{pfx}{name}" else s!"{pfx}/{name}"

-- | s3Parent returns none at bucket root
theorem s3Parent_none_at_root : s3Parent "s3://bucket/" = none := by native_decide

-- | s3Join with trailing slash doesn't double-slash
theorem s3Join_trailing_slash : s3Join "s3://b/a/" "x" = "s3://b/a/x" := by native_decide

-- | List S3 prefix via `aws s3 ls`, returns TSV matching listDir schema
def listS3 (path : String) : IO String := do
  statusMsg s!"Loading {path} ..."
  let p := if path.endsWith "/" then path else s!"{path}/"
  let extra ← s3Extra
  let out ← IO.Process.output { cmd := "aws", args := #["s3", "ls"] ++ extra ++ #[p] }
  let lines := out.stdout.splitOn "\n" |>.filter (·.length > 0)
  let hdr := "path\tsize\tdate\ttype"
  -- ".." entry if not at bucket root
  let parent := if s3Parent path |>.isSome then "..\t0\t\td" else ""
  let body := lines.map fun line =>
    if line.trimAsciiStart.toString.startsWith "PRE " then
      -- directory: "                           PRE prefix/"
      let name := (line.trimAscii.toString.drop 4).toString  -- strip "PRE "
      let name := if name.endsWith "/" then (name.take (name.length - 1)).toString else name
      s!"{name}\t0\t\td"
    else
      -- file: "2024-01-05 12:34:56   12345 filename"
      let parts := line.trimAscii.toString.splitOn " " |>.filter (·.length > 0)
      if parts.length >= 4 then
        let dt := s!"{parts.getD 0 ""} {parts.getD 1 ""}"
        let sz := parts.getD 2 ""
        let name := parts.drop 3 |> " ".intercalate  -- filename may have spaces
        s!"{name}\t{sz}\t{dt}\t "
      else line
  let entries := if parent.isEmpty then body else [parent] ++ body
  pure (hdr ++ (if entries.isEmpty then "" else "\n" ++ "\n".intercalate entries))

-- | Download S3 file to local temp path, returns local path
def s3Download (s3Path : String) : IO String := do
  statusMsg s!"Downloading {s3Path} ..."
  let _ ← IO.Process.output { cmd := "mkdir", args := #["-p", "/tmp/tc-s3"] }
  let name := s3Path.splitOn "/" |>.getLast? |>.getD "file"
  let local_ := s!"/tmp/tc-s3/{name}"
  let extra ← s3Extra
  let _ ← IO.Process.output { cmd := "aws", args := #["s3", "cp"] ++ extra ++ #[s3Path, local_] }
  pure local_

-- | List directory with find command, returns tab-separated output
-- Cols: path, size, date, type (path first for visibility)
-- Always includes ".." entry to allow navigation to parent
def listDir (path : String) (depth : Nat) : IO String := do
  let p := if path.isEmpty then "." else path
  let out ← IO.Process.output {
    cmd := "find"
    args := #["-L", p, "-maxdepth", toString depth, "-printf", "%y\t%s\t%T+\t%p\n"]
  }
  -- add header, ".." entry, then filtered content (skip directory itself)
  let lines := out.stdout.splitOn "\n" |>.filter (·.length > 0)
  let hdr := "path\tsize\tdate\ttype"
  let parentEntry := "..\t0\t\td"  -- always show ".." for parent navigation
  -- format type, date, strip base path to get relative names
  let body := lines.drop 1 |>.map fun line =>
    let parts := line.splitOn "\t"
    if parts.length >= 4 then
      let typ := fmtType (parts.getD 0 "")
      let sz := parts.getD 1 ""
      let dt := fmtDate (parts.getD 2 "")
      let path := stripBase p (parts.getLast!)
      [path, sz, dt, typ] |> "\t".intercalate
    else line
  pure (hdr ++ "\n" ++ parentEntry ++ (if body.isEmpty then "" else "\n" ++ "\n".intercalate body))

-- | Parse find output to MemTable (tab-separated)
def toMemTable (output : String) : Except String MemTable := MemTable.fromTsv output

-- | View file with bat (if available) or less
def viewFile (path : String) : IO Unit := do
  if ← Fzf.getTestMode then return  -- skip pager in test mode
  Term.shutdown  -- restore terminal before spawning pager
  let hasBat ← IO.Process.output { cmd := "which", args := #["bat"] }
  if hasBat.exitCode == 0 then
    let _ ← IO.Process.spawn { cmd := "bat", args := #["--paging=always", path], stdin := .inherit, stdout := .inherit, stderr := .inherit } >>= (·.wait)
  else
    let _ ← IO.Process.spawn { cmd := "less", args := #[path], stdin := .inherit, stdout := .inherit, stderr := .inherit } >>= (·.wait)
  let _ ← Term.init  -- reinit terminal after pager

-- | Open data file (csv/parquet) as table view, returns new stack or none
def openDataFile (s : ViewStack T) (path : String) : IO (Option (ViewStack T)) := do
  match ← TblOps.fromFile path with
  | some tbl => match View.fromTbl tbl path with
    | some v => pure (some (s.push v))
    | none => pure none
  | none => pure none

-- | Get path column value from current row
def curPath (v : View T) : Option String := do
  guard (v.vkind matches .fld _ _)
  let tbl : MemTable ← MemConvert.unwrap v.nav.tbl
  let pathCol ← tbl.names.idxOf? "path"
  let row := v.nav.row.cur.val
  pure ((tbl.cols.getD pathCol default).get row).toRaw

-- | Get type column value from current row (d=dir, space=file, l=link)
def curType (v : View T) : Option Char := do
  guard (v.vkind matches .fld _ _)
  let tbl : MemTable ← MemConvert.unwrap v.nav.tbl
  let typeCol ← tbl.names.idxOf? "type"
  let row := v.nav.row.cur.val
  let t := ((tbl.cols.getD typeCol default).get row).toRaw
  t.toList.head?

-- | Create folder view from path with given depth
def mkView (path : String) (depth : Nat) : IO (Option (View T)) := do
  if isS3 path then
    let output ← listS3 path
    match toMemTable output with
    | .error _ => pure none
    | .ok tbl =>
      let disp := (path.drop 5).toString |>.splitOn "/" |>.filter (·.length > 0) |>.getLast? |>.getD path
      pure <| View.fromTbl (MemConvert.wrap tbl : T) path |>.map fun v =>
        { v with vkind := .fld path depth, disp := disp }
  else
    -- resolve to absolute path
    let rp ← IO.Process.output { cmd := "realpath", args := #[path] }
    let absPath := if rp.exitCode == 0 then rp.stdout.trimAscii.toString else path
    let output ← listDir path depth
    match toMemTable output with
    | .error _ => pure none
    | .ok tbl =>
      let disp := absPath.splitOn "/" |>.getLast? |>.getD absPath
      pure <| View.fromTbl (MemConvert.wrap tbl : T) absPath |>.map fun v =>
        { v with vkind := .fld absPath depth, disp := disp }

-- | Push new folder view onto stack (use current path or ".")
def push (s : ViewStack T) : IO (Option (ViewStack T)) := do
  let path := match curPath s.cur with
    | some p => p
    | none => match s.cur.vkind with
      | .fld p _ => p  -- already a folder view, use its path
      | _ => "."       -- default to current directory
  match ← mkView path 1 with
  | some v => pure (some (s.push v))
  | none => pure none

-- | Join parent path with entry name
private def joinPath (parent entry : String) : String :=
  if parent == "." then s!"./{entry}"
  else if parent.endsWith "/" then s!"{parent}{entry}"
  else s!"{parent}/{entry}"

-- | Enter directory or view file based on current row
def enter (s : ViewStack T) : IO (Option (ViewStack T)) := do
  let curDir := match s.cur.vkind with | .fld dir _ => dir | _ => "."
  let s3 := isS3 curDir
  match curType s.cur, curPath s.cur with
  | some 'd', some p =>  -- directory: push new folder view
    -- ".." navigates to parent (pop if possible, else go to parent path)
    if p == ".." || p.endsWith "/.." then
      if s3 then
        match s3Parent curDir with
        | some parent =>
          match ← mkView (T := T) parent 1 with
          | some v => pure (some (s.setCur v))
          | none => pure (some s)
        | none => pure (some s)  -- at bucket root, no-op
      else if let some s' := s.pop then pure (some s')
      else  -- at root, go to actual parent
        let depth := match s.cur.vkind with | .fld _ d => d | _ => 1
        match ← mkView (T := T) ".." depth with
        | some v => pure (some (s.setCur v))
        | none => pure (some s)
    else
      let depth := match s.cur.vkind with | .fld _ d => d | _ => 1
      let fullPath := if s3 then s3Join curDir (p ++ "/") else joinPath curDir p
      match ← mkView fullPath depth with
      | some v => pure (some (s.push v))
      | none => pure (some s)
  | some ' ', some p =>  -- regular file: open data or view
    if s3 then
      let s3Path := s3Join curDir p
      if p.endsWith ".csv" || p.endsWith ".parquet" then
        let local_ ← s3Download s3Path
        match ← openDataFile s local_ with
        | some s' => pure (some s')
        | none => pure (some s)
      else pure (some s)  -- non-data S3 files: no-op
    else
      let fullPath := joinPath curDir p
      if p.endsWith ".csv" || p.endsWith ".parquet" then
        match ← openDataFile s fullPath with
        | some s' => pure (some s')
        | none => viewFile fullPath; pure (some s)  -- fallback to viewer
      else
        viewFile fullPath; pure (some s)
  | some 'l', some p =>  -- symlink: check if dir or file (local only)
    if s3 then pure (some s)
    else
      let fullPath := joinPath curDir p
      let stat ← IO.Process.output { cmd := "test", args := #["-d", fullPath] }
      if stat.exitCode == 0 then  -- is directory
        let depth := match s.cur.vkind with | .fld _ d => d | _ => 1
        match ← mkView fullPath depth with
        | some v => pure (some (s.push v))
        | none => pure (some s)
      else if fullPath.endsWith ".csv" || fullPath.endsWith ".parquet" then
        match ← openDataFile s fullPath with
        | some s' => pure (some s')
        | none => viewFile fullPath; pure (some s)
      else
        viewFile fullPath; pure (some s)
  | _, _ => pure none

-- | Get trash command (trash-put or gio trash)
def trashCmd : IO (Option (String × Array String)) := do
  let tp ← IO.Process.output { cmd := "which", args := #["trash-put"] }
  if tp.exitCode == 0 then return some ("trash-put", #[])
  let gio ← IO.Process.output { cmd := "which", args := #["gio"] }
  if gio.exitCode == 0 then return some ("gio", #["trash"])
  return none

-- | Get full paths of selected rows, or current row if none selected
def selPaths (v : View T) : Array String :=
  match v.vkind with
  | .fld curDir _ =>
    let tbl? : Option MemTable := MemConvert.unwrap v.nav.tbl
    match tbl?, tbl?.bind (fun t => t.names.idxOf? "path") with
    | some tbl, some pathCol =>
      let rows := if v.nav.row.sels.isEmpty then #[v.nav.row.cur.val] else v.nav.row.sels
      let join := if isS3 curDir then s3Join else joinPath
      rows.map fun r => join curDir ((tbl.cols.getD pathCol default).get r).toRaw
    | _, _ => #[]
  | _ => #[]

-- | Draw centered dialog box
def drawDialog (title : String) (lines : Array String) (footer : String) : IO Unit := do
  let w ← Term.width; let h ← Term.height
  let maxLen := lines.foldl (fun m l => max m l.length) (max title.length footer.length)
  let boxW := maxLen + 4  -- padding
  let boxH := lines.size + 4  -- title + blank + lines + footer
  let x0 := (w.toNat - boxW) / 2
  let y0 := (h.toNat - boxH) / 2
  let fg := Term.white; let bg := Term.blue
  -- top border
  Term.print x0.toUInt32 y0.toUInt32 fg bg ("┌" ++ "".pushn '─' (boxW - 2) ++ "┐")
  -- title
  let tpad := (boxW - 2 - title.length) / 2
  Term.print x0.toUInt32 (y0 + 1).toUInt32 fg bg ("│" ++ "".pushn ' ' tpad ++ title ++ "".pushn ' ' (boxW - 2 - tpad - title.length) ++ "│")
  -- blank line
  Term.print x0.toUInt32 (y0 + 2).toUInt32 fg bg ("│" ++ "".pushn ' ' (boxW - 2) ++ "│")
  -- content lines
  for i in [:lines.size] do
    let ln := (lines[i]!.take (boxW - 4)).toString
    Term.print x0.toUInt32 (y0 + 3 + i).toUInt32 fg bg ("│ " ++ ln ++ "".pushn ' ' (boxW - 3 - ln.length) ++ "│")
  -- footer
  let fpad := (boxW - 2 - footer.length) / 2
  Term.print x0.toUInt32 (y0 + 3 + lines.size).toUInt32 fg bg ("│" ++ "".pushn ' ' fpad ++ footer ++ "".pushn ' ' (boxW - 2 - fpad - footer.length) ++ "│")
  -- bottom border
  Term.print x0.toUInt32 (y0 + 4 + lines.size).toUInt32 fg bg ("└" ++ "".pushn '─' (boxW - 2) ++ "┘")
  Term.present

-- | Wait for y/n keypress
partial def waitYN : IO Bool := do
  let ev ← Term.pollEvent
  if ev.type != Term.eventKey then waitYN
  else if ev.ch == 'y'.toNat.toUInt32 || ev.ch == 'Y'.toNat.toUInt32 then pure true
  else if ev.ch == 'n'.toNat.toUInt32 || ev.ch == 'N'.toNat.toUInt32 || ev.key == Term.keyEsc then pure false
  else waitYN

-- | Confirm deletion with popup dialog (auto-decline in test mode)
def confirmDel (paths : Array String) : IO Bool := do
  if ← Fzf.getTestMode then return false
  let title := s!"Delete {paths.size} file(s)?"
  let shown := paths.toList.take 6 |>.toArray
  let lines := if paths.size > 6
    then shown.push s!"... +{paths.size - 6} more"
    else shown
  -- add trash command info
  let cmd ← trashCmd
  let cmdInfo := match cmd with
    | some ("trash-put", _) => "via trash-put (undo: trash-restore)"
    | some ("gio", _) => "via gio trash (undo: gio trash --restore)"
    | _ => "no trash command found"
  let lines := lines.push "" ++ #[cmdInfo, "[Y]es  [N]o"]
  drawDialog title lines ""
  waitYN

-- | Trash files, returns true if all succeeded
def trashFiles (paths : Array String) : IO Bool := do
  let some (cmd, baseArgs) ← trashCmd | return false
  let mut ok := true
  for p in paths do
    let r ← IO.Process.output { cmd := cmd, args := baseArgs ++ #[p] }
    if r.exitCode != 0 then ok := false
  pure ok

-- | Delete selected files and refresh view (no-op for S3)
def del (s : ViewStack T) : IO (Option (ViewStack T)) := do
  if !(s.cur.vkind matches .fld _ _) then return none
  let curDir := match s.cur.vkind with | .fld dir _ => dir | _ => ""
  if isS3 curDir then return some s
  let paths := selPaths s.cur
  if paths.isEmpty then return none
  if !(← confirmDel paths) then return some s  -- cancelled
  let _ ← trashFiles paths
  -- refresh view, preserve cursor (clamped to new row count)
  match s.cur.vkind with
  | .fld path depth =>
    match ← mkView (T := T) path depth with
    | some v =>
      let row := min s.cur.nav.row.cur.val (if v.nRows > 0 then v.nRows - 1 else 0)
      let v' := View.fromTbl v.nav.tbl path (row := row) |>.map fun x =>
        { x with vkind := .fld path depth, disp := s.cur.disp }
      pure (v'.map s.setCur)
    | none => pure (some s)
  | _ => pure (some s)

-- | Adjust find depth (+1 or -1, min 1). No-op for S3.
def setDepth (s : ViewStack T) (delta : Int) : IO (Option (ViewStack T)) := do
  match s.cur.vkind with
  | .fld path depth =>
    if isS3 path then return some s
    let newDepth := max 1 ((depth : Int) + delta).toNat
    if newDepth == depth then pure (some s)  -- no change
    else match ← mkView (T := T) path newDepth with
      | some v =>
        -- preserve row position if possible
        let row := min s.cur.nav.row.cur.val (v.nRows - 1)
        let v' := View.fromTbl v.nav.tbl path (row := row) |>.map fun x =>
          { x with vkind := .fld path newDepth, disp := s.cur.disp }
        pure (v'.map s.setCur)
      | none => pure (some s)  -- keep current if refresh fails
  | _ => pure none

-- | Pure update: returns Effect for IO operations
def update (s : ViewStack T) (cmd : Cmd) : Option (ViewStack T × Effect) :=
  match cmd with
  | .fld .dup => some (s, .folderPush)                          -- D: push folder view
  | .fld .inc => some (s, .folderDepth 1)                       -- +d: increase depth
  | .fld .dec => some (s, .folderDepth (-1))                    -- -d: decrease depth
  | .colSel .del =>                                             -- d: delete files
    if s.cur.vkind matches .fld _ _ then some (s, .folderDel) else none
  | .fld .ent =>                                                -- Enter: enter dir/file
    if s.cur.vkind matches .fld _ _ then some (s, .folderEnter) else none
  | _ => none

-- | Execute folder commands (IO version for backward compat)
def exec (s : ViewStack T) (cmd : Cmd) : IO (Option (ViewStack T)) :=
  match cmd with
  | .fld .dup => push s               -- D: push folder view
  | .fld .inc => setDepth s 1         -- +d: increase depth
  | .fld .dec => setDepth s (-1)      -- -d: decrease depth
  | .colSel .del =>                   -- d: delete files in folder view
    if s.cur.vkind matches .fld _ _ then del s else pure none
  | .fld .ent =>                      -- Enter: only for fld views
    if s.cur.vkind matches .fld _ _ then enter s else pure none
  | _ => pure none

end Tc.Folder
