/-
  Folder: directory browser with configurable find depth
  Commands: .fld .dup (push), .fld .inc/.dec (depth), .fld .ent (enter)
-/
import Tc.Fzf
import Tc.View
import Tc.Term
import Tc.S3
import Tc.Table
import Tc.HF
import Tc.Remote

namespace Tc.Folder

-- | Remote backend: list, parent, resolve path for opening
private structure Backend where
  list     : String → IO String
  parent   : String → Option String
  resolve  : String → IO String  -- S3: download; HF/local: identity

private def s3Back : Backend where
  list     := S3.list
  parent   := S3.parent
  resolve  := S3.download

private def hfBack : Backend where
  list     := HF.list
  parent   := HF.parent
  resolve  := fun p => do statusMsg s!"Loading {p} ..."; pure p

private def backend? (path : String) : Option Backend :=
  if S3.isS3 path then some s3Back
  else if HF.isHF path then some hfBack
  else none

-- | Strip base path prefix to get relative entry name
private def stripBase (base path : String) : String :=
  if path.startsWith base then
    let rest := path.drop base.length
    if rest.toString.startsWith "/" then (rest.drop 1).toString else rest.toString
  else if path.startsWith "./" then (path.drop 2).toString
  else path

-- | Format date: "2024-01-05+12:34:56.123" → "2024-01-05 12:34:56"
private def fmtDate (s : String) : String :=
  let s := s.replace "+" " "
  let parts := s.splitOn "."
  ((parts.head?.getD "").take 19).toString

-- | Format type: f→space, d→d, l→l
private def fmtType (s : String) : String :=
  if s == "f" then " " else s

-- | List directory with find command, returns tab-separated output
def listDir (path : String) (depth : Nat) : IO String := do
  let p := if path.isEmpty then "." else path
  let out ← IO.Process.output {
    cmd := "find"
    args := #["-L", p, "-maxdepth", toString depth, "-printf", "%y\t%s\t%T+\t%p\n"]
  }
  let lines := out.stdout.splitOn "\n" |>.filter (·.length > 0)
  let hdr := "path\tsize\tdate\ttype"
  let parentEntry := "..\t0\t\td"
  let body := lines.drop 1 |>.map fun line =>
    let parts := line.splitOn "\t"
    if parts.length >= 4 then
      let typ := fmtType (parts.getD 0 "")
      let sz := parts.getD 1 ""
      let dt := fmtDate (parts.getD 2 "")
      let path := stripBase p (parts.getLastD "")
      [path, sz, dt, typ] |> "\t".intercalate
    else line
  pure (hdr ++ "\n" ++ parentEntry ++ (if body.isEmpty then "" else "\n" ++ "\n".intercalate body))

-- | View file with bat (if available) or less
def viewFile (path : String) : IO Unit := do
  if ← Fzf.getTestMode then return
  Term.shutdown
  let hasBat ← IO.Process.output { cmd := "which", args := #["bat"] }
  if hasBat.exitCode == 0 then
    let _ ← IO.Process.spawn { cmd := "bat", args := #["--paging=always", path], stdin := .inherit, stdout := .inherit, stderr := .inherit } >>= (·.wait)
  else
    let _ ← IO.Process.spawn { cmd := "less", args := #[path], stdin := .inherit, stdout := .inherit, stderr := .inherit } >>= (·.wait)
  let _ ← Term.init

-- | Open data file (csv/parquet) as table view
def openDataFile (s : ViewStack Table) (path : String) : IO (Option (ViewStack Table)) := do
  match ← TblOps.fromFile (α := Table) path with
  | some tbl => match View.fromTbl tbl path with
    | some v => pure (some (s.push v))
    | none => pure none
  | none => pure none

-- | Is file a data format we can open as a table?
private def isDataFile (p : String) : Bool :=
  p.endsWith ".csv" || p.endsWith ".parquet"

-- | Get path column value from current row
def curPath (v : View Table) : IO (Option String) := do
  if !(v.vkind matches .fld _ _) then return none
  let names := TblOps.colNames v.nav.tbl
  let some pathCol := names.idxOf? "path" | return none
  let cols ← TblOps.getCols v.nav.tbl #[pathCol] v.nav.row.cur.val (v.nav.row.cur.val + 1)
  let c := cols.getD 0 default
  return some (c.get 0).toRaw

-- | Get type column value from current row
def curType (v : View Table) : IO (Option Char) := do
  if !(v.vkind matches .fld _ _) then return none
  let names := TblOps.colNames v.nav.tbl
  let some typeCol := names.idxOf? "type" | return none
  let cols ← TblOps.getCols v.nav.tbl #[typeCol] v.nav.row.cur.val (v.nav.row.cur.val + 1)
  let c := cols.getD 0 default
  let t := (c.get 0).toRaw
  return t.toList.head?

-- | Build folder view from TSV content
private def mkViewFromTsv (tsv : String) (path : String) (depth : Nat) (disp : String)
    : IO (Option (View Table)) := do
  match ← AdbcTable.fromTsv tsv with
  | some adbc =>
    pure <| View.fromTbl (Table.adbc adbc) path |>.map fun v =>
      { v with vkind := .fld path depth, disp }
  | none => pure none

-- | Create folder view
def mkView (path : String) (depth : Nat) : IO (Option (View Table)) := do
  match backend? path with
  | some b =>
    mkViewFromTsv (← b.list path) path depth (Remote.dispName path)
  | none =>
    let rp ← IO.Process.output { cmd := "realpath", args := #[path] }
    let absPath := if rp.exitCode == 0 then rp.stdout.trimAscii.toString else path
    let disp := absPath.splitOn "/" |>.getLast? |>.getD absPath
    mkViewFromTsv (← listDir path depth) absPath depth disp

-- | Push new folder view onto stack
def push (s : ViewStack Table) : IO (Option (ViewStack Table)) := do
  let path ← do
    match ← curPath s.cur with
    | some p => pure p
    | none => match s.cur.vkind with
      | .fld p _ => pure p
      | _ => pure "."
  match ← mkView path 1 with
  | some v => pure (some (s.push v))
  | none => pure none

-- | Join parent path with entry name (works for local, S3, HF)
private def joinPath (parent entry : String) : String :=
  if parent == "." then s!"./{entry}"
  else Remote.join parent entry

-- | Enter directory or view file based on current row
def enter (s : ViewStack Table) : IO (Option (ViewStack Table)) := do
  let curDir := match s.cur.vkind with | .fld dir _ => dir | _ => "."
  let back := backend? curDir
  match ← curType s.cur, ← curPath s.cur with
  | some 'd', some p =>  -- directory
    if p == ".." || p.endsWith "/.." then
      match back with
      | some b =>
        match b.parent curDir with
        | some par =>
          match ← mkView par 1 with
          | some v => pure (some (s.setCur v))
          | none => pure (some s)
        | none => pure (some s)
      | none =>
        if let some s' := s.pop then pure (some s')
        else
          let depth := match s.cur.vkind with | .fld _ d => d | _ => 1
          match ← mkView ".." depth with
          | some v => pure (some (s.setCur v))
          | none => pure (some s)
    else
      let depth := match s.cur.vkind with | .fld _ d => d | _ => 1
      let fullPath := joinPath curDir (if back.isSome then p ++ "/" else p)
      match ← mkView fullPath depth with
      | some v => pure (some (s.push v))
      | none => pure (some s)
  | some ' ', some p =>  -- regular file
    let fullPath := joinPath curDir p
    if isDataFile p then
      let openPath ← match back with
        | some b => b.resolve fullPath
        | none => pure fullPath
      match ← openDataFile s openPath with
      | some s' => pure (some s')
      | none => if back.isNone then viewFile fullPath; pure (some s) else pure (some s)
    else if back.isNone then viewFile fullPath; pure (some s)
    else pure (some s)
  | some 'l', some p =>  -- symlink (local only)
    if back.isSome then pure (some s)
    else
      let fullPath := joinPath curDir p
      let stat ← IO.Process.output { cmd := "test", args := #["-d", fullPath] }
      if stat.exitCode == 0 then
        let depth := match s.cur.vkind with | .fld _ d => d | _ => 1
        match ← mkView fullPath depth with
        | some v => pure (some (s.push v))
        | none => pure (some s)
      else if isDataFile fullPath then
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
def selPaths (v : View Table) : IO (Array String) := do
  match v.vkind with
  | .fld curDir _ =>
    let names := TblOps.colNames v.nav.tbl
    let some pathCol := names.idxOf? "path" | return #[]
    let cols ← TblOps.getCols v.nav.tbl #[pathCol] 0 v.nRows
    let c := cols.getD 0 default
    let rows := if v.nav.row.sels.isEmpty then #[v.nav.row.cur.val] else v.nav.row.sels
    return rows.map fun r => joinPath curDir (c.get r).toRaw
  | _ => return #[]

-- | Draw centered dialog box
def drawDialog (title : String) (lines : Array String) (footer : String) : IO Unit := do
  let w ← Term.width; let h ← Term.height
  let maxLen := lines.foldl (fun m l => max m l.length) (max title.length footer.length)
  let boxW := maxLen + 4
  let boxH := lines.size + 4
  let x0 := (w.toNat - boxW) / 2
  let y0 := (h.toNat - boxH) / 2
  let fg := Term.white; let bg := Term.blue
  Term.print x0.toUInt32 y0.toUInt32 fg bg ("┌" ++ "".pushn '─' (boxW - 2) ++ "┐")
  let tpad := (boxW - 2 - title.length) / 2
  Term.print x0.toUInt32 (y0 + 1).toUInt32 fg bg ("│" ++ "".pushn ' ' tpad ++ title ++ "".pushn ' ' (boxW - 2 - tpad - title.length) ++ "│")
  Term.print x0.toUInt32 (y0 + 2).toUInt32 fg bg ("│" ++ "".pushn ' ' (boxW - 2) ++ "│")
  for i in [:lines.size] do
    let ln := ((lines.getD i "").take (boxW - 4)).toString
    Term.print x0.toUInt32 (y0 + 3 + i).toUInt32 fg bg ("│ " ++ ln ++ "".pushn ' ' (boxW - 3 - ln.length) ++ "│")
  let fpad := (boxW - 2 - footer.length) / 2
  Term.print x0.toUInt32 (y0 + 3 + lines.size).toUInt32 fg bg ("│" ++ "".pushn ' ' fpad ++ footer ++ "".pushn ' ' (boxW - 2 - fpad - footer.length) ++ "│")
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

-- | Delete selected files and refresh view (no-op for remote)
def del (s : ViewStack Table) : IO (Option (ViewStack Table)) := do
  if !(s.cur.vkind matches .fld _ _) then return none
  let curDir := match s.cur.vkind with | .fld dir _ => dir | _ => ""
  if (backend? curDir).isSome then return some s
  let paths ← selPaths s.cur
  if paths.isEmpty then return none
  if !(← confirmDel paths) then return some s
  let _ ← trashFiles paths
  match s.cur.vkind with
  | .fld path depth =>
    match ← mkView path depth with
    | some v =>
      let row := min s.cur.nav.row.cur.val (if v.nRows > 0 then v.nRows - 1 else 0)
      let v' := View.fromTbl v.nav.tbl path (row := row) |>.map fun x =>
        { x with vkind := .fld path depth, disp := s.cur.disp }
      pure (v'.map s.setCur)
    | none => pure (some s)
  | _ => pure (some s)

-- | Adjust find depth (+1 or -1, min 1). No-op for remote.
def setDepth (s : ViewStack Table) (delta : Int) : IO (Option (ViewStack Table)) := do
  match s.cur.vkind with
  | .fld path depth =>
    if (backend? path).isSome then return some s
    let newDepth := max 1 ((depth : Int) + delta).toNat
    if newDepth == depth then pure (some s)
    else match ← mkView path newDepth with
      | some v =>
        let row := min s.cur.nav.row.cur.val (v.nRows - 1)
        let v' := View.fromTbl v.nav.tbl path (row := row) |>.map fun x =>
          { x with vkind := .fld path newDepth, disp := s.cur.disp }
        pure (v'.map s.setCur)
      | none => pure (some s)
  | _ => pure none

-- | Pure update: returns Effect for IO operations
def update (s : ViewStack Table) (cmd : Cmd) : Option (ViewStack Table × Effect) :=
  match cmd with
  | .fld .dup => some (s, .folderPush)
  | .fld .inc => some (s, .folderDepth 1)
  | .fld .dec => some (s, .folderDepth (-1))
  | .colSel .del =>
    if s.cur.vkind matches .fld _ _ then some (s, .folderDel) else none
  | .fld .ent =>
    if s.cur.vkind matches .fld _ _ then some (s, .folderEnter) else none
  | _ => none

end Tc.Folder
