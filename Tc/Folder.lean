/-
  Folder: directory browser with configurable find depth
  Commands: .fld .dup (push), .fld .inc/.dec (depth), .fld .ent (enter)
-/
import Tc.FileFormat
import Tc.Fzf
import Tc.View
import Tc.Term
import Tc.Theme
import Tc.SourceConfig
import Tc.Data.ADBC.Ops

namespace Tc.Folder

-- | Strip base path prefix to get relative entry name
private def stripBase (base path : String) : String :=
  if path.startsWith base then
    let rest := path.drop base.length
    if rest.toString.startsWith "/" then rest.drop 1 |>.toString else rest.toString
  else if path.startsWith "./" then path.drop 2 |>.toString
  else path

-- | Format date: "2024-01-05+12:34:56.123" → "2024-01-05 12:34:56"
private def fmtDate (s : String) : String :=
  s.replace "+" " " |>.splitOn "." |>.head?.getD "" |>.take 19 |>.toString

-- | Format type: f→file, d→dir, l→symlink
private def fmtType (s : String) : String :=
  match s with
  | "f" => "file"
  | "d" => "dir"
  | "l" => "symlink"
  | x => x

-- | List directory with find command, returns tab-separated output
def listDir (path : String) (depth : Nat) : IO String := do
  let p := if path.isEmpty then "." else path
  let out ← Log.run "find" "find" #["-H", p, "-maxdepth", toString depth, "-printf", "%y\t%s\t%T+\t%p\n"]
  let lines := out.stdout.splitOn "\n" |>.filter (!·.isEmpty)
  let hdr := "name\tsize\tmodified\ttype"
  let parentEntry := "..\t0\t\tdir"
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

-- | Find path column index (tries "path", "name", "id" in order)
private def pathColIdx (names : Array String) : Option Nat :=
  names.idxOf? "path" <|> names.idxOf? "name" <|> names.idxOf? "id"

-- | Get single cell value as string from current row
private def cellStr (v : View AdbcTable) (colIdx : Nat) : IO String := do
  let cols ← TblOps.getCols v.nav.tbl #[colIdx] v.nav.row.cur (v.nav.row.cur + 1)
  return cols.getD 0 default |>.get 0 |>.toRaw

-- | Get path column value from current row
def curPath (v : View AdbcTable) : IO (Option String) := do
  if !(v.vkind matches .fld _ _) then return none
  let some col := pathColIdx v.nav.colNames | return none
  return some (← cellStr v col)

-- | Get type column value from current row
-- Normalizes: 'f'/"file" → 'f', 'd'/"dir" → 'd', ' ' (HF/S3 file) → 'f'
def curType (v : View AdbcTable) : IO (Option Char) := do
  if !(v.vkind matches .fld _ _) then return none
  let some typeCol := v.nav.colNames.idxOf? "type" | return some 'f'
  match (← cellStr v typeCol).toList.head? with
  | some ' ' => return some 'f'  -- HF/S3 space = file
  | other => return other

-- | Build folder view from AdbcTable
private def mkFldView (adbc : AdbcTable) (path : String) (depth : Nat) (disp : String)
    (grp : Array String := #[]) : Option (View AdbcTable) :=
  View.fromTbl adbc path (grp := grp) |>.map fun v =>
    { v with vkind := .fld path depth, disp }

-- | Create folder view — config-driven listing, local fallback
def mkView (path : String) (depth : Nat) : IO (Option (View AdbcTable)) := do
  match ← SourceConfig.findSource path with
  | some cfg =>
    let some adbc ← cfg.runList path | return none
    let grp := if cfg.grp.isEmpty then #[] else #[cfg.grp]
    pure (mkFldView adbc path depth (Remote.dispName path) (grp := grp))
  | none =>
    let rp ← IO.Process.output { cmd := "realpath", args := #[path] }
    let absPath := if rp.exitCode == 0 then rp.stdout.trimAscii.toString else path
    let disp := absPath.splitOn "/" |>.getLast? |>.getD absPath
    let some adbc ← AdbcTable.fromTsv (← listDir path depth) | return none
    pure (mkFldView adbc absPath depth disp)

-- | Push new folder view onto stack
def push (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
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

-- | Try to create a view; push or setCur, fallback to original stack
private def tryView (s : ViewStack AdbcTable) (path : String) (depth : Nat) (push? : Bool)
    : IO (Option (ViewStack AdbcTable)) := do
  match ← mkView path depth with
  | some v => pure (some (if push? then s.push v else s.setCur v))
  | none => pure (some s)

-- | Get current folder depth
private def curDepth (s : ViewStack AdbcTable) : Nat :=
  match s.cur.vkind with | .fld _ d => d | _ => 1

-- | Go to parent directory (backspace key) — works for all folder backends
def goParent (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  let dir := s.cur.curDir
  match ← SourceConfig.findSource dir with
  | some c => match c.parent dir with
    | some par => tryView s par 1 false
    | none => pure (some s)
  | none => match s.pop with
    | some s' => pure (some s')
    | none => tryView s (dir ++ "/..") (curDepth s) false

-- | Try to open a file as data, fall back to viewer
private def openFile (s : ViewStack AdbcTable) (curDir p : String) (cfg : Option SourceConfig.Config)
    : IO (Option (ViewStack AdbcTable)) := do
  let fullPath := joinPath curDir p
  if FileFormat.isDataFile p then
    let openPath ← match cfg with | some c => c.resolve fullPath | none => pure fullPath
    match ← FileFormat.openFile openPath with
    | some v => return some (s.push v)
    | none => if cfg.isNone then FileFormat.viewFile fullPath
  else
    let viewPath ← match cfg with | some c => c.runDownload fullPath | none => pure fullPath
    if p.endsWith ".gz" then
      if let some v ← FileFormat.tryReadCsv viewPath then return some (s.push v)
    FileFormat.viewFile viewPath
  pure (some s)

-- | Enter directory or view file based on current row
def enter (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  let curDir := s.cur.curDir
  let cfg ← SourceConfig.findSource curDir
  -- Attach-based: FileFormat or config-driven (e.g. pg://)
  let isAttach := (FileFormat.find? curDir |>.map (·.attach) |>.getD false)
    || (cfg |>.map (·.attach) |>.getD false)
  if isAttach then
    let some tableName ← curPath s.cur | return some s
    let some (adbc, keys) ← AdbcTable.fromDuckDBTable tableName | return some s
    let some v := View.fromTbl adbc s!"{curDir}:{tableName}" (grp := keys) | return some s
    return some (s.push v)
  match ← curType s.cur, ← curPath s.cur with
  | some 'd', some p =>
    if p == ".." || p.endsWith "/.." then goParent s
    else
      let fullPath := match cfg with
        | some c => joinPath curDir (if c.dirSuffix then p ++ "/" else p)
        | none => joinPath curDir p
      tryView s fullPath (curDepth s) true
  | some 'f', some p => do
      -- Config-driven enter: script cmd → JSON, or enterUrl redirect
      if let some c := cfg then
        if !c.script.isEmpty then
          match ← c.runEnter p with
          | some adbc => match View.fromTbl adbc s!"{c.pfx}{p}" with
            | some v => return some (s.push v)
            | none => return some s
          | none => return some s
        if !c.enterUrl.isEmpty then
          return ← tryView s (SourceConfig.expand c.enterUrl #[("name", p)]) (curDepth s) true
      openFile s curDir p cfg
  | some 's', some p =>
    if cfg.isSome then pure (some s) else
    let fullPath := joinPath curDir p
    let stat ← IO.Process.output { cmd := "test", args := #["-d", fullPath] }
    if stat.exitCode == 0 then tryView s fullPath (curDepth s) true
    else openFile s curDir p none
  | _, _ => pure none

-- | Get trash command (trash-put or gio trash)
def trashCmd : IO (Option (String × Array String)) := do
  let tp ← IO.Process.output { cmd := "which", args := #["trash-put"] }
  if tp.exitCode == 0 then return some ("trash-put", #[])
  let gio ← IO.Process.output { cmd := "which", args := #["gio"] }
  if gio.exitCode == 0 then return some ("gio", #["trash"])
  return none

-- | Get full paths of selected rows, or current row if none selected
def selPaths (v : View AdbcTable) : IO (Array String) := do
  match v.vkind with
  | .fld curDir _ =>
    let some pathCol := pathColIdx v.nav.colNames | return #[]
    let cols ← TblOps.getCols v.nav.tbl #[pathCol] 0 v.nRows
    let c := cols.getD 0 default
    let rows := if v.nav.row.sels.isEmpty then #[v.nav.row.cur] else v.nav.row.sels
    return rows.map fun r => c.get r |>.toRaw |> joinPath curDir
  | _ => return #[]

-- | Draw centered dialog box
def drawDialog (title : String) (lines : Array String) (footer : String) : IO Unit := do
  let w ← Term.width; let h ← Term.height
  let maxLen := lines.foldl (fun m l => max m l.length) (max title.length footer.length)
  let boxW := maxLen + 4
  let boxH := lines.size + 4
  let x0 := (w.toNat - boxW) / 2
  let y0 := (h.toNat - boxH) / 2
  let s ← Theme.getStyles
  let fg := Theme.styleFg s Theme.sBar; let bg := Theme.styleBg s Theme.sBar
  Term.print x0.toUInt32 y0.toUInt32 fg bg ("┌" ++ "".pushn '─' (boxW - 2) ++ "┐")
  let tpad := (boxW - 2 - title.length) / 2
  Term.print x0.toUInt32 (y0 + 1).toUInt32 fg bg ("│" ++ "".pushn ' ' tpad ++ title ++ "".pushn ' ' (boxW - 2 - tpad - title.length) ++ "│")
  Term.print x0.toUInt32 (y0 + 2).toUInt32 fg bg ("│" ++ "".pushn ' ' (boxW - 2) ++ "│")
  for i in [:lines.size] do
    let ln := lines.getD i "" |>.take (boxW - 4) |>.toString
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
  let cmdInfo ← trashCmd |>.map fun
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
    let r ← Log.run "trash" cmd (baseArgs ++ #[p])
    if r.exitCode != 0 then ok := false
  pure ok

-- | Refresh folder view at path/depth, preserving cursor row
private def refreshView (s : ViewStack AdbcTable) (path : String) (depth : Nat)
    : IO (Option (ViewStack AdbcTable)) := do
  match ← mkView path depth with
  | some v =>
    let row := min s.cur.nav.row.cur (if v.nRows > 0 then v.nRows - 1 else 0)
    pure (View.fromTbl v.nav.tbl path (row := row) |>.map fun x =>
      s.setCur { x with vkind := .fld path depth, disp := s.cur.disp })
  | none => pure (some s)

def del (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  let .fld path depth := s.cur.vkind | return none
  if (← SourceConfig.findSource path).isSome then return some s
  let paths ← selPaths s.cur
  if paths.isEmpty then return none
  if !(← confirmDel paths) then return some s
  let _ ← trashFiles paths
  refreshView s path depth

def setDepth (s : ViewStack AdbcTable) (delta : Int) : IO (Option (ViewStack AdbcTable)) := do
  let .fld path depth := s.cur.vkind | return none
  if (← SourceConfig.findSource path).isSome then return some s
  let newDepth := max 1 ((depth : Int) + delta).toNat
  if newDepth == depth then pure (some s) else refreshView s path newDepth

-- | Dispatch folder handler to IO action. Returns none if handler not recognized.
def dispatch (s : ViewStack AdbcTable) (h : Cmd) : Option (IO (Option (ViewStack AdbcTable))) :=
  let opt f := some (f s)
  match h with
  | .folderPush     => opt push
  | .folderDepthInc => some (setDepth s 1)
  | .folderDepthDec => some (setDepth s (-1))
  | .folderDel      => if s.cur.vkind matches .fld _ _ then opt del else none
  | .folderParent   => if s.cur.vkind matches .fld _ _ then opt goParent else none
  | .folderEnter    => if s.cur.vkind matches .fld _ _ then opt enter else none
  | _ => none

end Tc.Folder
