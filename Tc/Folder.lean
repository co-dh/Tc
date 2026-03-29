/-
  Folder: directory browser with configurable find depth
  Commands: .fld .dup (push), .fld .inc/.dec (depth), .fld .ent (enter)
-/
import Tc.Fzf
import Tc.View
import Tc.Term
import Tc.SourceConfig
import Tc.Data.ADBC.Ops
import Tc.Remote

namespace Tc.Folder

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
  let lines := out.stdout.splitOn "\n" |>.filter (·.length > 0)
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

-- | View file with bat (if available) or less. .gz files piped through zcat.
def viewFile (path : String) : IO Unit := do
  let gz := path.endsWith ".gz"
  -- Escape single quotes for shell: ' → '\''
  let esc := path.replace "'" "'\\''"
  if ← Fzf.getTestMode then
    let r ← if gz
      then IO.Process.output { cmd := "sh", args := #["-c", s!"zcat '{esc}' | bat --paging=never --plain"] }
      else IO.Process.output { cmd := "bat", args := #["--paging=never", "--plain", path] }
    if r.exitCode == 0 then IO.print r.stdout
    else if gz then
      let r ← IO.Process.output { cmd := "zcat", args := #[path] }
      IO.print r.stdout
    else IO.print (← IO.FS.readFile path)
    return
  Term.shutdown
  let hasBat ← IO.Process.output { cmd := "which", args := #["bat"] }
  if gz then
    let viewer := if hasBat.exitCode == 0 then "bat --paging=always" else "less"
    let _ ← IO.Process.spawn { cmd := "sh", args := #["-c", s!"zcat '{esc}' | {viewer}"], stdin := .inherit, stdout := .inherit, stderr := .inherit } >>= (·.wait)
  else if hasBat.exitCode == 0 then
    let _ ← IO.Process.spawn { cmd := "bat", args := #["--paging=always", path], stdin := .inherit, stdout := .inherit, stderr := .inherit } >>= (·.wait)
  else
    let _ ← IO.Process.spawn { cmd := "less", args := #[path], stdin := .inherit, stdout := .inherit, stderr := .inherit } >>= (·.wait)
  let _ ← Term.init

-- | File format descriptor: how DuckDB should handle a file extension
structure FileFormat where
  exts       : Array String  -- file extensions (e.g. #[".csv", ".parquet"])
  reader     : String        -- DuckDB reader function. Empty = auto-detect.
  duckdbExt  : String        -- DuckDB extension to INSTALL/LOAD. Empty = none.
  attach     : Bool          -- true = ATTACH as database, list tables
  attachType : String        -- ATTACH TYPE clause (e.g. "SQLITE"). Empty = native.

-- | All file formats supported by DuckDB
def fileFormats : Array FileFormat := #[
  ⟨#[".csv", ".parquet", ".json", ".jsonl", ".ndjson"], "", "", false, ""⟩,
  ⟨#[".arrow", ".feather"], "read_arrow", "arrow", false, ""⟩,
  ⟨#[".xlsx", ".xls"], "read_xlsx", "excel", false, ""⟩,
  ⟨#[".avro"], "read_avro", "avro", false, ""⟩,
  ⟨#[".duckdb", ".db"], "", "", true, ""⟩,
  ⟨#[".sqlite", ".sqlite3"], "", "sqlite", true, "SQLITE"⟩
]

-- | Find format by file extension (handles .gz: strip suffix, match inner ext)
def findFormat (path : String) : Option FileFormat :=
  let p := if path.endsWith ".gz" then (path.take (path.length - 3)).toString else path
  fileFormats.find? fun fmt => fmt.exts.any (p.endsWith ·)

-- | Is file a data format we can open as a table?
def isDataFile (p : String) : Bool :=
  (findFormat p).isSome

-- | Is file a .txt (or .txt.gz)?
def isTxtFile (p : String) : Bool :=
  let p' := if p.endsWith ".gz" then (p.take (p.length - 3)).toString else p
  p'.endsWith ".txt"

-- | Try to ingest file as CSV via DuckDB read_csv (handles .gz natively).
-- Returns none on failure (not valid CSV) so caller can fall back to viewer.
def tryReadCsv (path : String) : IO (Option (View AdbcTable)) := do
  let absPath ← do
    let rp ← IO.Process.output { cmd := "realpath", args := #[path] }
    pure (if rp.exitCode == 0 then rp.stdout.trimAscii.toString else path)
  try
    match ← AdbcTable.fromFileWith absPath "read_csv" "" with
    | some tbl => pure (View.fromTbl tbl path)
    | none => pure none
  catch e => Log.write "tryReadCsv" s!"{path}: {e}"; pure none

-- | ATTACH a database file and list its tables as a folder view
private def attachFile (absPath : String) (fmt : FileFormat) : IO (Option (View AdbcTable)) := do
  if !fmt.duckdbExt.isEmpty then
    let _ ← Adbc.query s!"INSTALL {fmt.duckdbExt}; LOAD {fmt.duckdbExt}"
  let typClause := if fmt.attachType.isEmpty then "" else s!"TYPE {fmt.attachType}, "
  let _ ← Adbc.query s!"DETACH DATABASE IF EXISTS extdb"
  let _ ← Adbc.query s!"ATTACH '{escSql absPath}' AS extdb ({typClause}READ_ONLY)"
  let some qr ← Prql.query Prql.ducktabs | return none
  let total ← Adbc.nrows qr
  if total.toNat == 0 then return none
  let adbc ← AdbcTable.ofQueryResult qr
    { base := Prql.ducktabs }
    total.toNat
  let disp := absPath.splitOn "/" |>.getLast?.getD absPath
  match View.fromTbl adbc absPath (grp := #["name"]) with
  | some v => pure (some { v with vkind := .fld absPath 1, disp })
  | none => pure none

-- | Open any supported data file as a View
def openFile (path : String) : IO (Option (View AdbcTable)) := do
  let absPath ← do
    let rp ← IO.Process.output { cmd := "realpath", args := #[path] }
    pure (if rp.exitCode == 0 then rp.stdout.trimAscii.toString else path)
  match findFormat path with
  | some fmt =>
    if fmt.attach then attachFile absPath fmt
    else
      match ← AdbcTable.fromFileWith absPath fmt.reader fmt.duckdbExt with
      | some tbl => pure (View.fromTbl tbl path)
      | none => pure none
  | none =>
    match ← AdbcTable.fromFile absPath with
    | some tbl => pure (View.fromTbl tbl path)
    | none => pure none

-- | Get path column value from current row
def curPath (v : View AdbcTable) : IO (Option String) := do
  if !(v.vkind matches .fld _ _) then return none
  let names := TblOps.colNames v.nav.tbl
  let some pathCol := names.idxOf? "path" <|> names.idxOf? "name" <|> names.idxOf? "id" | return none
  let cols ← TblOps.getCols v.nav.tbl #[pathCol] v.nav.row.cur.val (v.nav.row.cur.val + 1)
  let c := cols.getD 0 default
  return some (c.get 0).toRaw

-- | Get type column value from current row
-- Normalizes: 'f'/"file" → 'f', 'd'/"dir" → 'd', ' ' (HF/S3 file) → 'f'
def curType (v : View AdbcTable) : IO (Option Char) := do
  if !(v.vkind matches .fld _ _) then return none
  let names := TblOps.colNames v.nav.tbl
  let some typeCol := names.idxOf? "type" | return some 'f'
  let cols ← TblOps.getCols v.nav.tbl #[typeCol] v.nav.row.cur.val (v.nav.row.cur.val + 1)
  let c := cols.getD 0 default
  let t := (c.get 0).toRaw
  match t.toList.head? with
  | some ' ' => return some 'f'  -- HF/S3 space = file
  | other => return other

-- | Build folder view from TSV content
private def mkViewFromTsv (tsv : String) (path : String) (depth : Nat) (disp : String)
    : IO (Option (View AdbcTable)) := do
  match ← AdbcTable.fromTsv tsv with
  | some adbc =>
    pure <| View.fromTbl (adbc) path |>.map fun v =>
      { v with vkind := .fld path depth, disp }
  | none => pure none

-- | Build folder view from an AdbcTable directly
private def mkViewFromAdbc (adbc : AdbcTable) (path : String) (depth : Nat) (disp : String)
    (grp : Array String := #[]) : Option (View AdbcTable) :=
  View.fromTbl (adbc) path (grp := grp) |>.map fun v =>
    { v with vkind := .fld path depth, disp }

-- | Create folder view — config-driven listing, local fallback
def mkView (path : String) (depth : Nat) : IO (Option (View AdbcTable)) := do
  match ← SourceConfig.findSource path with
  | some cfg =>
    match ← cfg.runList path with
    | some adbc =>
      let grp := if cfg.grp.isEmpty then #[] else #[cfg.grp]
      pure (mkViewFromAdbc adbc path depth (Remote.dispName path) (grp := grp))
    | none => pure none
  | none =>
    -- Local filesystem fallback
    let rp ← IO.Process.output { cmd := "realpath", args := #[path] }
    let absPath := if rp.exitCode == 0 then rp.stdout.trimAscii.toString else path
    let disp := absPath.splitOn "/" |>.getLast? |>.getD absPath
    mkViewFromTsv (← listDir path depth) absPath depth disp

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
  let curDir := match s.cur.vkind with | .fld dir _ => dir | _ => "."
  match ← SourceConfig.findSource curDir with
  | some c => match c.parent curDir with
    | some par => tryView s par 1 false
    | none => pure (some s)
  | none => match s.pop with
    | some s' => pure (some s')
    | none => tryView s (curDir ++ "/..") (curDepth s) false

-- | Enter directory or view file based on current row
def enter (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  let curDir := match s.cur.vkind with | .fld dir _ => dir | _ => "."
  let cfg ← SourceConfig.findSource curDir
  -- Attach-based database files: enter opens a table
  if let some fmt := findFormat curDir then
    if fmt.attach then
      let some tableName ← curPath s.cur | return some s
      let some (adbc, keys) ← AdbcTable.fromDuckDBTable tableName | return some s
      let some v := View.fromTbl (adbc) s!"{curDir}:{tableName}" (grp := keys) | return some s
      return some (s.push v)
  -- Config-driven attach (e.g. pg://)
  if let some c := cfg then
    if c.attach then
      let some tableName ← curPath s.cur | return some s
      let some (adbc, keys) ← AdbcTable.fromDuckDBTable tableName | return some s
      let some v := View.fromTbl (adbc) s!"{curDir}:{tableName}" (grp := keys) | return some s
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
      let fullPath := joinPath curDir p
      if isDataFile p then
        let openPath ← match cfg with | some c => c.resolve fullPath | none => pure fullPath
        match ← openFile openPath with
        | some v => pure (some (s.push v))
        | none => if cfg.isNone then viewFile fullPath; pure (some s) else pure (some s)
      else
        let viewPath ← match cfg with | some c => c.runDownload fullPath | none => pure fullPath
        -- Smart: try read_csv for unrecognized .gz before falling back to viewer
        if p.endsWith ".gz" then
          if let some v ← tryReadCsv viewPath then return some (s.push v)
        viewFile viewPath; pure (some s)
  | some 's', some p =>
    if cfg.isSome then pure (some s) else
    let fullPath := joinPath curDir p
    let stat ← IO.Process.output { cmd := "test", args := #["-d", fullPath] }
    if stat.exitCode == 0 then tryView s fullPath (curDepth s) true
    else if isDataFile fullPath then
      match ← openFile fullPath with
      | some v => pure (some (s.push v))
      | none => viewFile fullPath; pure (some s)
    else viewFile fullPath; pure (some s)
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
    let names := TblOps.colNames v.nav.tbl
    let some pathCol := names.idxOf? "path" <|> names.idxOf? "name" | return #[]
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
    let r ← Log.run "trash" cmd (baseArgs ++ #[p])
    if r.exitCode != 0 then ok := false
  pure ok

-- | Delete selected files and refresh view (no-op for remote)
def del (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  if !(s.cur.vkind matches .fld _ _) then return none
  let curDir := match s.cur.vkind with | .fld dir _ => dir | _ => ""
  if (← SourceConfig.findSource curDir).isSome then return some s
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
def setDepth (s : ViewStack AdbcTable) (delta : Int) : IO (Option (ViewStack AdbcTable)) := do
  match s.cur.vkind with
  | .fld path depth =>
    if (← SourceConfig.findSource path).isSome then return some s
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
def update (s : ViewStack AdbcTable) (cmd : Cmd) : Option (ViewStack AdbcTable × Effect) :=
  match cmd with
  | .fld .dup => some (s, .folder .push)
  | .fld .inc => some (s, .folder (.depth 1))
  | .fld .dec => some (s, .folder (.depth (-1)))
  | .fld .del =>
    if s.cur.vkind matches .fld _ _ then some (s, .folder .del) else none
  | .fld .lbc =>
    if s.cur.vkind matches .fld _ _ then some (s, .folder .parent) else none
  | .fld .ent =>
    if s.cur.vkind matches .fld _ _ then some (s, .folder .enter) else none
  | _ => none

end Tc.Folder
