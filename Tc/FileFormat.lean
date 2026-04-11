/-
  FileFormat: file format detection, opening, and viewing.
  Maps file extensions → DuckDB readers, handles ATTACH for database files.
-/
import Tc.Fzf
import Tc.View
import Tc.Term
import Tc.Data.ADBC.Ops

namespace Tc.FileFormat

-- | How DuckDB should handle a file extension
structure Format where
  exts       : Array String  -- file extensions (e.g. #[".csv", ".parquet"])
  reader     : String        -- DuckDB reader function. Empty = auto-detect.
  duckdbExt  : String        -- DuckDB extension to INSTALL/LOAD. Empty = none.
  attach     : Bool          -- true = ATTACH as database, list tables
  attachType : String        -- ATTACH TYPE clause (e.g. "SQLITE"). Empty = native.

-- | All file formats supported by DuckDB
def formats : Array Format := #[
  ⟨#[".csv", ".parquet", ".json", ".jsonl", ".ndjson"], "", "", false, ""⟩,
  ⟨#[".arrow", ".feather"], "read_arrow", "arrow", false, ""⟩,
  ⟨#[".xlsx", ".xls"], "read_xlsx", "excel", false, ""⟩,
  ⟨#[".avro"], "read_avro", "avro", false, ""⟩,
  ⟨#[".duckdb", ".db"], "", "", true, ""⟩,
  ⟨#[".sqlite", ".sqlite3"], "", "sqlite", true, "SQLITE"⟩
]

-- | Strip .gz suffix for extension matching
private def stripGz (p : String) : String :=
  if p.endsWith ".gz" then (p.take (p.length - 3)).toString else p

-- | Find format by file extension (handles .gz: strip suffix, match inner ext)
def find? (path : String) : Option Format :=
  let p := stripGz path; formats.find? fun fmt => fmt.exts.any (p.endsWith ·)
-- | Is file a recognized data format?
def isDataFile (p : String) : Bool := (find? p).isSome
-- | Is file a .txt (or .txt.gz)?
def isTxtFile (p : String) : Bool := (stripGz p).endsWith ".txt"

-- | Resolve absolute path via realpath
private def absPath (path : String) : IO String := do
  let rp ← IO.Process.output { cmd := "realpath", args := #[path] }
  pure (if rp.exitCode == 0 then rp.stdout.trimAscii.toString else path)

-- | Spawn interactive process (bat/less/zcat)
private def spawn (cmd : String) (args : Array String) : IO Unit := do
  let _ ← IO.Process.spawn { cmd, args, stdin := .inherit, stdout := .inherit, stderr := .inherit } >>= (·.wait)

-- | View file with bat (if available) or less. .gz files piped through zcat.
def viewFile (test : Bool) (path : String) : IO Unit := do
  let gz := path.endsWith ".gz"
  let esc := path.replace "'" "'\\''"
  if test then
    let r ← if gz
      then IO.Process.output { cmd := "sh", args := #["-c", s!"zcat '{esc}' | bat --paging=never --plain"] }
      else IO.Process.output { cmd := "bat", args := #["--paging=never", "--plain", path] }
    if r.exitCode == 0 then IO.print r.stdout
    else if gz then IO.print (← IO.Process.output { cmd := "zcat", args := #[path] }).stdout
    else IO.print (← IO.FS.readFile path)
    return
  Term.shutdown
  let hasBat := (← IO.Process.output { cmd := "which", args := #["bat"] }).exitCode == 0
  if gz then spawn "sh" #["-c", s!"zcat '{esc}' | {if hasBat then "bat --paging=always" else "less"}"]
  else if hasBat then spawn "bat" #["--paging=always", path]
  else spawn "less" #[path]
  let _ ← Term.init

-- | Try to ingest as CSV via DuckDB read_csv (handles .gz). None = not valid CSV.
def tryReadCsv (path : String) : IO (Option (View AdbcTable)) := do
  try pure ((← AdbcTable.fromFileWith (← absPath path) "read_csv" "") |>.bind (View.fromTbl · path))
  catch e => Log.write "tryReadCsv" s!"{path}: {e}"; pure none

-- | ATTACH database file and list its tables as a folder view
private def attachFile (ap : String) (fmt : Format) : IO (Option (View AdbcTable)) := do
  loadDuckExt fmt.duckdbExt
  let typClause := if fmt.attachType.isEmpty then "" else s!"TYPE {fmt.attachType}, "
  let _ ← Adbc.query s!"DETACH DATABASE IF EXISTS extdb"
  let _ ← Adbc.query s!"ATTACH '{escSql ap}' AS extdb ({typClause}READ_ONLY)"
  let some qr ← Prql.query Prql.ducktabs | return none
  let total ← Adbc.nrows qr
  if total.toNat == 0 then return none
  let adbc ← AdbcTable.ofQueryResult qr { base := Prql.ducktabs } total.toNat
  let disp := ap.splitOn "/" |>.getLast?.getD ap
  pure (View.fromTbl adbc ap (grp := #["name"]) |>.map fun v => { v with vkind := .fld ap 1, disp })

-- | Open any supported data file as a View (attach for DB, reader for data files)
def openFile (path : String) : IO (Option (View AdbcTable)) := do
  let ap ← absPath path
  match find? path with
  | some fmt =>
    if fmt.attach then attachFile ap fmt
    else pure ((← AdbcTable.fromFileWith ap fmt.reader fmt.duckdbExt) |>.bind (View.fromTbl · path))
  | none => pure ((← AdbcTable.fromFile ap) |>.bind (View.fromTbl · path))

end Tc.FileFormat
