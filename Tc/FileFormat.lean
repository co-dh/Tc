/-
  FileFormat: file format detection, opening, and viewing.
  Maps file extensions → DuckDB readers, handles ATTACH for database files.
-/
import Tc.Fzf
import Tc.View
import Tc.Term
import Tc.Data.ADBC.Ops

namespace Tc.FileFormat

-- | File format descriptor: how DuckDB should handle a file extension
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

-- | Find format by file extension (handles .gz: strip suffix, match inner ext)
def find? (path : String) : Option Format :=
  let p := if path.endsWith ".gz" then (path.take (path.length - 3)).toString else path
  formats.find? fun fmt => fmt.exts.any (p.endsWith ·)

-- | Is file a data format we can open as a table?
def isDataFile (p : String) : Bool := (find? p).isSome

-- | Is file a .txt (or .txt.gz)?
def isTxtFile (p : String) : Bool :=
  let p' := if p.endsWith ".gz" then (p.take (p.length - 3)).toString else p
  p'.endsWith ".txt"

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
private def attachFile (absPath : String) (fmt : Format) : IO (Option (View AdbcTable)) := do
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
  match find? path with
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

end Tc.FileFormat
