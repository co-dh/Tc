/-
  Osquery: browse osquery tables via osqueryi CLI + DuckDB.
  Table listing populated by scripts/osquery_tables.py into ~/.cache/tc/osquery.duckdb.
  This module ATTACHes that DB and reads from it.
-/
import Tc.Data.ADBC.Table
import Tc.Error
import Tc.Render
import Tc.TmpDir

namespace Tc.Osquery

def isOsquery (path : String) : Bool := path.startsWith "osquery://"

-- | Check if a table is dangerous (requires specific input) via osq.listing
private def isDangerous (table : String) : IO Bool := do
  try
    let qr ← Adbc.query s!"SELECT safety FROM osq.listing WHERE name = '{escSql table}' LIMIT 1"
    pure ((← Adbc.cellStr qr 0 0) == "input-required")
  catch _ => pure false

-- | Run osqueryi with --json flag, return stdout
private def osqueryi (sql : String) : IO String := do
  let r ← Log.run "osquery" "osqueryi" #["--json", sql]
  if r.exitCode != 0 then
    throw <| IO.userError s!"osqueryi failed: {r.stderr.trimAscii.toString}"
  pure r.stdout

-- | Extract a JSON field value from a JSON object fragment
private def jsonField (entry key : String) : Option String :=
  (entry.splitOn s!"\"{key}\":\"" |>.getD 1 "" |>.splitOn "\"" |>.head?).filter (!·.isEmpty)

-- | Map osquery type to DuckDB type for TRY_CAST
private def duckdbType (osqType : String) : Option String :=
  match osqType.toLower with
  | "bigint" | "integer" | "int" => some "BIGINT"
  | "double" => some "DOUBLE"
  | _ => none

-- | Get column schema from pragma table_info
private def tableColumns (table : String) : IO (Array (String × String)) := do
  let json ← osqueryi s!"pragma table_info('{table}')"
  if json.trimAscii.toString == "[]" || json.isEmpty then return #[]
  pure (json.splitOn "{" |>.drop 1 |>.filterMap fun entry =>
    match jsonField entry "name", jsonField entry "type" with
    | some n, some t => some (n, t)
    | _, _ => none).toArray

-- | Build typed SELECT with TRY_CAST based on pragma schema
private def typedSelect (cols : Array (String × String)) : String :=
  if cols.isEmpty then "*"
  else ", ".intercalate (cols.map fun (name, typ) =>
    match duckdbType typ with
    | some dt => s!"TRY_CAST(\"{name}\" AS {dt}) AS \"{name}\""
    | none => s!"\"{name}\"").toList

-- | Write JSON to tmpfile, load into DuckDB temp table via read_json_auto.
--   `sel` is the SELECT clause, `suffix` appended after the FROM (e.g. JOINs).
private def jsonToTable (json : String) (sel := "*") (suffix := "") : IO (Option AdbcTable) := do
  if json.trimAscii.toString == "[]" || json.isEmpty then return none
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tmp ← Tc.tmpPath s!"osq-{n}.json"
  IO.FS.writeFile tmp json
  let tbl := s!"tc_osq_{n}"
  try
    let _ ← Adbc.query s!"CREATE TEMP TABLE {tbl} AS SELECT {sel} FROM read_json_auto('{tmp}') j{suffix}"
    try IO.FS.removeFile tmp catch _ => pure ()
    let q : Prql.Query := { base := s!"from {tbl}" }
    AdbcTable.requery q (← AdbcTable.queryCount q)
  catch e =>
    try IO.FS.removeFile tmp catch _ => pure ()
    Log.write "osquery" s!"error: {e}"
    errorPopup e.toString
    return none

/-! ## Schema DB: ATTACHed from persistent DuckDB populated by Python -/

-- | Schema state: none=not tried, some false=tried+failed, some true=available
initialize schemaState : IO.Ref (Option Bool) ← IO.mkRef none

private def schemaAvail : IO Bool := (·.getD false) <$> schemaState.get

-- | Attach the schema DB (created by scripts/osquery_tables.py). Runs once per session.
def ensureSchema : IO Unit := do
  if (← schemaState.get).isSome then return
  schemaState.set (some false)
  let homeDir := (← IO.getEnv "HOME").getD "/tmp"
  let dbPath := s!"{homeDir}/.cache/tc/osquery.duckdb"
  try
    let _ ← Adbc.query s!"ATTACH '{dbPath}' AS osq"
  catch _ =>
    try IO.FS.removeFile dbPath catch _ => pure ()
    try let _ ← Adbc.query s!"ATTACH '{dbPath}' AS osq"
    catch e => Log.write "osquery" s!"ATTACH failed: {e}"; return
  let populated : Bool ← try
    let qr ← Adbc.query "SELECT count(*) FROM osq.listing"
    pure (decide ((← Adbc.cellInt qr 0 0).toNat > 0))
  catch _ => pure false
  if populated then
    schemaState.set (some true)
    Log.write "osquery" s!"Schema DB attached ({dbPath})"
  else
    Log.write "osquery" s!"Schema DB empty or missing ({dbPath})"

/-! ## Public API -/

-- | Run the Python script to populate the osquery DB
private def runPythonSetup : IO Bool := do
  let exe ← IO.appPath
  let exeDir := exe.parent.getD "."
  let candidates := #[
    s!"{exeDir}/scripts/osquery_tables.py",
    s!"{exeDir}/../scripts/osquery_tables.py",
    s!"{exeDir}/../../scripts/osquery_tables.py"
  ]
  let mut script := "scripts/osquery_tables.py"
  for c in candidates do
    if ← (c : System.FilePath).pathExists then script := c; break
  let r ← IO.Process.output { cmd := "python3", args := #[script] }
  if r.exitCode != 0 then
    Log.write "osquery" s!"Python script failed: {r.stderr.trimAscii.toString}"
    errorPopup s!"osquery listing failed: {r.stderr.trimAscii.toString}"
    return false
  return true

-- | List all osquery tables from the pre-populated DB
def list (_path : String) : IO (Option (AdbcTable × String)) := do
  statusMsg "Loading osquery tables ..."
  if !(← runPythonSetup) then return none
  ensureSchema
  if !(← schemaAvail) then return none
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tbl := s!"tc_osq_{n}"
  try
    let _ ← Adbc.query s!"CREATE TEMP TABLE {tbl} AS SELECT name, safety, rows, description FROM osq.listing"
    let q : Prql.Query := { base := s!"from {tbl}" }
    let total ← AdbcTable.queryCount q
    (·.map (·, "osquery://")) <$> AdbcTable.requery q total
  catch e =>
    Log.write "osquery" s!"list error: {e}"
    errorPopup e.toString
    return none

-- | osquery parent: only root level, no hierarchy
def parent (_path : String) : Option String := none

-- | Enter a table: safe → query it, dangerous → show schema
def enterTable (table : String) : IO (Option (AdbcTable × String)) := do
  if !(← schemaAvail) then let _ ← runPythonSetup; ensureSchema
  if ← isDangerous table then
    statusMsg s!"Loading schema for {table} ..."
    let json ← osqueryi s!"pragma table_info('{table}')"
    if json.trimAscii.toString == "[]" || json.isEmpty then return none
    -- isDangerous succeeded → schema DB is available, so JOIN with duckdb_columns()
    let join := s!" LEFT JOIN duckdb_columns() c ON c.schema_name = 'osq' AND c.table_name = '{escSql table}' AND c.column_name = j.name"
    let some tbl ← jsonToTable json (sel := "j.*, COALESCE(c.comment, '') as description") (suffix := join) | return none
    pure <| some (tbl, s!"schema:{table}")
  else
    statusMsg s!"Querying {table} ..."
    let cols ← tableColumns table
    let sel := typedSelect cols
    pure <| (← jsonToTable (← osqueryi s!"SELECT * FROM {table}") (sel := sel)).map (·, table)

-- | Enrich a meta temp table with column descriptions from the schema DB
def enrichMeta (metaTblName tableName : String) : IO Unit := do
  if !(← schemaAvail) then return
  try
    let _ ← Adbc.query s!"ALTER TABLE {metaTblName} ADD COLUMN description VARCHAR DEFAULT ''"
    let _ ← Adbc.query s!"UPDATE {metaTblName} SET description = COALESCE((SELECT comment FROM duckdb_columns() WHERE schema_name = 'osq' AND table_name = '{escSql tableName}' AND column_name = {metaTblName}.\"column\"), '')"
  catch e => Log.write "enrichMeta" s!"error: {e}"

-- | Look up column description for a given osquery table and column name
def colDesc (tableName colName : String) : IO String := do
  if !(← schemaAvail) then return ""
  try
    Adbc.cellStr (← Adbc.query s!"SELECT comment FROM duckdb_columns() WHERE schema_name = 'osq' AND table_name = '{escSql tableName}' AND column_name = '{escSql colName}' LIMIT 1") 0 0
  catch _ => pure ""

end Tc.Osquery
