/-
  Osquery: browse osquery tables via osqueryi CLI + DuckDB.
  Table listing populated by scripts/osquery_tables.py into ~/.cache/tc/osquery.duckdb.
  This module ATTACHes that DB and reads from it.
-/
import Tc.Data.ADBC.Table
import Tc.Error
import Tc.Render
import Tc.SourceConfig
import Tc.TmpDir

namespace Tc.Osquery

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

-- | Check if osq schema is already attached and populated (e.g. by SourceConfig.runSetup).
--   If so, mark schemaState as available.
def ensureSchema : IO Unit := do
  if (← schemaState.get).isSome then return
  schemaState.set (some false)
  -- Check if already attached by config setup_sql
  let populated : Bool ← try
    let qr ← Adbc.query "SELECT count(*) FROM osq.listing"
    pure (decide ((← Adbc.cellInt qr 0 0).toNat > 0))
  catch _ => pure false
  if populated then
    schemaState.set (some true)
    Log.write "osquery" "Schema DB available (attached by config)"
  else
    Log.write "osquery" "Schema DB not available"

/-! ## Public API -/

-- | Enter a table: safe → query it, dangerous → show schema
def enterTable (table : String) : IO (Option (AdbcTable × String)) := do
  ensureSchema
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

-- | SourceConfig handler for osquery:// (enter, status, enrichMeta only; listing is config-driven)
def handler : SourceConfig.Handler where
  enter := fun _curDir table => enterTable table
  statusInfo := fun path colName => do
    let raw := (path.drop 10).toString  -- drop "osquery://"
    let tableName := if raw.startsWith "schema:" then (raw.drop 7).toString else raw
    if tableName.isEmpty then pure "" else colDesc tableName colName
  enrichMeta := enrichMeta

end Tc.Osquery
