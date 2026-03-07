/-
  Osquery: browse osquery tables via osqueryi CLI + DuckDB read_json_auto
  Schema descriptions fetched from osquery-site GitHub, cached in persistent DuckDB.
-/
import Tc.Data.ADBC.Table
import Tc.Error
import Tc.Render
import Tc.TmpDir

namespace Tc.Osquery

-- | Check if path is an osquery URI
def isOsquery (path : String) : Bool := path.startsWith "osquery://"

-- | Tables that require specific input columns and should not be auto-queried.
private def dangerousTables : Array String := #[
  "hash", "file", "augeas", "yara", "curl", "curl_certificate",
  "magic", "device_file", "carves", "suid_bin",
  "file_events", "process_events", "process_file_events", "socket_events",
  "hardware_events", "selinux_events", "seccomp_events", "syslog_events",
  "user_events", "apparmor_events"
]

-- | SQL IN list from dangerous tables
private def dangerousIn : String :=
  let quoted := dangerousTables.map fun t => s!"'{t}'"
  "(" ++ ", ".intercalate quoted.toList ++ ")"

-- | Run osqueryi with --json flag, return stdout
private def osqueryi (sql : String) : IO String := do
  let r ← Log.run "osquery" "osqueryi" #["--json", sql]
  if r.exitCode != 0 then
    throw <| IO.userError s!"osqueryi failed: {r.stderr.trimAscii.toString}"
  pure r.stdout

-- | Run osqueryi, write JSON to tmp, load into DuckDB temp table, return AdbcTable
private def osqueryToTable (sql : String) (extraSql : String := "") : IO (Option AdbcTable) := do
  let json ← osqueryi sql
  if json.trimAscii.toString == "[]" || json.isEmpty then return none
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tmpFile ← Tc.tmpPath s!"osq-{n}.json"
  IO.FS.writeFile tmpFile json
  let tblName := s!"tc_osq_{n}"
  try
    let selectPart := if extraSql.isEmpty then "*" else s!"*, {extraSql}"
    let _ ← Adbc.query s!"CREATE TEMP TABLE {tblName} AS SELECT {selectPart} FROM read_json_auto('{tmpFile}')"
    try IO.FS.removeFile tmpFile catch _ => pure ()
    let query : Prql.Query := { base := s!"from {tblName}" }
    let total ← AdbcTable.queryCount query
    AdbcTable.requery query total
  catch e =>
    try IO.FS.removeFile tmpFile catch _ => pure ()
    Log.write "osqueryToTable" s!"error: {e.toString}"
    return none

/-! ## Schema: persistent DuckDB with table/column descriptions from osquery.io -/

-- | Schema URL template (version detected from osqueryi)
private def schemaUrlBase : String :=
  "https://raw.githubusercontent.com/osquery/osquery-site/source/src/data/osquery_schema_versions/"

-- | Detect osqueryi version
private def osqueryVersion : IO String := do
  let r ← IO.Process.output { cmd := "osqueryi", args := #["--version"] }
  let parts := r.stdout.trimAscii.toString.splitOn " "
  pure (parts.getLast?.getD "5.21.0")

-- | Whether schema has been loaded this session
initialize schemaReady : IO.Ref Bool ← IO.mkRef false

-- | Whether schema is available (tables exist in attached DB)
initialize schemaAvail : IO.Ref Bool ← IO.mkRef false

-- | Ensure schema DB is attached and populated. Runs once per session.
-- Downloads JSON from GitHub, stores in ~/.cache/tc/osquery.duckdb.
def ensureSchema : IO Unit := do
  if ← schemaReady.get then return
  schemaReady.set true
  let home ← IO.Process.output { cmd := "sh", args := #["-c", "echo $HOME"] }
  let homeDir := home.stdout.trimAscii.toString
  let cacheDir := s!"{homeDir}/.cache/tc"
  let _ ← IO.Process.output { cmd := "mkdir", args := #["-p", cacheDir] }
  let ver ← osqueryVersion
  let dbPath := s!"{cacheDir}/osquery.duckdb"
  let schemaUrl := s!"{schemaUrlBase}{ver}.json"
  -- ATTACH persistent DB
  try
    let _ ← Adbc.query s!"ATTACH '{dbPath}' AS osq"
  catch _ =>
    -- corrupt file: remove and retry
    let _ ← IO.Process.output { cmd := "rm", args := #["-f", dbPath] }
    try let _ ← Adbc.query s!"ATTACH '{dbPath}' AS osq"
    catch e => Log.write "osquery" s!"ATTACH failed: {e}"; return
  -- Check if tables already populated
  let populated : Bool ← try
    let qr ← Adbc.query "SELECT count(*) FROM osq.tables"
    let n ← Adbc.cellInt qr 0 0
    pure (decide (n.toNat > 0))
  catch _ => pure false
  if populated then
    schemaAvail.set true
    Log.write "osquery" s!"Schema DB attached ({dbPath})"
    return
  -- Download schema JSON
  statusMsg "Downloading osquery schema ..."
  let tmpFile ← Tc.tmpPath "osquery_schema.json"
  let dl ← IO.Process.output { cmd := "curl", args := #["-sf", "-o", tmpFile, schemaUrl] }
  if dl.exitCode != 0 then
    Log.write "osquery" s!"Schema download failed for {schemaUrl}"
    return
  -- Populate tables
  try
    let _ ← Adbc.query "DROP TABLE IF EXISTS osq.tables"
    let _ ← Adbc.query "DROP TABLE IF EXISTS osq.columns"
    let _ ← Adbc.query s!"CREATE TABLE osq.tables AS SELECT name, description, array_to_string(platforms, ', ') as platforms FROM read_json_auto('{tmpFile}')"
    let _ ← Adbc.query s!"CREATE TABLE osq.columns AS SELECT t.name as table_name, u.unnest.name as col_name, u.unnest.type as col_type, u.unnest.description as col_desc FROM read_json_auto('{tmpFile}') t, LATERAL UNNEST(t.columns) u"
    schemaAvail.set true
    let qr ← Adbc.query "SELECT count(*) FROM osq.tables"
    let cnt ← Adbc.cellInt qr 0 0
    Log.write "osquery" s!"Schema loaded: {cnt} tables from {ver}"
  catch e =>
    Log.write "osquery" s!"Schema build failed: {e}"
  try IO.FS.removeFile tmpFile catch _ => pure ()

/-! ## Public API -/

-- | List all osquery tables with metadata and description. Returns AdbcTable for folder view.
def list (_path : String) : IO (Option AdbcTable) := do
  statusMsg "Loading osquery tables ..."
  ensureSchema
  let json ← osqueryi
    "SELECT name FROM osquery_registry WHERE registry='table' ORDER BY name"
  if json.trimAscii.toString == "[]" || json.isEmpty then return none
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tmpFile ← Tc.tmpPath s!"osq-{n}.json"
  IO.FS.writeFile tmpFile json
  let tblName := s!"tc_osq_{n}"
  let hasSchema ← schemaAvail.get
  try
    let descJoin := if hasSchema
      then ", COALESCE(s.description, '') as description FROM read_json_auto('" ++
           tmpFile ++ "') j LEFT JOIN osq.tables s ON s.name = j.name"
      else " FROM read_json_auto('" ++ tmpFile ++ "') j"
    let _ ← Adbc.query
      (s!"CREATE TEMP TABLE {tblName} AS SELECT j.*, " ++
       s!"CASE WHEN j.name IN {dangerousIn} THEN 'input-required' ELSE 'safe' END as safety" ++
       descJoin)
    try IO.FS.removeFile tmpFile catch _ => pure ()
    let query : Prql.Query := { base := s!"from {tblName}" }
    let total ← AdbcTable.queryCount query
    AdbcTable.requery query total
  catch e =>
    try IO.FS.removeFile tmpFile catch _ => pure ()
    Log.write "osqueryList" s!"error: {e.toString}"
    return none

-- | osquery parent: only root level, no hierarchy
def parent (_path : String) : Option String := none

-- | Enter a table: safe → query it, dangerous → show schema with column descriptions.
-- Returns AdbcTable and display label.
def enterTable (table : String) : IO (Option (AdbcTable × String)) := do
  if dangerousTables.contains table then
    statusMsg s!"Loading schema for {table} ..."
    ensureSchema
    let hasSchema ← schemaAvail.get
    let json ← osqueryi s!"pragma table_info('{table}')"
    if json.trimAscii.toString == "[]" || json.isEmpty then return none
    let n ← memTblCounter.modifyGet fun n => (n, n + 1)
    let tmpFile ← Tc.tmpPath s!"osq-{n}.json"
    IO.FS.writeFile tmpFile json
    let tblName := s!"tc_osq_{n}"
    try
      let esc := table.replace "'" "''"
      let descJoin := if hasSchema
        then s!" LEFT JOIN osq.columns c ON c.table_name = '{esc}' AND c.col_name = j.name"
        else ""
      let descCol := if hasSchema then ", COALESCE(c.col_desc, '') as description" else ""
      let _ ← Adbc.query s!"CREATE TEMP TABLE {tblName} AS SELECT j.*{descCol} FROM read_json_auto('{tmpFile}') j{descJoin}"
      try IO.FS.removeFile tmpFile catch _ => pure ()
      let query : Prql.Query := { base := s!"from {tblName}" }
      let total ← AdbcTable.queryCount query
      let tbl ← AdbcTable.requery query total
      pure <| tbl.map (·, s!"schema:{table}")
    catch e =>
      try IO.FS.removeFile tmpFile catch _ => pure ()
      Log.write "enterTable" s!"error: {e.toString}"
      return none
  else
    statusMsg s!"Querying {table} ..."
    let tbl ← osqueryToTable s!"SELECT * FROM {table}"
    pure <| tbl.map (·, table)

-- | Enrich a meta temp table with column descriptions from the schema DB.
-- Called after queryMeta creates tc_meta_N. Adds 'description' column via osq.columns.
def enrichMeta (metaTblName tableName : String) : IO Unit := do
  if !(← schemaAvail.get) then return
  let esc := tableName.replace "'" "''"
  try
    let _ ← Adbc.query s!"ALTER TABLE {metaTblName} ADD COLUMN description VARCHAR DEFAULT ''"
    let _ ← Adbc.query s!"UPDATE {metaTblName} SET description = COALESCE((SELECT col_desc FROM osq.columns WHERE table_name = '{esc}' AND col_name = {metaTblName}.\"column\"), '')"
  catch e =>
    Log.write "enrichMeta" s!"error: {e}"

end Tc.Osquery
