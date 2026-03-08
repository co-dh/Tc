/-
  Osquery: browse osquery tables via osqueryi CLI + DuckDB read_json_auto
  Schema descriptions fetched from osquery-site GitHub, cached in persistent DuckDB.
-/
import Std.Data.HashMap
import Tc.Data.ADBC.Table
import Tc.Error
import Tc.Render
import Tc.TmpDir

namespace Tc.Osquery

def isOsquery (path : String) : Bool := path.startsWith "osquery://"

-- | Tables that require specific input and should not be auto-queried
private def dangerousTables : Array String := #[
  "hash", "file", "augeas", "yara", "curl", "curl_certificate",
  "magic", "device_file", "carves", "suid_bin",
  "file_events", "process_events", "process_file_events", "socket_events",
  "hardware_events", "selinux_events", "seccomp_events", "syslog_events",
  "user_events", "apparmor_events"
]

private def dangerousIn : String :=
  "(" ++ ", ".intercalate (dangerousTables.map (s!"'{·}'") |>.toList) ++ ")"

-- | Run osqueryi with --json flag, return stdout
private def osqueryi (sql : String) : IO String := do
  let r ← Log.run "osquery" "osqueryi" #["--json", sql]
  if r.exitCode != 0 then
    throw <| IO.userError s!"osqueryi failed: {r.stderr.trimAscii.toString}"
  pure r.stdout

-- | Extract a JSON field value from a JSON object fragment
private def jsonField (entry key : String) : Option String :=
  (entry.splitOn s!"\"{key}\":\"" |>.getD 1 "" |>.splitOn "\"" |>.head?).filter (!·.isEmpty)

-- | Parse table names from osquery JSON: [{"name":"foo"},...]
private def parseNames (json : String) : Array String :=
  (json.splitOn "{" |>.drop 1 |>.filterMap (jsonField · "name")).toArray

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

-- | Write JSON to tmpfile, load into DuckDB temp table, return (tblName, tmpFile cleaned up)
private def jsonToTable (json : String) (select : String := "*") : IO (Option AdbcTable) := do
  if json.trimAscii.toString == "[]" || json.isEmpty then return none
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tmp ← Tc.tmpPath s!"osq-{n}.json"
  IO.FS.writeFile tmp json
  let tbl := s!"tc_osq_{n}"
  try
    let _ ← Adbc.query s!"CREATE TEMP TABLE {tbl} AS SELECT {select} FROM read_json_auto('{tmp}') j"
    try IO.FS.removeFile tmp catch _ => pure ()
    let q : Prql.Query := { base := s!"from {tbl}" }
    AdbcTable.requery q (← AdbcTable.queryCount q)
  catch e =>
    try IO.FS.removeFile tmp catch _ => pure ()
    Log.write "osquery" s!"error: {e}"
    errorPopup e.toString
    return none

-- | Like jsonToTable but takes a full SQL query with __TMP__ placeholder for tmpfile path
private def jsonToTableSql (json : String) (sql : String) : IO (Option AdbcTable) := do
  if json.trimAscii.toString == "[]" || json.isEmpty then return none
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tmp ← Tc.tmpPath s!"osq-{n}.json"
  IO.FS.writeFile tmp json
  let tbl := s!"tc_osq_{n}"
  try
    let _ ← Adbc.query s!"CREATE TEMP TABLE {tbl} AS {sql.replace "__TMP__" tmp}"
    try IO.FS.removeFile tmp catch _ => pure ()
    let q : Prql.Query := { base := s!"from {tbl}" }
    AdbcTable.requery q (← AdbcTable.queryCount q)
  catch e =>
    try IO.FS.removeFile tmp catch _ => pure ()
    Log.write "osquery" s!"error: {e}"
    errorPopup e.toString
    return none

/-! ## Schema: persistent DuckDB with table/column descriptions from osquery.io -/

private def schemaUrlBase : String :=
  "https://raw.githubusercontent.com/osquery/osquery-site/source/src/data/osquery_schema_versions/"

private def osqueryVersion : IO String := do
  let r ← IO.Process.output { cmd := "osqueryi", args := #["--version"] }
  pure (r.stdout.trimAscii.toString.splitOn " " |>.getLast?.getD "5.21.0")

-- | Schema state: none=not tried, some false=tried+failed, some true=available
initialize schemaState : IO.Ref (Option Bool) ← IO.mkRef none

private def schemaAvail : IO Bool := (·.getD false) <$> schemaState.get

-- | Ensure schema DB is attached and populated. Runs once per session.
def ensureSchema : IO Unit := do
  if (← schemaState.get).isSome then return
  schemaState.set (some false)
  let homeDir := (← IO.getEnv "HOME").getD "/tmp"
  let cacheDir := s!"{homeDir}/.cache/tc"
  try IO.FS.createDirAll cacheDir catch _ => pure ()
  let ver ← osqueryVersion
  let dbPath := s!"{cacheDir}/osquery.duckdb"
  -- ATTACH persistent DB
  try
    let _ ← Adbc.query s!"ATTACH '{dbPath}' AS osq"
  catch _ =>
    try IO.FS.removeFile dbPath catch _ => pure ()
    try let _ ← Adbc.query s!"ATTACH '{dbPath}' AS osq"
    catch e => Log.write "osquery" s!"ATTACH failed: {e}"; return
  -- Check if tables already populated
  let populated : Bool ← try
    let qr ← Adbc.query "SELECT count(*) FROM osq.tables"
    pure (decide ((← Adbc.cellInt qr 0 0).toNat > 0))
  catch _ => pure false
  if populated then
    schemaState.set (some true)
    Log.write "osquery" s!"Schema DB attached ({dbPath})"
    return
  -- Download schema JSON
  statusMsg "Downloading osquery schema ..."
  let tmpFile ← Tc.tmpPath "osquery_schema.json"
  let dl ← IO.Process.output { cmd := "curl", args := #["-sf", "-o", tmpFile, s!"{schemaUrlBase}{ver}.json"] }
  if dl.exitCode != 0 then
    Log.write "osquery" s!"Schema download failed for {ver}"
    return
  try
    let _ ← Adbc.query "DROP TABLE IF EXISTS osq.tables"
    let _ ← Adbc.query "DROP TABLE IF EXISTS osq.columns"
    let _ ← Adbc.query s!"CREATE TABLE osq.tables AS SELECT name, description, array_to_string(platforms, ', ') as platforms FROM read_json_auto('{tmpFile}')"
    let _ ← Adbc.query s!"CREATE TABLE osq.columns AS SELECT t.name as table_name, u.unnest.name as col_name, u.unnest.type as col_type, u.unnest.description as col_desc FROM read_json_auto('{tmpFile}') t, LATERAL UNNEST(t.columns) u"
    schemaState.set (some true)
    let cnt ← Adbc.cellInt (← Adbc.query "SELECT count(*) FROM osq.tables") 0 0
    Log.write "osquery" s!"Schema loaded: {cnt} tables from {ver}"
  catch e =>
    Log.write "osquery" s!"Schema build failed: {e}"
  try IO.FS.removeFile tmpFile catch _ => pure ()

-- | Count rows for a single table with timeout
private def countTable (name : String) : IO (Option (String × Nat)) := do
  let r ← IO.Process.output { cmd := "timeout", args := #["-k", "1", "2", "osqueryi", "--json",
    s!"SELECT count(*) as n FROM {name}"] }
  if r.exitCode != 0 then return none
  let nStr := (jsonField (r.stdout) "n").getD ""
  pure <| nStr.toNat?.map (name, ·)

-- | Count rows for all safe tables concurrently
private def countAllTables (names : Array String) : IO (Array (String × Nat)) := do
  let tasks ← (names.filter (!dangerousTables.contains ·)).mapM fun name =>
    IO.asTask (prio := .dedicated) (countTable name)
  let mut results := #[]
  for task in tasks do
    match ← IO.wait task with
    | .ok (some r) => results := results.push r
    | _ => pure ()
  pure results

-- | Load cached row counts from osq.row_counts
private def loadCachedCounts : IO (Array (String × Nat) × String) := do
  if !(← schemaAvail) then return (#[], "")
  try
    let ts ← Adbc.cellStr (← Adbc.query "SELECT max(updated_at) as ts FROM osq.row_counts") 0 0
    let fresh ← Adbc.cellInt (← Adbc.query "SELECT CASE WHEN max(updated_at)::TIMESTAMP > (CURRENT_TIMESTAMP::TIMESTAMP - INTERVAL '24 hours') THEN 1 ELSE 0 END FROM osq.row_counts") 0 0
    if fresh.toNat == 0 then return (#[], "")
    let allQr ← Adbc.query "SELECT name, rows FROM osq.row_counts"
    let cnt ← Adbc.cellInt (← Adbc.query "SELECT count(*) FROM osq.row_counts") 0 0
    let mut results := #[]
    for i in [:cnt.toNat] do
      results := results.push (← Adbc.cellStr allQr i.toUInt64 0, (← Adbc.cellInt allQr i.toUInt64 1).toNat)
    pure (results, ts)
  catch _ => pure (#[], "")

-- | Save row counts to persistent osq.row_counts
private def saveCounts (counts : Array (String × Nat)) : IO Unit := do
  if !(← schemaAvail) then return
  try
    let _ ← Adbc.query "DROP TABLE IF EXISTS osq.row_counts"
    let _ ← Adbc.query "CREATE TABLE osq.row_counts (name VARCHAR, rows INTEGER, updated_at VARCHAR)"
    if !counts.isEmpty then
      let ts ← Log.localTimestamp
      let values := counts.map fun (name, cnt) =>
        s!"('{escSql name}', {cnt}, '{ts}')"
      let _ ← Adbc.query s!"INSERT INTO osq.row_counts (name, rows, updated_at) VALUES {", ".intercalate values.toList}"
    Log.write "osquery" s!"Cached {counts.size} row counts"
  catch e => Log.write "osquery" s!"saveCounts error: {e}"

private def cacheLabel (ts : String) : String :=
  let hm := (ts.splitOn " " |>.getD 1 "").take 5 |>.toString
  if hm.isEmpty then "osquery://" else s!"osquery:// (cached {hm})"

/-! ## Public API -/

-- | List all osquery tables with metadata and description
def list (_path : String) : IO (Option (AdbcTable × String)) := do
  statusMsg "Loading osquery tables ..."
  ensureSchema
  let json ← osqueryi "SELECT name FROM osquery_registry WHERE registry='table' ORDER BY name"
  if json.trimAscii.toString == "[]" || json.isEmpty then return none
  let names := parseNames json
  let (counts, label) ← do
    let (cached, ts) ← loadCachedCounts
    if !cached.isEmpty then pure (cached, cacheLabel ts)
    else statusMsg "Counting rows ..."; let live ← countAllTables names; saveCounts live; pure (live, "osquery://")
  let hasSchema ← schemaAvail
  let some tbl ← jsonToTable json
    (s!"j.*, CASE WHEN j.name IN {dangerousIn} THEN 'input-required' ELSE 'safe' END as safety") | return none
  -- Add rows and description as extra columns
  let tblName := tbl.query.base.drop 5  -- "from tc_osq_N" → "tc_osq_N"
  let _ ← Adbc.query s!"ALTER TABLE {tblName} ADD COLUMN rows INTEGER DEFAULT NULL"
  if !counts.isEmpty then
    let cases := counts.map fun (name, cnt) => s!"WHEN '{escSql name}' THEN {cnt}"
    let nameList := counts.map fun (name, _) => s!"'{escSql name}'"
    let _ ← Adbc.query s!"UPDATE {tblName} SET rows = CASE name {" ".intercalate cases.toList} END WHERE name IN ({", ".intercalate nameList.toList})"
  if hasSchema then
    let _ ← Adbc.query s!"ALTER TABLE {tblName} ADD COLUMN description VARCHAR DEFAULT ''"
    let _ ← Adbc.query s!"UPDATE {tblName} SET description = COALESCE((SELECT s.description FROM osq.tables s WHERE s.name = {tblName}.name), '')"
  let q : Prql.Query := { base := s!"from {tblName}" }
  let total ← AdbcTable.queryCount q
  (·.map (·, label)) <$> AdbcTable.requery q total

-- | osquery parent: only root level, no hierarchy
def parent (_path : String) : Option String := none

-- | Enter a table: safe → query it, dangerous → show schema
def enterTable (table : String) : IO (Option (AdbcTable × String)) := do
  if dangerousTables.contains table then
    statusMsg s!"Loading schema for {table} ..."
    ensureSchema
    let hasSchema ← schemaAvail
    let json ← osqueryi s!"pragma table_info('{table}')"
    if json.trimAscii.toString == "[]" || json.isEmpty then return none
    let join := if hasSchema then s!" LEFT JOIN osq.columns c ON c.table_name = '{escSql table}' AND c.col_name = j.name" else ""
    let desc := if hasSchema then ", COALESCE(c.col_desc, '') as description" else ""
    let some tbl ← jsonToTableSql json s!"SELECT j.*{desc} FROM read_json_auto('__TMP__') j{join}" | return none
    pure <| some (tbl, s!"schema:{table}")
  else
    statusMsg s!"Querying {table} ..."
    ensureSchema
    let cols ← tableColumns table
    pure <| (← jsonToTable (← osqueryi s!"SELECT * FROM {table}") (typedSelect cols)).map (·, table)

-- | Enrich a meta temp table with column descriptions from the schema DB
def enrichMeta (metaTblName tableName : String) : IO Unit := do
  if !(← schemaAvail) then return
  try
    let _ ← Adbc.query s!"ALTER TABLE {metaTblName} ADD COLUMN description VARCHAR DEFAULT ''"
    let _ ← Adbc.query s!"UPDATE {metaTblName} SET description = COALESCE((SELECT col_desc FROM osq.columns WHERE table_name = '{escSql tableName}' AND col_name = {metaTblName}.\"column\"), '')"
  catch e => Log.write "enrichMeta" s!"error: {e}"

-- | Cache for column descriptions (avoids per-frame SQL query)
initialize colDescCache : IO.Ref (Std.HashMap (String × String) String) ← IO.mkRef {}

-- | Look up column description for a given osquery table and column name
def colDesc (tableName colName : String) : IO String := do
  if !(← schemaAvail) then return ""
  let cache ← colDescCache.get
  match cache[(tableName, colName)]? with
  | some v => return v
  | none =>
    let v ← try
      Adbc.cellStr (← Adbc.query s!"SELECT col_desc FROM osq.columns WHERE table_name = '{escSql tableName}' AND col_name = '{escSql colName}' LIMIT 1") 0 0
    catch _ => pure ""
    colDescCache.modify (·.insert (tableName, colName) v)
    return v

end Tc.Osquery
