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

-- | Map osquery type to DuckDB type for CAST
private def duckdbType (osqType : String) : Option String :=
  match osqType.toLower with
  | "bigint" | "integer" | "int" => some "BIGINT"
  | "double" => some "DOUBLE"
  | _ => none

-- | Get column schema from pragma table_info, returns (name, osqueryType) pairs
private def tableColumns (table : String) : IO (Array (String × String)) := do
  let json ← osqueryi s!"pragma table_info('{table}')"
  if json.trimAscii.toString == "[]" || json.isEmpty then return #[]
  -- Parse name/type pairs from JSON using simple string splits
  let entries := json.splitOn "{" |>.drop 1
  let cols := entries.filterMap fun entry =>
    let getName := entry.splitOn "\"name\":\"" |>.getD 1 "" |>.splitOn "\"" |>.head?
    let getType := entry.splitOn "\"type\":\"" |>.getD 1 "" |>.splitOn "\"" |>.head?
    match getName, getType with
    | some n, some t => if !n.isEmpty then some (n, t) else none
    | _, _ => none
  pure cols.toArray

-- | Build typed SELECT: CAST columns to proper types based on pragma schema
private def typedSelect (cols : Array (String × String)) : String :=
  if cols.isEmpty then "*"
  else
    let parts := cols.map fun (name, typ) =>
      match duckdbType typ with
      | some dt => s!"TRY_CAST(\"{name}\" AS {dt}) AS \"{name}\""
      | none => s!"\"{name}\""
    ", ".intercalate parts.toList

-- | Run osqueryi, write JSON to tmp, load into DuckDB temp table, return AdbcTable
private def osqueryToTable (sql : String) (extraSql : String := "") (castSelect : String := "*") : IO (Option AdbcTable) := do
  let json ← osqueryi sql
  if json.trimAscii.toString == "[]" || json.isEmpty then return none
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tmpFile ← Tc.tmpPath s!"osq-{n}.json"
  IO.FS.writeFile tmpFile json
  let tblName := s!"tc_osq_{n}"
  try
    let selectPart := if extraSql.isEmpty then castSelect else s!"{castSelect}, {extraSql}"
    let _ ← Adbc.query s!"CREATE TEMP TABLE {tblName} AS SELECT {selectPart} FROM read_json_auto('{tmpFile}')"
    try IO.FS.removeFile tmpFile catch _ => pure ()
    let query : Prql.Query := { base := s!"from {tblName}" }
    let total ← AdbcTable.queryCount query
    AdbcTable.requery query total
  catch e =>
    try IO.FS.removeFile tmpFile catch _ => pure ()
    Log.write "osqueryToTable" s!"error: {e.toString}"
    errorPopup e.toString
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

-- | Parse table names from osquery JSON: [{"name":"foo"},...]
private def parseNames (json : String) : Array String :=
  let entries := json.splitOn "\"name\":\"" |>.drop 1
  entries.filterMap (fun e => let n := e.splitOn "\"" |>.head?; n.filter (!·.isEmpty)) |>.toArray

-- | Count rows for a single table with timeout
private def countTable (name : String) : IO (Option (String × Nat)) := do
  let r ← IO.Process.output { cmd := "timeout", args := #["-k", "1", "2", "osqueryi", "--json",
    s!"SELECT count(*) as n FROM {name}"] }
  if r.exitCode != 0 then return none
  let nStr := ((r.stdout.splitOn "\"n\":\"" |>.getD 1 "").splitOn "\"" |>.head?).getD ""
  match nStr.toNat? with
  | some n => pure (some (name, n))
  | none => pure none

-- | Count rows for all safe tables concurrently via BaseIO tasks.
private def countAllTables (names : Array String) : IO (Array (String × Nat)) := do
  let safeNames := names.filter (!dangerousTables.contains ·)
  let tasks ← safeNames.mapM fun name =>
    IO.asTask (prio := .dedicated) (countTable name)
  let mut results := #[]
  for task in tasks do
    match ← IO.wait task with
    | .ok (some r) => results := results.push r
    | _ => pure ()
  pure results

-- | Load cached row counts from osq.row_counts. Returns (counts, updated_at) or empty.
private def loadCachedCounts : IO (Array (String × Nat) × String) := do
  if !(← schemaAvail.get) then return (#[], "")
  try
    let qr ← Adbc.query "SELECT max(updated_at) as ts FROM osq.row_counts"
    let ts ← Adbc.cellStr qr 0 0
    -- Check age: invalidate if > 24h
    let ageQr ← Adbc.query "SELECT CASE WHEN max(updated_at)::TIMESTAMP > (CURRENT_TIMESTAMP::TIMESTAMP - INTERVAL '24 hours') THEN 1 ELSE 0 END FROM osq.row_counts"
    let fresh ← Adbc.cellInt ageQr 0 0
    if fresh.toNat == 0 then return (#[], "")
    let allQr ← Adbc.query "SELECT name, rows FROM osq.row_counts"
    let cnt ← Adbc.cellInt (← Adbc.query "SELECT count(*) FROM osq.row_counts") 0 0
    let mut results := #[]
    for i in [:cnt.toNat] do
      let name ← Adbc.cellStr allQr i.toUInt64 0
      let rows ← Adbc.cellInt allQr i.toUInt64 1
      results := results.push (name, rows.toNat)
    pure (results, ts)
  catch _ => pure (#[], "")

-- | Save row counts to persistent osq.row_counts
private def saveCounts (counts : Array (String × Nat)) : IO Unit := do
  if !(← schemaAvail.get) then return
  try
    let _ ← Adbc.query "DROP TABLE IF EXISTS osq.row_counts"
    let _ ← Adbc.query "CREATE TABLE osq.row_counts (name VARCHAR, rows INTEGER, updated_at VARCHAR)"
    if !counts.isEmpty then
      -- Get local time string from OS
      let now ← IO.Process.output { cmd := "date", args := #["+%Y-%m-%d %H:%M"] }
      let ts := now.stdout.trimAscii.toString
      let values := counts.map fun (name, cnt) =>
        s!"('{name.replace "'" "''"}', {cnt}, '{ts}')"
      let _ ← Adbc.query s!"INSERT INTO osq.row_counts (name, rows, updated_at) VALUES {", ".intercalate values.toList}"
    Log.write "osquery" s!"Cached {counts.size} row counts"
  catch e => Log.write "osquery" s!"saveCounts error: {e}"

-- | Format cache label: "2026-03-07 13:05" → "osquery:// (cached 13:05)"
private def cacheLabel (ts : String) : String :=
  let hm := (ts.splitOn " " |>.getD 1 "").take 5 |>.toString
  if hm.isEmpty then "osquery://" else s!"osquery:// (cached {hm})"

/-! ## Public API -/

-- | List all osquery tables with metadata and description. Returns (AdbcTable, dispName).
def list (_path : String) : IO (Option (AdbcTable × String)) := do
  statusMsg "Loading osquery tables ..."
  ensureSchema
  let json ← osqueryi
    "SELECT name FROM osquery_registry WHERE registry='table' ORDER BY name"
  if json.trimAscii.toString == "[]" || json.isEmpty then return none
  let names := parseNames json
  -- Try cache first, fall back to live counting
  let (counts, cacheLabel) ← do
    let (cached, ts) ← loadCachedCounts
    if !cached.isEmpty then
      pure (cached, cacheLabel ts)
    else
      statusMsg "Counting rows ..."
      let live ← countAllTables names
      saveCounts live
      pure (live, "osquery://")
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tmpFile ← Tc.tmpPath s!"osq-{n}.json"
  IO.FS.writeFile tmpFile json
  let tblName := s!"tc_osq_{n}"
  let hasSchema ← schemaAvail.get
  try
    let _ ← Adbc.query
      (s!"CREATE TEMP TABLE {tblName} AS SELECT j.*, " ++
       s!"CASE WHEN j.name IN {dangerousIn} THEN 'input-required' ELSE 'safe' END as safety" ++
       s!" FROM read_json_auto('{tmpFile}') j")
    try IO.FS.removeFile tmpFile catch _ => pure ()
    -- Add rows column via single CASE UPDATE
    let _ ← Adbc.query s!"ALTER TABLE {tblName} ADD COLUMN rows INTEGER DEFAULT NULL"
    if !counts.isEmpty then
      let cases := counts.map fun (name, cnt) =>
        s!"WHEN '{name.replace "'" "''"}' THEN {cnt}"
      let caseExpr := "CASE name " ++ " ".intercalate cases.toList ++ " END"
      let nameList := counts.map fun (name, _) => s!"'{name.replace "'" "''"}'"
      let inList := ", ".intercalate nameList.toList
      let _ ← Adbc.query s!"UPDATE {tblName} SET rows = {caseExpr} WHERE name IN ({inList})"
    -- Add description as last column (after rows)
    if hasSchema then
      let _ ← Adbc.query s!"ALTER TABLE {tblName} ADD COLUMN description VARCHAR DEFAULT ''"
      let _ ← Adbc.query s!"UPDATE {tblName} SET description = COALESCE((SELECT s.description FROM osq.tables s WHERE s.name = {tblName}.name), '')"
    let query : Prql.Query := { base := s!"from {tblName}" }
    let total ← AdbcTable.queryCount query
    let tbl ← AdbcTable.requery query total
    pure <| tbl.map (·, cacheLabel)
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
    let cols ← tableColumns table
    let tbl ← osqueryToTable s!"SELECT * FROM {table}" (castSelect := typedSelect cols)
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
