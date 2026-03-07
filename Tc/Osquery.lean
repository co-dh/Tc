/-
  Osquery: browse osquery tables via osqueryi CLI + DuckDB read_json_auto
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

-- | Run osqueryi, write JSON to tmp, load into DuckDB with extra SQL, return AdbcTable
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

-- | List all osquery tables with metadata. Returns AdbcTable with path/type for folder view.
def list (_path : String) : IO (Option AdbcTable) := do
  statusMsg "Loading osquery tables ..."
  osqueryToTable
    ("SELECT t.name, COUNT(c.name) as columns, GROUP_CONCAT(c.name) as cols " ++
     "FROM osquery_registry t LEFT JOIN pragma_table_info(t.name) c ON 1=1 " ++
     "WHERE t.registry='table' GROUP BY t.name ORDER BY t.name")
    (s!"CASE WHEN name IN {dangerousIn} THEN 'input-required' ELSE 'safe' END as safety, " ++
     "name as path, ' ' as type")

-- | osquery parent: only root level, no hierarchy
def parent (_path : String) : Option String := none

-- | Enter a table: safe → query it, dangerous → show schema.
-- Returns AdbcTable and display label.
def enterTable (table : String) : IO (Option (AdbcTable × String)) := do
  if dangerousTables.contains table then
    statusMsg s!"Loading schema for {table} ..."
    let tbl ← osqueryToTable s!"pragma table_info('{table}')"
    pure <| tbl.map (·, s!"schema:{table}")
  else
    statusMsg s!"Querying {table} ..."
    let tbl ← osqueryToTable s!"SELECT * FROM {table}"
    pure <| tbl.map (·, table)

end Tc.Osquery
