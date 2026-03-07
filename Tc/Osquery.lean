/-
  Osquery: browse osquery tables via osqueryi CLI + DuckDB read_json_auto
-/
import Tc.Error
import Tc.Render
import Tc.TmpDir

namespace Tc.Osquery

-- | Check if path is an osquery URI
def isOsquery (path : String) : Bool := path.startsWith "osquery://"

-- | Tables that require specific input columns and should not be auto-queried.
-- These scan filesystem/network on demand or require event logging configuration.
private def dangerousTables : Array String := #[
  "hash", "file", "augeas", "yara", "curl", "curl_certificate",
  "magic", "device_file", "carves", "suid_bin",
  "file_events", "process_events", "process_file_events", "socket_events",
  "hardware_events", "selinux_events", "seccomp_events", "syslog_events",
  "user_events", "apparmor_events"
]

-- | Run osqueryi with --json flag, return stdout
private def osqueryi (sql : String) : IO String := do
  let r ← Log.run "osquery" "osqueryi" #["--json", sql]
  if r.exitCode != 0 then
    throw <| IO.userError s!"osqueryi failed: {r.stderr.trimAscii.toString}"
  pure r.stdout

-- | List all osquery tables, returns TSV with path/size/date/type columns.
-- Safe tables get type ' ' (file-like, enters query), dangerous get 'd'-like schema view.
def list (_path : String) : IO String := do
  statusMsg "Loading osquery tables ..."
  let json ← osqueryi "SELECT name FROM osquery_registry WHERE registry='table' ORDER BY name"
  let names := extractNames (json.splitOn "\n" |>.foldl (· ++ ·) "")
  let hdr := "path\tsize\tdate\ttype"
  let body := names.map fun n =>
    let kind := if dangerousTables.contains n then "input-required" else "safe"
    s!"{n}\t{kind}\t\t "
  pure (hdr ++ (if body.isEmpty then "" else "\n" ++ "\n".intercalate body.toList))
where
  extractNames (json : String) : Array String :=
    -- parse [ {"name":"foo"}, ... ] by splitting on "name":"
    let parts := json.splitOn "\"name\":\""
    parts.drop 1 |>.foldl (fun acc part =>
      match part.splitOn "\"" with
      | name :: _ => acc.push name
      | _ => acc
    ) #[]

-- | Query a safe table, returns JSON string
def queryTable (table : String) : IO String := do
  statusMsg s!"Querying {table} ..."
  osqueryi s!"SELECT * FROM {table}"

-- | Get schema for a dangerous/input-required table, returns JSON string
def tableSchema (table : String) : IO String := do
  statusMsg s!"Loading schema for {table} ..."
  osqueryi s!"pragma table_info('{table}')"

-- | osquery parent: only root level, no hierarchy
def parent (_path : String) : Option String := none

-- | Enter a table: safe → query it, dangerous → show schema.
-- Returns JSON content to be loaded via fromJson.
def enterTable (table : String) : IO (String × String) := do
  if dangerousTables.contains table then
    let json ← tableSchema table
    pure (json, s!"schema:{table}")
  else
    let json ← queryTable table
    pure (json, table)

end Tc.Osquery
