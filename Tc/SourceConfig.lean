/-
  SourceConfig: config-driven file/folder handling for remote sources.
  Each source is a row in a config table with shell command templates
  and SQL for listing, downloading, and loading into DuckDB.

  Flow: CLI cmd → JSON → tmp file → read_json_auto → DuckDB temp table
-/
import Tc.Data.ADBC.Table
import Tc.Error
import Tc.Render
import Tc.Remote
import Tc.TmpDir

namespace Tc.SourceConfig

-- | How to parse CLI output before DuckDB ingestion
inductive ListFormat where
  | json     -- output is JSON array, use read_json_auto directly
  | s3Text   -- output is `aws s3 ls` text, convert to JSON in Lean
  deriving BEq

-- | Config entry for a remote source
structure Config where
  prefix         : String      -- URI prefix: "s3://", "hf://", etc.
  minParts       : Nat         -- min URI parts before parent returns none
  listCmd        : String      -- shell cmd template → stdout (JSON or text)
  listFormat     : ListFormat  -- how to interpret output
  listSql        : String      -- SQL SELECT to transform columns. Empty = SELECT *
  downloadCmd    : String      -- shell cmd template to download a file
  needsDownload  : Bool        -- true: download before DuckDB read. false: DuckDB reads URI
  dirSuffix      : Bool        -- true: append "/" when joining child dir paths
  parentFallback : String      -- fallback parent when at minParts root. Empty = none.

-- | Global flag: use --no-sign-request for S3 (set via +n arg)
initialize noSign : IO.Ref Bool ← IO.mkRef false

def setNoSign (b : Bool) : IO Unit := noSign.set b
def getNoSign : IO Bool := noSign.get

-- | Get S3 extra args string
def s3Extra : IO String := do
  if ← getNoSign then pure "--no-sign-request" else pure ""

-- | Escape a string for JSON output
private def escJson (s : String) : String :=
  s.replace "\\" "\\\\" |>.replace "\"" "\\\"" |>.replace "\n" "\\n" |>.replace "\t" "\\t"

-- | Convert `aws s3 ls` text output to JSON array
def s3TextToJson (text : String) : String :=
  let lines := text.splitOn "\n" |>.filter (·.length > 0)
  let entries := lines.filterMap fun line =>
    if line.trimAsciiStart.toString.startsWith "PRE " then
      let name := (line.trimAscii.toString.drop 4).toString
      let name := if name.endsWith "/" then (name.take (name.length - 1)).toString else name
      some s!"\{\"name\":\"{escJson name}\",\"size\":0,\"date\":\"\",\"type\":\"dir\"}"
    else
      let parts := line.trimAscii.toString.splitOn " " |>.filter (·.length > 0)
      if parts.length >= 4 then
        let dt := s!"{parts.getD 0 ""} {parts.getD 1 ""}"
        let sz := parts.getD 2 ""
        let name := parts.drop 3 |> " ".intercalate
        some s!"\{\"name\":\"{escJson name}\",\"size\":{sz},\"date\":\"{dt}\",\"type\":\"file\"}"
      else none
  "[" ++ ",".intercalate entries ++ "]"

-- | Split path into components after stripping prefix
def pathParts (prefix path : String) : Array String :=
  let rest := (path.drop prefix.length).toString
  let rest := if rest.endsWith "/" then (rest.take (rest.length - 1)).toString else rest
  if rest.isEmpty then #[] else (rest.splitOn "/").toArray

-- | Expand template placeholders: {path}, {name}, {tmp}, {extra}, {1}, {2}, {2+}, etc.
def expand (tmpl : String) (vars : Array (String × String)) : String :=
  vars.foldl (fun s (k, v) => s.replace s!"\{{k}}" v) tmpl

-- | Build template variables from a config and path
-- Provides {path}, {name}, {tmp}, {extra}, {1}..{9}, {1+}..{9+}
-- Missing parts default to empty string so unreplaced placeholders don't leak
def mkVars (cfg : Config) (path tmp name extra : String) : Array (String × String) :=
  let parts := pathParts cfg.prefix path
  let baseVars := #[("path", path), ("tmp", tmp), ("name", name), ("extra", extra)]
  -- {N} = Nth part, empty if out of range. Up to 9.
  let numbered := (List.range 9).map fun i =>
    (s!"{i + 1}", parts.getD i "")
  -- {N+} = parts from N onward joined by "/". Empty if out of range.
  let plus := (List.range 9).map fun i =>
    (s!"{i + 1}+", "/".intercalate (parts.toList.drop i))
  baseVars ++ numbered.toArray ++ plus.toArray

/-! ## Config Table -/

def s3Cfg : Config where
  prefix         := "s3://"
  minParts       := 3
  listCmd        := "aws s3 ls {path} {extra}"
  listFormat     := .s3Text
  listSql        := ""
  downloadCmd    := "aws s3 cp {extra} {path} {tmp}/{name}"
  needsDownload  := true
  dirSuffix      := true
  parentFallback := ""

def hfCfg : Config where
  prefix         := "hf://datasets/"
  minParts       := 5
  listCmd        := "curl -sf https://huggingface.co/api/datasets/{1}/{2}/tree/main/{3+}"
  listFormat     := .json
  listSql        := "SELECT split_part(path, '/', -1) as name, size, type FROM read_json_auto('{src}')"
  downloadCmd    := "curl -sfL -o {tmp}/{name} https://huggingface.co/datasets/{1}/{2}/resolve/main/{3+}"
  needsDownload  := false
  dirSuffix      := true
  parentFallback := "hf://"

-- | All registered sources. First prefix match wins.
def sources : Array Config := #[s3Cfg, hfCfg]

-- | Find config for a path by prefix match
def findSource (path : String) : Option Config :=
  sources.find? fun c => path.startsWith c.prefix

/-! ## Generic Operations -/

-- | Parent path navigation using Remote.parent with config's minParts
def Config.parent (cfg : Config) (path : String) : Option String :=
  match Remote.parent path cfg.minParts with
  | some p => some p
  | none => if cfg.parentFallback.isEmpty then none else some cfg.parentFallback

-- | Run listing command, ingest JSON into DuckDB temp table, return AdbcTable
def Config.runList (cfg : Config) (path : String) : IO (Option AdbcTable) := do
  statusMsg s!"Loading {path} ..."
  let p := if path.endsWith "/" then path else s!"{path}/"
  let tmpDir ← Tc.tmpPath "src"
  let _ ← Log.run "src" "mkdir" #["-p", tmpDir]
  let name := path.splitOn "/" |>.filter (·.length > 0) |>.getLast? |>.getD "file"
  let extra ← if cfg.prefix == "s3://" then s3Extra else pure ""
  let vars := mkVars cfg p tmpDir name extra
  let cmd := expand cfg.listCmd vars
  Log.write "src" s!"list: {cmd}"
  let out ← IO.Process.output { cmd := "sh", args := #["-c", cmd] }
  if out.exitCode != 0 then
    Log.write "src" s!"list failed (exit {out.exitCode}): {out.stderr.trimAscii.toString}"
    errorPopup s!"List failed: {out.stderr.trimAscii.toString}"
    return none
  -- Convert to JSON based on format
  let json := match cfg.listFormat with
    | .json => out.stdout
    | .s3Text => s3TextToJson out.stdout
  if json.trimAscii.toString.isEmpty || json.trimAscii.toString == "[]" then return none
  -- Add ".." parent entry if applicable
  let json := if cfg.parent path |>.isSome then
    let parentEntry := "{\"name\":\"..\",\"size\":0,\"date\":\"\",\"type\":\"dir\"}"
    -- Prepend parent entry to JSON array
    if json.trimAscii.toString.startsWith "[" then
      "[" ++ parentEntry ++ "," ++ (json.trimAscii.toString.drop 1)
    else json
  else json
  -- Save JSON and ingest via DuckDB
  let tmpFile ← Tc.tmpPath "src-list.json"
  IO.FS.writeFile tmpFile json
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tbl := s!"tc_src_{n}"
  let sql := if cfg.listSql.isEmpty
    then s!"CREATE TEMP TABLE {tbl} AS SELECT * FROM read_json_auto('{tmpFile}')"
    else s!"CREATE TEMP TABLE {tbl} AS {expand cfg.listSql #[("src", tmpFile)]}"
  try
    let _ ← Adbc.query sql
    try IO.FS.removeFile tmpFile catch _ => pure ()
    let q : Prql.Query := { base := s!"from {tbl}" }
    AdbcTable.requery q (← AdbcTable.queryCount q)
  catch e =>
    try IO.FS.removeFile tmpFile catch _ => pure ()
    Log.write "src" s!"ingest error: {e}"
    errorPopup e.toString
    return none

-- | Download a remote file to local temp path
def Config.runDownload (cfg : Config) (path : String) : IO String := do
  statusMsg s!"Downloading {path} ..."
  let tmpDir ← Tc.tmpPath "src"
  let _ ← Log.run "src" "mkdir" #["-p", tmpDir]
  let name := path.splitOn "/" |>.filter (·.length > 0) |>.getLast? |>.getD "file"
  let extra ← if cfg.prefix == "s3://" then s3Extra else pure ""
  let vars := mkVars cfg path tmpDir name extra
  let cmd := expand cfg.downloadCmd vars
  Log.write "src" s!"download: {cmd}"
  let _ ← IO.Process.output { cmd := "sh", args := #["-c", cmd] }
  pure s!"{tmpDir}/{name}"

-- | Resolve data file path: download if needed, or return URI for DuckDB
def Config.resolve (cfg : Config) (path : String) : IO String := do
  if cfg.needsDownload then cfg.runDownload path
  else pure path

end Tc.SourceConfig
