/-
  HF: helpers for browsing Hugging Face datasets via DuckDB's hf:// protocol
  Uses HF Hub API for directory listing, DuckDB httpfs for file reading.
  Dataset discovery (hf://) uses a pre-populated DuckDB from scripts/hf_datasets.py.
-/
import Tc.Data.ADBC.Table
import Tc.Render
import Tc.Remote
import Tc.Error
import Tc.TmpDir

namespace Tc.HF

-- | Check if path is a Hugging Face URI
def isHF (path : String) : Bool := path.startsWith "hf://"

-- | Is this the HF root listing? (bare "hf://" or "hf://datasets")
def isRoot (path : String) : Bool :=
  path == "hf://" || path == "hf://datasets" || path == "hf://datasets/"

-- | Parse HF path into (user/dataset, subpath)
-- "hf://datasets/user/dataset/sub/path" → ("user/dataset", "sub/path")
-- First component must be "datasets" (DuckDB convention)
def parsePath (path : String) : Option (String × String) :=
  let p := if path.endsWith "/" then (path.take (path.length - 1)).toString else path
  let rest := (p.drop 5).toString  -- drop "hf://"
  let parts := rest.splitOn "/"
  if parts.length < 3 then none
  else if parts.getD 0 "" != "datasets" then none
  else
    let user := parts.getD 1 ""
    let dataset := parts.getD 2 ""
    let sub := "/".intercalate (parts.drop 3)
    some (s!"{user}/{dataset}", sub)

-- | HF parent: dataset root → "hf://" (listing), inside dataset → strip last component
def parent (path : String) : Option String :=
  if isRoot path then none  -- already at root listing
  else match Remote.parent path 5 with
    | some p => some p
    | none => some "hf://"  -- at dataset root → back to listing

-- | Build HF Hub API URL for listing
def apiUrl (path : String) : Option String := do
  let (repo, sub) ← parsePath path
  if sub.isEmpty then
    some s!"https://huggingface.co/api/datasets/{repo}/tree/main"
  else
    some s!"https://huggingface.co/api/datasets/{repo}/tree/main/{sub}"

-- | List HF dataset directory via Hub API, returns TSV matching folder schema
def list (path : String) : IO String := do
  statusMsg s!"Loading {path} ..."
  let some url := apiUrl path | do
    Log.write "hf" "Invalid HF path"
    return ""
  let curlOut ← IO.Process.output { cmd := "curl", args := #["-sf", url] }
  if curlOut.exitCode != 0 then
    Log.write "hf" s!"API request failed for: {url}"
    return ""
  -- write JSON to temp file, then run jq on it (avoids shell quoting issues)
  let tmp ← Tc.tmpPath "hf-api.json"
  IO.FS.writeFile tmp curlOut.stdout
  let jqFilter := ".[] | [.path, (.size|tostring), \"-\", ({\"directory\":\"d\",\"file\":\" \"}[.type] // .type)] | @tsv"
  let out ← IO.Process.output { cmd := "jq", args := #["-r", jqFilter, tmp] }
  try IO.FS.removeFile tmp catch _ => pure ()
  if out.exitCode != 0 then return ""
  let lines := out.stdout.splitOn "\n" |>.filter (·.length > 0)
  -- strip common prefix: HF API returns full paths like "data/train.parquet"
  let some (_, sub) := parsePath path | return ""
  let pfx := if sub.isEmpty then "" else if sub.endsWith "/" then sub else s!"{sub}/"
  let body := lines.map fun line =>
    if pfx.isEmpty then line
    else
      let parts := line.splitOn "\t"
      if parts.length >= 1 then
        let p := parts.getD 0 ""
        let stripped := if p.startsWith pfx then (p.drop pfx.length).toString else p
        let rest := parts.drop 1
        "\t".intercalate (stripped :: rest)
      else line
  let hdr := "path\tsize\tdate\ttype"
  let parentEntry := if parent path |>.isSome then "..\t0\t\td" else ""
  let entries := if parentEntry.isEmpty then body else [parentEntry] ++ body
  pure (hdr ++ (if entries.isEmpty then "" else "\n" ++ "\n".intercalate entries))

-- | Resolve data file: return hf:// URL as-is (DuckDB reads via httpfs, caches in temp table)
def resolve (_hfPath : String) : IO String := pure _hfPath

-- | Download file to /tmp for non-data viewing (bat/less)
def download (hfPath : String) : IO String := do
  let some (repo, sub) := parsePath hfPath | return hfPath
  if sub.isEmpty then return hfPath
  let url := s!"https://huggingface.co/datasets/{repo}/resolve/main/{sub}"
  let base := sub.splitOn "/" |>.getLast? |>.getD "file"
  let tmp ← Tc.tmpPath s!"hf-{base}"
  statusMsg s!"Downloading {hfPath} ..."
  let r ← IO.Process.output { cmd := "curl", args := #["-sfL", "-o", tmp, url] }
  if r.exitCode != 0 then
    Log.write "hf" s!"download failed: {url}"
    return hfPath
  return tmp

/-! ## Dataset Listing (from pre-populated DuckDB) -/

-- | DB state: none=not tried, some false=tried+failed, some true=available
initialize hfDbState : IO.Ref (Option Bool) ← IO.mkRef none

private def dbAvail : IO Bool := (·.getD false) <$> hfDbState.get

-- | Find and run the Python script
private def runPythonSetup : IO Bool := do
  let exe ← IO.appPath
  let exeDir := exe.parent.getD "."
  let candidates := #[
    s!"{exeDir}/scripts/hf_datasets.py",
    s!"{exeDir}/../scripts/hf_datasets.py",
    s!"{exeDir}/../../scripts/hf_datasets.py"
  ]
  let mut script := "scripts/hf_datasets.py"
  for c in candidates do
    if ← (c : System.FilePath).pathExists then script := c; break
  let r ← IO.Process.output { cmd := "python3", args := #[script] }
  if r.exitCode != 0 then
    Log.write "hf" s!"Python script failed: {r.stderr.trimAscii.toString}"
    errorPopup s!"HF listing failed: {r.stderr.trimAscii.toString}"
    return false
  return true

-- | Attach the HF DB (created by scripts/hf_datasets.py). Runs once per session.
private def ensureDb : IO Unit := do
  if (← hfDbState.get).isSome then return
  hfDbState.set (some false)
  let homeDir := (← IO.getEnv "HOME").getD "/tmp"
  let dbPath := s!"{homeDir}/.cache/tc/hf_datasets.duckdb"
  try
    let _ ← Adbc.query s!"ATTACH '{dbPath}' AS hf (READ_ONLY)"
  catch _ =>
    try let _ ← Adbc.query s!"ATTACH '{dbPath}' AS hf (READ_ONLY)"
    catch e => Log.write "hf" s!"ATTACH failed: {e}"; return
  let populated : Bool ← try
    let qr ← Adbc.query "SELECT count(*) FROM hf.listing"
    pure (decide ((← Adbc.cellInt qr 0 0).toNat > 0))
  catch _ => pure false
  if populated then
    hfDbState.set (some true)
    Log.write "hf" s!"HF DB attached ({dbPath})"
  else
    Log.write "hf" s!"HF DB empty or missing ({dbPath})"

-- | List all HF datasets from the pre-populated DB
def listAll : IO (Option (AdbcTable × String)) := do
  statusMsg "Loading HF datasets ..."
  if !(← runPythonSetup) then return none
  ensureDb
  if !(← dbAvail) then return none
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tbl := s!"tc_hf_{n}"
  try
    let _ ← Adbc.query s!"CREATE TEMP TABLE {tbl} AS SELECT id, downloads, likes, description, license, task, language, created, modified FROM hf.listing ORDER BY downloads DESC"
    let q : Prql.Query := { base := s!"from {tbl}" }
    let total ← AdbcTable.queryCount q
    (·.map (·, "hf://")) <$> AdbcTable.requery q total
  catch e =>
    Log.write "hf" s!"list error: {e}"
    errorPopup e.toString
    return none

end Tc.HF
