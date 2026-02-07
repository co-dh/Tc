/-
  HF: helpers for browsing Hugging Face datasets via DuckDB's hf:// protocol
  Uses HF Hub API for directory listing, DuckDB httpfs for file reading.
-/
import Tc.Render
import Tc.Remote
import Tc.Error

namespace Tc.HF

-- | Check if path is a Hugging Face URI
def isHF (path : String) : Bool := path.startsWith "hf://"

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

-- | HF parent: none at dataset root ("hf://datasets/user/ds" = 5 parts)
def parent (path : String) : Option String := Remote.parent path 5

theorem parent_none_at_root : parent "hf://datasets/user/ds" = none := by native_decide

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
  let tmp := "/tmp/tc-hf-api.json"
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

-- | Build raw download URL for an HF file
def rawUrl (path : String) : Option String := do
  let (repo, sub) ← parsePath path
  if sub.isEmpty then none
  else some s!"https://huggingface.co/datasets/{repo}/resolve/main/{sub}"

-- | Get cached local path for HF file. Downloads if not cached.
def cachedPath (hfPath : String) : IO String := do
  let home ← IO.Process.output { cmd := "sh", args := #["-c", "echo $HOME"] }
  let homeDir := home.stdout.trimAscii.toString
  let base := s!"{homeDir}/.cache/tc/hf"
  let some (repo, sub) := parsePath hfPath | pure hfPath
  if sub.isEmpty then return hfPath
  let local_ := s!"{base}/{repo}/{sub}"
  let cached ← try discard (IO.FS.Handle.mk local_ .read); pure true catch _ => pure false
  if cached then
    Log.write "hf" s!"cache hit: {local_}"
    return local_
  statusMsg s!"Downloading {hfPath} ..."
  let parent := "/".intercalate (local_.splitOn "/" |>.dropLast)
  let _ ← IO.Process.output { cmd := "mkdir", args := #["-p", parent] }
  match rawUrl hfPath with
  | some url =>
    let r ← IO.Process.output { cmd := "curl", args := #["-sfL", "-o", local_, url] }
    if r.exitCode != 0 then
      Log.write "hf" s!"download failed: {url}"
      return hfPath
    Log.write "hf" s!"cached: {local_}"
    return local_
  | none => return hfPath

-- | Download HF file (uses cache)
def download (hfPath : String) : IO String := cachedPath hfPath
end Tc.HF
