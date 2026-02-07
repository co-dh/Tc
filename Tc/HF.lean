/-
  HF: helpers for browsing Hugging Face datasets via DuckDB's hf:// protocol
  Uses HF Hub API for directory listing, DuckDB httpfs for file reading.
-/
import Tc.Render

namespace Tc.HF

-- | Check if path is a Hugging Face URI
def isHF (path : String) : Bool := path.startsWith "hf://"

-- | Parse HF path into (user/dataset, subpath)
-- "hf://datasets/user/dataset/sub/path" → ("user/dataset", "sub/path")
-- "hf://datasets/user/dataset" → ("user/dataset", "")
def parsePath (path : String) : Option (String × String) :=
  let p := if path.endsWith "/" then (path.take (path.length - 1)).toString else path
  let rest := (p.drop 5).toString  -- drop "hf://"
  -- expect "datasets/user/dataset[/subpath]"
  let parts := rest.splitOn "/"
  if parts.length < 3 then none  -- need at least "datasets/user/dataset"
  else
    let user := parts.getD 1 ""
    let dataset := parts.getD 2 ""
    let sub := "/".intercalate (parts.drop 3)
    some (s!"{user}/{dataset}", sub)

-- | Get parent HF path: "hf://datasets/user/ds/a/b" → some "hf://datasets/user/ds/a/"
-- Returns none at dataset root ("hf://datasets/user/ds")
def parent (path : String) : Option String :=
  let p := if path.endsWith "/" then (path.take (path.length - 1)).toString else path
  let parts := p.splitOn "/"
  -- hf://datasets/user/dataset = 5 parts minimum (["hf:", "", "datasets", "user", "dataset"])
  if parts.length ≤ 5 then none
  else some ("/".intercalate (parts.dropLast) ++ "/")

-- | Join HF prefix with child name
def join (pfx name : String) : String :=
  if pfx.endsWith "/" then s!"{pfx}{name}" else s!"{pfx}/{name}"

-- | parent returns none at dataset root
theorem parent_none_at_root : parent "hf://datasets/user/ds" = none := by native_decide

-- | join with trailing slash doesn't double-slash
theorem join_trailing_slash : join "hf://datasets/u/d/" "x" = "hf://datasets/u/d/x" := by native_decide

-- | Build HF Hub API URL for listing
-- path "hf://datasets/user/dataset/subdir" → "https://huggingface.co/api/datasets/user/dataset/tree/main/subdir"
def apiUrl (path : String) : Option String := do
  let (repo, sub) ← parsePath path
  if sub.isEmpty then
    some s!"https://huggingface.co/api/datasets/{repo}/tree/main"
  else
    some s!"https://huggingface.co/api/datasets/{repo}/tree/main/{sub}"

-- | List HF dataset directory via Hub API, returns TSV matching folder schema
-- Uses curl + jq to parse JSON response into TSV format
def list (path : String) : IO String := do
  statusMsg s!"Loading {path} ..."
  let some url := apiUrl path | return ""
  -- curl the API, pipe through jq to extract TSV
  let jqExpr := ".[] | [.path, (.size // 0 | tostring), \"\", (if .type == \"directory\" then \"d\" else \" \" end)] | @tsv"
  let out ← IO.Process.output {
    cmd := "sh"
    args := #["-c", s!"curl -sf '{url}' | jq -r '{jqExpr}'"]
  }
  if out.exitCode != 0 then return ""
  let lines := out.stdout.splitOn "\n" |>.filter (·.length > 0)
  -- strip common prefix: HF API returns full paths like "data/train.parquet"
  -- but if we're in subdir "data", show just "train.parquet"
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

-- | Display name for HF path (last meaningful component)
def dispName (path : String) : String :=
  let p := if path.endsWith "/" then (path.take (path.length - 1)).toString else path
  p.splitOn "/" |>.filter (·.length > 0) |>.getLast? |>.getD path

end Tc.HF
