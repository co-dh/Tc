# Config-Driven File Handling Design

## Problem

S3, HF, Osquery each have hardcoded logic in separate `.lean` modules.
Adding a new source requires a new module + if/else wiring in `Folder.lean`.

## Goal

A single **config table** — pure data, string templates — that drives all behavior.
Each row is a source. Columns define: how to list, download, detect type, load into DuckDB.

## Config Table

```
prefix     │ minParts │ listCmd                                          │ listParse  │ downloadCmd                             │ resolveSql
───────────┼──────────┼──────────────────────────────────────────────────┼────────────┼─────────────────────────────────────────┼──────────────────────────────────────────
s3://      │ 3        │ aws s3 ls {path}                                │ s3         │ aws s3 cp {path} {tmp}/{name}           │ SELECT * FROM '{local}'
hf://      │ 5        │ curl -sf https://huggingface.co/api/datasets/{repo}/tree/main/{sub} │ hf_json    │ curl -sfL -o {tmp}/{name} https://huggingface.co/datasets/{repo}/resolve/main/{sub} │ SELECT * FROM '{path}'
gs://      │ 3        │ gsutil ls -l {path}                             │ gs         │ gsutil cp {path} {tmp}/{name}           │ SELECT * FROM '{local}'
az://      │ 3        │ az storage blob list --container {container}    │ az_json    │ az storage blob download -c {container} -n {blob} -f {tmp}/{name} │ SELECT * FROM '{local}'
```

### Column Definitions

| Column       | Type     | Description |
|-------------|----------|-------------|
| `prefix`    | String   | URI prefix to match (`s3://`, `hf://`, `gs://`). First match wins. |
| `minParts`  | Nat      | Min `/`-separated parts before `parent` returns none (root threshold). |
| `listCmd`   | Template | Shell command to list contents. Placeholders: `{path}`, `{repo}`, `{sub}`. |
| `listParse` | Enum     | How to parse listing output into TSV: `s3` (PRE-based), `hf_json` (jq), `gs` (gsutil), `find` (local). |
| `downloadCmd` | Template | Shell command to download a file. Placeholders: `{path}`, `{tmp}`, `{name}`, `{repo}`, `{sub}`. |
| `resolveSql` | Template | SQL to create DuckDB temp table. `{path}` = original URI, `{local}` = downloaded file, `{tbl}` = table name. |

### Template Placeholders

| Placeholder | Meaning |
|------------|---------|
| `{path}`   | Full URI path (e.g. `s3://bucket/dir/file.csv`) |
| `{name}`   | Last path component (filename) |
| `{tmp}`    | Temp directory |
| `{local}`  | Local file path after download |
| `{repo}`   | Extracted repo identifier (HF: `user/dataset`) |
| `{sub}`    | Sub-path within repo/container |
| `{tbl}`    | DuckDB temp table name |
| `{container}` | Container/bucket name |

### Parse Modes (`listParse`)

Each parse mode converts CLI output → standard TSV (`name\tsize\tdate\ttype`):

| Mode      | Input format | Type detection |
|-----------|-------------|----------------|
| `s3`      | `aws s3 ls` text | `PRE ` prefix → dir, else file |
| `hf_json` | HF API JSON array | `.type == "directory"` → dir |
| `gs`      | `gsutil ls -l` text | trailing `/` → dir |
| `az_json` | Azure CLI JSON | `isDirectory` field |
| `find`    | `find -printf` output | `%y`: `d`/`f`/`l` |

### Special Cases

Some sources need behavior beyond the table:

| Source | Special | How |
|--------|---------|-----|
| S3     | `--no-sign-request` flag | Append to `listCmd`/`downloadCmd` when flag set |
| HF     | Root listing (`hf://`) | Separate `rootList` entry: loads from `hf_datasets.duckdb` |
| HF     | No download for DuckDB | `resolveSql` uses `{path}` directly (httpfs), not `{local}` |
| Osquery | Flat (no hierarchy) | `minParts = ∞`, parent always none |
| Osquery | Table query | `enterCmd`: `osqueryi --json "SELECT * FROM {name}"` |

### Lean Representation

```lean
structure SourceConfig where
  prefix      : String
  minParts    : Nat
  listCmd     : String       -- template string
  listParse   : String       -- "s3" | "hf_json" | "gs" | "find" | ...
  downloadCmd : String       -- template string
  resolveSql  : String       -- template string
  needsDownload : Bool       -- true: download first, use {local}. false: use {path} directly
  rootList    : Option String -- optional: SQL/cmd to list root (e.g. HF datasets, osquery tables)
  enterCmd    : Option String -- optional: cmd to "open" an entry (e.g. osqueryi for osquery)

-- The config table: a plain Array, loaded at init or compiled in
def sources : Array SourceConfig := #[
  { prefix := "s3://",  minParts := 3, listCmd := "aws s3 ls {path}",
    listParse := "s3", downloadCmd := "aws s3 cp {path} {tmp}/{name}",
    resolveSql := "SELECT * FROM '{local}'", needsDownload := true,
    rootList := none, enterCmd := none },
  { prefix := "hf://",  minParts := 5,
    listCmd := "curl -sf https://huggingface.co/api/datasets/{repo}/tree/main/{sub}",
    listParse := "hf_json",
    downloadCmd := "curl -sfL -o {tmp}/{name} https://huggingface.co/datasets/{repo}/resolve/main/{sub}",
    resolveSql := "SELECT * FROM '{path}'", needsDownload := false,
    rootList := some "hf_datasets", enterCmd := none },
  { prefix := "gs://",  minParts := 3, listCmd := "gsutil ls -l {path}",
    listParse := "gs", downloadCmd := "gsutil cp {path} {tmp}/{name}",
    resolveSql := "SELECT * FROM '{local}'", needsDownload := true,
    rootList := none, enterCmd := none },
]

def findSource (path : String) : Option SourceConfig :=
  sources.find? fun c => path.startsWith c.prefix
```

### Template Expansion

One generic function replaces placeholders:

```lean
def expand (tmpl : String) (vars : List (String × String)) : String :=
  vars.foldl (fun s (k, v) => s.replace s!"\{{k}}" v) tmpl
```

Usage:
```lean
let cmd := expand cfg.listCmd [("path", path), ("repo", repo), ("sub", sub)]
let out ← Log.run "list" "sh" #["-c", cmd]
```

### How Folder.lean Changes

```lean
-- BEFORE: if S3.isS3 then ... else if HF.isHF then ...
-- AFTER:
match findSource curDir with
| some cfg =>
  let vars := extractVars cfg.prefix curDir  -- parse repo, sub, container, etc.
  let cmd := expand cfg.listCmd vars
  let raw ← Log.run "source" "sh" #["-c", cmd]
  let tsv := parseListing cfg.listParse raw.stdout
  mkViewFromTsv tsv curDir depth (Remote.dispName curDir)
| none =>
  -- local filesystem fallback
  mkViewFromTsv (← listDir path depth) absPath depth disp
```

### Adding a New Source

Just add a row to the `sources` array. No new `.lean` file needed:

```lean
{ prefix := "r2://", minParts := 3,
  listCmd := "aws s3 ls --endpoint-url {endpoint} {path}",
  listParse := "s3",  -- reuse S3 parser since R2 is S3-compatible
  downloadCmd := "aws s3 cp --endpoint-url {endpoint} {path} {tmp}/{name}",
  resolveSql := "SELECT * FROM '{local}'", needsDownload := true,
  rootList := none, enterCmd := none }
```

### Implementation Plan

1. Add `SourceConfig` structure + `expand` + `parseListing` dispatcher in `Tc/SourceConfig.lean`
2. Define `sources` table with S3 and HF entries
3. Add `extractVars` to parse prefix-specific parts (repo/sub for HF, bucket for S3)
4. Refactor `Folder.lean`: replace `Backend`/`backend?` with `findSource` + template expansion
5. Keep `S3.lean`/`HF.lean` parse functions (reused by `listParse` dispatcher)
6. Test: existing tests pass unchanged
