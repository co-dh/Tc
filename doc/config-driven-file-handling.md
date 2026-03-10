# Config-Driven File Handling Design

## Problem

S3, HF, Osquery, and local files each have hardcoded logic in separate modules
(`S3.lean`, `HF.lean`, `Osquery.lean`, `Folder.lean`). Adding a new source
(GCS, Azure Blob, HTTP, SFTP, etc.) requires writing a new Lean module and
wiring it into `Folder.backend?` with if/else chains.

## Goal

A single config structure that defines, per prefix:
1. **Is it a file or folder?** — how to list children, detect type
2. **How to fetch?** — curl/shell commands to list and download
3. **How to load into DuckDB?** — SQL to create a temp table

## Current State

```
Prefix       │ isX check     │ list cmd              │ download cmd          │ DuckDB load
─────────────┼───────────────┼───────────────────────┼───────────────────────┼─────────────────────
s3://        │ startsWith    │ aws s3 ls {path}      │ aws s3 cp {src} {dst} │ SELECT * FROM '{local}'
hf://datasets│ startsWith    │ curl HF API + jq→TSV  │ curl -sfL -o {dst}    │ SELECT * FROM '{hf_url}'
             │               │                       │   {resolve_url}       │   (httpfs, no download)
osquery://   │ startsWith    │ python3 script → DB   │ osqueryi --json {sql} │ read_json_auto('{tmp}')
(local)      │ fallthrough   │ find -H {p} -maxdepth │ (already local)       │ SELECT * FROM '{path}'
duckdb://    │ startsWith    │ duckdb_tables()       │ n/a                   │ FROM extdb.{table}
```

## Design

### SourceConfig structure

```lean
structure SourceConfig where
  prefix      : String                          -- "s3://", "hf://datasets/", "file://", etc.
  minParts    : Nat                             -- min URI parts before parent returns none
  -- Listing
  listCmd     : String → IO String             -- path → TSV (name\tsize\tdate\ttype)
  -- Type detection: from TSV type column
  -- "d" = directory, "f" or " " = file  (already standardized in TSV output)
  -- Parent navigation
  parentFn    : String → Option String         -- path → parent path (or none at root)
  -- Data resolution: how to get a path DuckDB can read
  resolveData : String → IO String             -- remote path → local or URL that DuckDB can read
  -- Download for viewing (bat/less) — distinct from resolveData when DuckDB can read natively (e.g. hf://)
  downloadView: String → IO String             -- remote path → local file path
  -- DuckDB table creation
  loadSql     : String → String → String       -- (resolved_path, table_name) → CREATE TABLE SQL
  -- Optional: root listing (like HF dataset browser, osquery table browser)
  rootList    : Option (IO (Option (AdbcTable × String)))
  -- Optional: flags/state
  extraArgs   : IO (Array String)              -- e.g. --no-sign-request for S3
```

### Config Registry

```lean
-- A flat array, checked in order (first prefix match wins)
def sourceConfigs : Array SourceConfig := #[
  s3Config,
  hfConfig,
  osqueryConfig,
  duckdbConfig
  -- add new sources here
]

def findConfig (path : String) : Option SourceConfig :=
  sourceConfigs.find? fun c => path.startsWith c.prefix
```

### Per-Source Configs

#### S3

```
prefix:       "s3://"
minParts:     3
listCmd:      aws s3 ls [--no-sign-request] {path}  → parse PRE/file → TSV
parentFn:     Remote.parent path 3
resolveData:  aws s3 cp {src} {tmpdir}/s3/{name}  → return local path
downloadView: same as resolveData
loadSql:      CREATE TEMP TABLE {tbl} AS SELECT * FROM '{local_path}'
rootList:     none
extraArgs:    ["--no-sign-request"] if noSign flag set
```

#### HuggingFace

```
prefix:       "hf://"
minParts:     5
listCmd:      curl -sf https://huggingface.co/api/datasets/{repo}/tree/main[/{sub}]
              | jq → TSV
parentFn:     Remote.parent path 5, fallback to "hf://"
resolveData:  identity (return hf:// URL as-is — DuckDB reads via httpfs)
downloadView: curl -sfL -o {tmp} https://huggingface.co/datasets/{repo}/resolve/main/{sub}
loadSql:      CREATE TEMP TABLE {tbl} AS SELECT * FROM '{hf_url}'
rootList:     some (listAll from hf.listing DB)
extraArgs:    #[]
```

#### Osquery

```
prefix:       "osquery://"
minParts:     n/a (flat)
listCmd:      (from pre-populated DB: osq.listing)
parentFn:     always none (flat)
resolveData:  osqueryi --json "SELECT * FROM {table}" → write JSON → tmpfile
downloadView: same as resolveData
loadSql:      CREATE TEMP TABLE {tbl} AS SELECT * FROM read_json_auto('{tmp}')
rootList:     some (list from osq.listing)
extraArgs:    #[]
```

#### Local filesystem

```
prefix:       "" (fallthrough / default)
minParts:     n/a
listCmd:      find -H {path} -maxdepth {depth} -printf "%y\t%s\t%T+\t%p\n"
parentFn:     pop stack or ".."
resolveData:  identity (already local)
downloadView: identity
loadSql:      CREATE TEMP TABLE {tbl} AS SELECT * FROM '{path}'
rootList:     none
extraArgs:    #[]
```

### How It Fits Together

#### Folder.lean changes

Replace `backend?` with config lookup:

```lean
-- BEFORE (hardcoded)
private def backend? (path : String) : Option Backend :=
  if S3.isS3 path then some ⟨S3.list, S3.parent, S3.download, S3.download⟩
  else if HF.isHF path then some ⟨HF.list, HF.parent, HF.resolve, HF.download⟩
  else none

-- AFTER (config-driven)
private def backend? (path : String) : Option SourceConfig :=
  findConfig path
```

`mkView`, `enter`, `del`, `setDepth` all use the config's functions instead of
calling `S3.list`, `HF.parent`, etc. directly.

#### Adding a new source (example: GCS)

Just add to the config array:

```lean
def gcsConfig : SourceConfig := {
  prefix      := "gs://"
  minParts    := 3
  listCmd     := fun path => do
    let out ← Log.run "gcs" "gsutil" #["ls", "-l", path]
    parseGcsListing out.stdout  -- → TSV
  parentFn    := Remote.parent · 3
  resolveData := fun path => do
    let tmp ← Tc.tmpPath "gcs"
    let _ ← Log.run "gcs" "gsutil" #["cp", path, tmp]
    pure tmp
  downloadView := fun path => resolveData path
  loadSql     := fun resolved tbl => s!"CREATE TEMP TABLE {tbl} AS SELECT * FROM '{resolved}'"
  rootList    := none
  extraArgs   := pure #[]
}
```

No changes to `Folder.lean`, `Runner.lean`, or any other module.

### TSV Contract

All `listCmd` functions must return the same TSV schema:

```
name\tsize\tdate\ttype
..\t0\t\td                    ← optional parent entry
file1.csv\t1024\t2024-01-05 12:00:00\tf
subdir\t0\t\td
```

- **type** column: `d` = directory, `f` or ` ` = file, `s` = symlink
- **name** column: relative to current path (no leading `/`)
- **size**: bytes (0 for directories)
- **date**: ISO-ish format or empty

### loadSql patterns

| Pattern | When to use | Example |
|---------|------------|---------|
| Direct read | DuckDB can read the format natively | `SELECT * FROM '{path}'` |
| httpfs URL | Remote URL DuckDB can fetch | `SELECT * FROM 'hf://datasets/...'` |
| read_json_auto | JSON data piped through tmpfile | `SELECT * FROM read_json_auto('{tmp}')` |
| read_csv_auto | CSV/TSV via tmpfile | `SELECT * FROM read_csv_auto('{tmp}')` |
| ATTACH + query | External DB file | `ATTACH '{path}'; SELECT * FROM ...` |

### Implementation Plan

1. **Define `SourceConfig`** in a new `Tc/SourceConfig.lean`
2. **Move per-source logic** from `S3.lean`, `HF.lean`, `Osquery.lean` into config constructors
   - Keep the modules for helper functions (parsing, etc.)
   - Export a `config : SourceConfig` from each
3. **Registry** in `SourceConfig.lean`: `sourceConfigs` array
4. **Refactor `Folder.lean`**: replace `Backend` struct + `backend?` with `findConfig`
5. **Test**: existing test suite should pass unchanged (behavior is identical)

### What Stays The Same

- `Remote.lean` — generic URI ops, still used by configs
- `Types.lean`, `Nav.lean`, `View.lean` — untouched
- `Data/ADBC/Table.lean` — still handles `fromTsv`, `fromUrl`, etc.
- TSV schema — already standardized across S3/HF/local
- The rest of the app sees `SourceConfig` functions, not raw S3/HF calls

### Migration Path

Phase 1: Add `SourceConfig` + registry alongside existing code (both paths work)
Phase 2: Switch `Folder.lean` to use configs
Phase 3: Remove old `Backend` struct
