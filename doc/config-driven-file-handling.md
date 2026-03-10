# Config-Driven File Handling

## Problem

S3, HF, Osquery each have hardcoded logic in separate `.lean` modules.
Adding a new source requires a new module + if/else wiring in `Folder.lean`.

## Solution

A single **config table** in `Tc/SourceConfig.lean`. Each row is a source.
Columns define: how to list (CLI → JSON → DuckDB), download, and navigate.

## Config Table

```
prefix          │ minParts │ listCmd                              │ listFormat │ listSql                                              │ downloadCmd                          │ needsDownload │ dirSuffix │ parentFallback
────────────────┼──────────┼──────────────────────────────────────┼────────────┼──────────────────────────────────────────────────────┼──────────────────────────────────────┼───────────────┼───────────┼───────────────
s3://           │ 3        │ aws s3 ls {path} {extra}             │ s3Text     │ (none)                                               │ aws s3 cp {extra} {path} {tmp}/{name}│ true          │ true      │ (none)
hf://datasets/  │ 5        │ curl -sf https://.../datasets/{1}/{2}│ json       │ SELECT split_part(path,'/',-1) as name, size, type   │ curl -sfL -o {tmp}/{name} https://...│ false         │ true      │ hf://
                │          │   /tree/main/{3+}                    │            │   FROM read_json_auto('{src}')                       │   .../datasets/{1}/{2}/resolve/...   │               │           │
```

## Flow

```
CLI cmd → stdout (JSON or text)
  → [s3Text: convert to JSON in Lean]
  → save to tmp file
  → read_json_auto(tmpfile) [or listSql transform]
  → DuckDB temp table
  → AdbcTable → folder view
```

## Template Placeholders

| Placeholder | Meaning |
|------------|---------|
| `{path}`   | Full URI path (with trailing `/` for listing) |
| `{name}`   | Last path component (filename) |
| `{tmp}`    | Temp directory path |
| `{extra}`  | Runtime extra args (e.g. `--no-sign-request` for S3) |
| `{1}`..`{9}` | Path components after stripping prefix (1-indexed) |
| `{N+}`     | Parts N onward joined by `/` |
| `{src}`    | JSON temp file path (in listSql only) |

## Adding a New Source

Add a row to `sources` in `SourceConfig.lean`:

```lean
def gcsCfg : Config where
  prefix         := "gs://"
  minParts       := 3
  listCmd        := "gsutil ls -l -json {path}"
  listFormat     := .json
  listSql        := ""
  downloadCmd    := "gsutil cp {path} {tmp}/{name}"
  needsDownload  := true
  dirSuffix      := true
  parentFallback := ""

def sources : Array Config := #[s3Cfg, hfCfg, gcsCfg]
```

No changes to `Folder.lean`, `Runner.lean`, or any other module.

## Files Changed

- **`Tc/SourceConfig.lean`** (new): `Config` structure, config table, template expansion, generic `runList`/`runDownload`/`resolve`
- **`Tc/Folder.lean`**: Replaced `Backend`/`backend?` with `sourceConfig?` → `SourceConfig.findSource`
- **`Tc/App/Common.lean`**: `S3.setNoSign` → `SourceConfig.setNoSign`
- **`lakefile.lean`**: Added `Tc.SourceConfig` to roots

## Special Cases (unchanged)

| Source | Special behavior | Handled by |
|--------|-----------------|------------|
| HF root (`hf://`) | Dataset browser from pre-populated DB | `HF.isRoot` + `HF.listAll` |
| Osquery | Table listing + safe/dangerous enter | `Osquery.list` + `Osquery.enterTable` |
| DuckDB files | Attach + table browser | `Folder.openFile` |
| Local filesystem | `find` command + TSV | `Folder.listDir` (default fallback) |
