# Tc Design

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  TblOps α                 ModifyTable α [TblOps α]      │
│    nRows, colNames          excludeCols, sortBy         │
│    queryMeta, filter                                    │
│    distinct, findRow, render                            │
│    getCols, colType, plotExport, fetchMore              │
│    fromFile, fromUrl                                    │
└───────────────────────────┬─────────────────────────────┘
                            │ instance (AdbcTable)
                            ▼
┌─────────────────────────────────────────────────────────┐
│  View + ViewStack (View.lean)                           │
│    View: wraps NavState, exposes update/doRender        │
│    ViewStack: cur + parents, push/pop/swap/dup          │
└───────────────────────────┬─────────────────────────────┘
                            │ composed by
                            ▼
┌─────────────────────────────────────────────────────────┐
│  AppState (App/Common.lean)                             │
│    stk : ViewStack, vs : ViewState                      │
│    theme : Theme.State, info : UI.Info.State            │
│    dispatch : CmdInfo → IO Action                       │
└───────────────────────────┬─────────────────────────────┘
                            │ key → handler → dispatch
                            ▼
┌─────────────────────────────────────────────────────────┐
│  appMain (App/Common.lean)                              │
│    CmdConfig.keyLookup ∪ evToHandler → dispatch         │
└─────────────────────────────────────────────────────────┘
```

## Pure/IO Separation

Most dispatch is IO (calls domain modules directly). A few pure functions
return residual `Effect` values for cases where pure code needs IO:

```
┌─────────────────────────────────────────────────────────┐
│  Pure: View.update, ViewStack.update, Freq.update       │
│    Return (State × Effect) — caller executes Effect     │
│                                                         │
│  IO: dispatch calls domain modules directly             │
│    Folder.dispatch, Meta.dispatch, Filter.dispatch,     │
│    Plot.run, Transpose.push, Diff.run, etc.             │
└─────────────────────────────────────────────────────────┘
```

## Classes

| Class       | Methods                              | Purpose                    |
|-------------|--------------------------------------|----------------------------|
| TblOps      | nRows, colNames, totalRows           | Unified table interface    |
|             | queryMeta, filter, distinct, findRow | Query operations           |
|             | render, getCols, colType             | Render + column access     |
|             | plotExport, fetchMore                | Plot export + pagination   |
|             | fromFile, fromUrl                    | Load from path/URL         |
| ModifyTable | excludeCols, sortBy                   | Table mutations            |

## Backend

```
┌─────────────────────────────────────────────────────────┐
│  AdbcTable                                               │
│  DuckDB via ADBC/C FFI                                   │
│  PRQL → SQL queries (prqlc compile -t sql.duckdb)        │
│  nanoarrow typed column accessors                        │
│  parquet, CSV, JSON, DuckDB files, S3, HF, osquery       │
└─────────────────────────────────────────────────────────┘
```

## Remote Browsing

All remote sources are config-driven via `SourceConfig.sources` (inline Lean array).
Adding a source = add an entry to the array with prefix, list/download commands, and SQL transforms.

```
┌─────────────────────────────────────────────────────────┐
│  SourceConfig.lean (config-driven source routing)       │
│    sources : Array Config — inline config               │
│    findSource : longest prefix match                    │
│    runList, runDownload, runEnter, runSetup             │
├──────────────┬──────────────────┬───────────────────────┤
│  s3://       │  hf://           │  osquery://            │
│  aws CLI     │  HF Hub API      │  osqueryi --json       │
│  +n = public │  curl            │  python3 metadata      │
│  buckets     │  disk cache      │  stub views            │
├──────────────┼──────────────────┼───────────────────────┤
│  ftp://      │  rest://         │  pg://                 │
│  curl ls -l  │  curl JSON       │  ATTACH POSTGRES       │
└──────────────┴──────────────────┴───────────────────────┘
```

## Key → Handler Mapping

Two sources, checked in order by mainLoop:

1. **`CmdConfig.keyLookup`**: physical key → handler name (from inline `commands` array, cached as HashMap)
2. **`evToHandler`**: terminal special keys (Enter, Backspace, Shift+Arrow, arrows, PgUp/Dn, Ctrl-D/U, hjkl nav)

Socket commands send handler names directly (e.g. `"sort.asc"`, `"heat.3"`).

## Residual Effect (Cmd.lean)

Minimal — only for pure code that can't do IO:

```lean
inductive Effect where
  | none | quit | fetchMore
  | sort (colIdx : Nat) (sels : Array Nat) (grp : Array Nat) (asc : Bool)
  | exclude (cols : Array String)
  | freq (colNames : Array String)
  | freqFilter (cols : Array String) (row : Nat)
```

Most effects were eliminated by having `dispatch` call IO directly.

## Plot (Plot.lean)

Interactive plot with interval control:

```
┌─────────────────────────────────────────────────────────┐
│  Plot.run                                               │
│  1. Determine x/y columns from group + cursor           │
│  2. Detect x-axis type (time/timestamp/date/str/num)    │
│  3. Term.shutdown                                       │
│  4. Loop:                                               │
│     export data → Rscript (ggplot2) → PNG → viu         │
│     show interval selector: [1d] 1M 1Y                  │
│     read key: +/- → change interval, else → exit        │
│  5. Term.init                                           │
└─────────────────────────────────────────────────────────┘
```

**Downsampling strategies:**
- Time-like x-axis: `ds_trunc` (SUBSTRING truncation to interval length)
- Non-time x-axis: hand-written SQL with `ROW_NUMBER() OVER ()` sampling

## Structures

| Struct       | Purpose                                      |
|--------------|----------------------------------------------|
| CmdInfo      | Handler name + resetsVS (key/handler lookup) |
| Entry        | Command config entry (handler, key, label)   |
| Effect       | Residual IO descriptor (7 variants)          |
| NavState     | Table + row/col cursors + selections + group |
| NavAxis      | Generic axis: cur (Fin n) + sels (Array)     |
| View         | Existential wrapper hiding table type        |
| ViewKind     | View type: tbl, colMeta, freqV, fld path depth |
| ViewStack    | Non-empty stack of Views (cur + parents)     |
| ViewState    | Scroll offsets for rendering                 |
| AppState     | Top-level: stk + vs + theme + info           |
| Action       | Dispatch result: quit, unhandled, ok state   |

## Testing (Test.lean)

Tests use headless `-c` mode with `IO.Process.output` (no tmux dependency):

```
┌─────────────────────────────────────────────────────────┐
│  Test Harness                                           │
│  1. IO.Process.output { cmd := bin, args := #[file, "-c", keys] } │
│  2. -c injects keystrokes, headless C shim provides 80×24 buffer  │
│  3. On key exhaustion, mainLoop prints buffer to stdout and exits  │
│  4. Tests compare stdout against expected strings                  │
└─────────────────────────────────────────────────────────┘
```

### Key Notation

| Test notation | Mapped to |
|---------------|-----------|
| `<ret>`       | Enter (`\r`) |
| `<esc>`       | Escape (`\x1b`) |
| `<C-d>`       | Ctrl-D (`\x04`) |
| `<C-u>`       | Ctrl-U (`\x15`) |
| `<backslash>` | `\` |
| `abc`         | literal chars |

Do NOT use `$` in key strings (shell interprets it). Do NOT include `Q` (causes quit before buffer read).

### Render Performance

Column width computation scans visible rows only (`r0` to `r1`), not all rows.
This prevents CPU burn on large tables (e.g., 300M row parquet).

## Module Dependency Graph

```
LAYER 1: FOUNDATION
┌─────────────────────────────────────────────────────────────────┐
│ Types (Cell,Column,TblOps,ModifyTable,Agg,Op)                  │
│ Cmd (Effect,PlotKind,ExportFmt)   Util (Log,Socket,TmpDir,Remote) │
│ Term                                                            │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 2: DATA
┌─────────────────────────────────────────────────────────────────┐
│ Data/ADBC/Table ──→ Data/ADBC/Ops                              │
│              └──→ Data/ADBC/Prql (+ funcs.prql)                │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 3: VIEW
┌─────────────────────────────────────────────────────────────────┐
│ Nav (clamp,adjOff) ──→ Render ──→ Key                          │
│                    └──→ View (View + ViewStack)                │
│ CmdConfig (inline commands array, key/handler HashMaps)        │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 4: FEATURES
┌─────────────────────────────────────────────────────────────────┐
│ Meta  Filter  Folder  Theme  Fzf  Plot  UI/Info                │
│ SourceConfig (inline sources array)                             │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 5: STATE & DISPATCH
┌─────────────────────────────────────────────────────────────────┐
│ App/Common (AppState, dispatch → IO Action)                    │
│ Runner (Freq logic only)                                       │
└─────────────────────────────────────────────────────────────────┘

TESTS
┌─────────────────────────────────────────────────────────────────┐
│ test/Test.lean (headless -c integration tests)                 │
│ test/TestPure.lean (pure theorems + #guard tests)              │
│ test/TestScreen.lean (screen buffer tests)                     │
└─────────────────────────────────────────────────────────────────┘
```

### Build

| Executable | Use case |
|------------|----------|
| `tv`       | Full (CSV, parquet, JSON, DuckDB, S3, HF, osquery) |
| `test`     | Integration tests (headless `-c` mode) |

Generic code (View, Meta, etc.) uses typeclasses (`TblOps`, `ModifyTable`).
