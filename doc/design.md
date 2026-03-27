# Tc Design

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  TblOps α                 ModifyTable α [TblOps α]      │
│    nRows, colNames          delCols, sortBy             │
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
│    update chains: theme → info → stk → fld → meta → ...│
└───────────────────────────┬─────────────────────────────┘
                            │ returns Effect
                            ▼
┌─────────────────────────────────────────────────────────┐
│  Effect (Cmd.lean)                                      │
│    none | quit | fzf _ | query _ | folder _ | ...      │
└───────────────────────────┬─────────────────────────────┘
                            │ interpreted by
                            ▼
┌─────────────────────────────────────────────────────────┐
│  Runner (Runner.lean)                                   │
│    runStackEffect : ViewStack AdbcTable → Effect        │
│                   → IO (ViewStack AdbcTable)            │
└───────────────────────────┬─────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────┐
│  appMain (App/Common.lean)                              │
│    lookup KeyMap.char ∪ evToCmd → update → runEffect    │
└─────────────────────────────────────────────────────────┘
```

## Pure/IO Separation

The architecture separates pure state logic from IO effects:

```
┌─────────────────────────────────────────────────────────┐
│                   Pure State Machine                     │
│  update : AppState → Cmd → Option (AppState × Effect)   │
│  - Nav.update (cursor, selection, group)                │
│  - Theme.update (returns Effect.themeLoad)              │
│  - Info.update (toggle visibility)                      │
│  - View.update (prec/width, returns query effects)      │
│  - Filter.update (returns fzf effects)                  │
│  - Meta.update (returns query effects)                  │
│  - Folder.update (returns folder effects)               │
│  - Plot.update (returns plotLine/plotBar effects)       │
└─────────────────────────────────────────────────────────┘
                            │ Effect
                            ▼
┌─────────────────────────────────────────────────────────┐
│                   IO Effect Runner                       │
│  runEffect : AppState → Effect → IO AppState            │
│  runStackEffect : ViewStack → Effect → IO ViewStack     │
│  - fzf spawning (Fzf.lean)                              │
│  - database queries (QueryTable)                        │
│  - filesystem operations (Folder.lean)                  │
│  - plot rendering (Plot.lean)                           │
│  - terminal rendering (Term.lean)                       │
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
| ModifyTable | delCols, sortBy                      | Table mutations            |
| Update      | update                               | Pure: Cmd → (State, Effect)|

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

```
┌─────────────────────────────────────────────────────────┐
│  Remote.lean (shared URI path operations)               │
│    join, parent, dispName                               │
├──────────────┬──────────────────┬───────────────────────┤
│  S3.lean     │  HF.lean         │  Osquery.lean          │
│  s3:// via   │  hf:// via HF    │  osquery:// via        │
│  aws CLI     │  Hub API         │  osqueryi --json       │
│  +n = public │  curl + jq       │  python3 metadata      │
│  buckets     │  disk cache      │  row count cache       │
└──────────────┴──────────────────┴───────────────────────┘
```

## Key → Cmd Mapping

Three sources, checked in order by `evToCmd` + main loop:

1. **`evToCmd`**: terminal special keys (Enter, Backspace, Shift+Arrow, arrows, PageUp/Down, Home/End, Ctrl-D/U)
2. **`KeyMap.char`**: single-key shortcuts (the single source of truth for all one-key mappings)
3. **`objMenu`+`verbsFor`**: space → fzf object picker → verb picker

Key = single-key shortcut. Name = implemented via space menu / `-c` code only.

```
Verb │ r:row      │ c:col      │ s:stk    │ i:info   │ M:metaV  │ F:freq   │ D:fld    │ desc
─────┼────────────┼────────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────────────────────────
  ~  │ T togRow   │ ! group    │ swap     │ togInfo  │ ⏎* setKey│ ⏎* filt  │ ⏎* enter │ M~:set key cols from selected  F~:filter parent table by current row
  <  │ k up       │ h left     │ q pop    │ precDec  │          │          │ depth--  │
  >  │ j down     │ l right    │ dup      │ precInc  │          │          │ depth++  │ s>:clone current view
  [  │ ^U pgUp    │ [ sortAsc  │ joinLeft │ { scrUp  │          │          │          │ i[:scroll cell preview up
  ]  │ ^D pgDn    │ ] sortDesc │ joinRigh │ } scrDn  │          │          │          │ i]:scroll cell preview down
  {  │ Home top   │ first      │ quit     │ 0dp      │          │          │ ⌫ parent │
  }  │ End bot    │ last       │ inner    │ 17dp max │          │          │          │
  -  │ N prevMat  │ S-← shift  │ setDiff  │          │          │          │ trash    │
  +  │ n nextMat  │ S-→ shift  │ union    │          │ open     │ open     │          │ M+:column metadata  F+:value frequency counts
  /  │ / search   │ search     │ SPC menu │          │          │          │          │ c/:jump to col by name
  \  │ \ filter   │ hide       │          │          │          │          │          │ r\:PRQL filter expr
  :  │            │ : split    │          │          │          │          │          │
  =  │            │ = derive   │          │          │          │          │          │ c=:name = expr
  0  │            │ area       │          │ heat off │ selNull  │          │          │ M0:select null cols
  1  │            │ line       │ xpose    │ heat num │ selSing  │          │          │ M1:select single-val cols  s1:rows↔cols
  2  │            │ scat       │ diff     │ heat cat │          │          │          │ s2:compare top 2 views
  3  │            │ bar        │          │ heat all │          │          │          │
  4  │            │ box        │          │          │          │          │          │
  5  │            │ step       │          │          │          │          │          │
  6  │            │ hist       │          │          │          │          │          │
  7  │            │ dens       │          │          │          │          │          │
  8  │            │ violin     │          │          │          │          │          │

* ⏎ context-sensitive: freq→filter, meta→enter, fld→enter, tbl→none
```

## Effect DSL (Cmd.lean)

Effect describes IO operations without executing them. Sub-effects are grouped by domain:

```lean
inductive FzfEffect where | cmd | col | row | filter
inductive QueryEffect where
  | colMeta | freq (colNames : Array String)
  | freqFilter (cols : Array String) (row : Nat)
  | filter (expr : String)
  | sort (colIdx : Nat) (sels : Array Nat) (grp : Array Nat) (asc : Bool)
inductive FolderEffect where | push | enter | del | parent | depth (delta : Int)
inductive SearchEffect where | next | prev
inductive PlotKind where | line | bar | scatter | hist | box | area | density | step | violin
inductive MetaEffect where | selNull | selSingle | setKey
inductive ClipEffect where | cell | row | col
inductive ExportFmt where | csv | parquet | json | ndjson

inductive Effect where
  | none | quit
  | fzf : FzfEffect → Effect
  | query : QueryEffect → Effect
  | folder : FolderEffect → Effect
  | search : SearchEffect → Effect
  | plot : PlotKind → Effect
  | colMeta : MetaEffect → Effect
  | clip : ClipEffect → Effect
  | themeLoad (delta : Int)
  | fetchMore
  | export : ExportFmt → Effect
  | sessionSave | sessionLoad | join
  | transpose | diff
```

**Functor pattern**: `update` maps `Cmd → Effect`:

```lean
class Update (α : Type) where
  update : α → Cmd → Option (α × Effect)
```

## Plot (Plot.lean)

Interactive plot with interval control. After display, `+`/`-` cycles intervals:

```
┌─────────────────────────────────────────────────────────┐
│  Plot.run                                               │
│  1. Determine x/y columns from group + cursor           │
│  2. Detect x-axis type (time/timestamp/date/str/num)    │
│  3. Infer date/time from string values if needed         │
│  4. Term.shutdown                                        │
│  5. Loop:                                                │
│     export data (DB-side or Lean fallback)              │
│     Rscript (ggplot2) → PNG → viu                       │
│     show interval selector: [1d] 1M 1Y                  │
│     read key: +/- → change interval, else → exit        │
│  6. Term.init                                            │
└─────────────────────────────────────────────────────────┘
```

**Downsampling strategies:**
- Time-like x-axis: `ds_trunc` (SUBSTRING truncation to interval length)
- Non-time x-axis: hand-written SQL with `ROW_NUMBER() OVER ()` sampling

**Interval types:**

| Column type | Intervals | Method |
|-------------|-----------|--------|
| time (HH:MM:SS) | 1s, 1m, 1h | SUBSTRING len 8/5/2 |
| timestamp | 1s, 1m, 1h, 1d | SUBSTRING len 19/16/13/10 |
| date (YYYY-MM-DD) | 1d, 1M, 1Y | SUBSTRING len 10/7/4 |
| numeric/string | 1x, 2x, 4x, ... | every-Nth-row sampling |

## Structures

| Struct       | Purpose                                      |
|--------------|----------------------------------------------|
| Verb         | Action type: inc/dec/ent/del/dup/up/val (7 verbs) |
| Cmd          | Object + Verb command pattern (15 objects)   |
| Effect       | IO operation descriptor (30+ variants)       |
| NavState     | Table + row/col cursors + selections + group |
| NavAxis      | Generic axis: cur (Fin n) + sels (Array)     |
| View         | Existential wrapper hiding table type        |
| ViewKind     | View type: tbl, colMeta, freqV, fld path depth |
| ViewStack    | Non-empty stack of Views (cur + parents)     |
| ViewState    | Scroll offsets for rendering                 |
| AppState     | Top-level: stk + vs + theme + info           |
| Theme.State  | Theme styles + index                         |
| Info.State   | Info overlay visibility                      |
| Interval     | Plot downsample: label, truncLen, timefmt    |

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
│ Cmd (Verb,Cmd,Effect,Update)   Error   Term   TmpDir           │
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
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 4: FEATURES
┌─────────────────────────────────────────────────────────────────┐
│ Meta  Filter  Folder  Theme  Fzf  Plot  UI/Info                │
│ Remote  S3  HF  Osquery                                        │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 5: STATE & DISPATCH
┌─────────────────────────────────────────────────────────────────┐
│ App/Common (AppState, update) ──→ Runner (runStackEffect)      │
└─────────────────────────────────────────────────────────────────┘

TESTS
┌─────────────────────────────────────────────────────────────────┐
│ test/Test.lean (headless -c integration tests)                 │
│ test/TestPure.lean (pure #guard tests)                         │
│ test/TestScreen.lean (screen buffer tests)                     │
└─────────────────────────────────────────────────────────────────┘
```

### Build

| Executable | Use case |
|------------|----------|
| `tv`       | Full (CSV, parquet, JSON, DuckDB, S3, HF, osquery) |
| `test`     | Integration tests (headless `-c` mode) |

Generic code (View, Meta, etc.) uses typeclasses (`TblOps`, `ModifyTable`).
