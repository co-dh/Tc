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

## Cmd System (Cmd.lean)

### Verb (7 actions)

| Verb   | Char | Meaning                       |
|--------|------|-------------------------------|
| inc    | >    | increment, forward, next      |
| dec    | <    | decrement, backward, prev     |
| ent    | ~    | enter, toggle                 |
| del    | d    | delete, destroy               |
| dup    | c    | copy, push, create            |
| up     | ^    | go up / parent                |
| val n  | 0-9  | direct value selection         |

### Cmd Objects (22 objects)

| Obj      | Char | Purpose                          |
|----------|------|----------------------------------|
| row      | r    | row cursor                       |
| col      | c    | column cursor, c=fzf cmd menu    |
| vPage    | v    | vertical page scroll             |
| hPage    | h    | horizontal page scroll           |
| ver      | V    | vertical end (top/bottom)        |
| hor      | H    | horizontal end (first/last col)  |
| rowSel   | R    | row selection/search/filter      |
| colSel   | C    | column selection/sort/hide       |
| grp      | g    | group (key columns)              |
| stk      | s    | view stack: pop/swap/dup/quit/xpose/diff |
| prec     | p    | decimal precision                |
| width    | w    | column width                     |
| thm      | T    | theme                            |
| info     | i    | info overlay                     |
| metaV    | M    | meta view (0=selNull, 1=selSingle) |
| freq     | F    | frequency view                   |
| fld      | D    | folder view                      |
| plot     | P    | R/ggplot2 chart                  |
| colShift | K    | reorder key columns              |
| heat     | m    | heatmap mode (0-3)               |
| yank     | y    | copy to clipboard                |
| prev     | B    | preview scroll ({/} keys)        |

### Isomorphism

```lean
theorem parse_toString (c : Cmd) : Parse.parse? (toString c) = some c
theorem ofChar_toChar (v : Verb) : Verb.ofChar? (Verb.toChar v) = some v
```

## Obj/Verb Matrix (Key.lean)

`KeyMap.char` is the single source of truth for all one-key shortcuts.
All entries are `Cmd` (obj+verb). Command mode: `space` → fzf object picker → verb picker.

```
                 │ DEC │ INC │ ENT │ DEL │ DUP │ UP  │ VAL │
                 │  <  │  >  │  ~  │  d  │  c  │  ^  │ 0-9 │
Char │ Obj       │     │     │     │     │     │     │     │ Description
─────┴───────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────
 --- Navigation (direct keys, via evToCmd) ---
 r   │ row       │  k  │  j  │     │     │     │     │     │ cursor up/down
 c   │ col       │  h  │  l  │  s  │     │ SPC │     │     │ cursor, s=fzf, SPC=cmd menu
 v   │ vPage     │ ^U  │ ^D  │     │     │     │     │     │ page (also JK)
 h   │ hPage     │     │     │     │     │     │     │     │ horizontal page
 V   │ ver       │Home │End  │     │     │     │     │     │ top/bottom
 H   │ hor       │  ←  │  →  │     │     │     │     │     │ first/last col
 --- Selection ---
 R   │ rowSel    │  \  │  /  │  T  │     │     │     │     │ \=filter, /=search, T=toggle
 C   │ colSel    │  ]  │  [  │  t  │     │     │     │     │ sort/toggle (hide via Ch)
 g   │ grp       │  N  │  n  │  !  │     │     │     │     │ prev/next/toggle
 --- Options ---
 s   │ stk       │  q  │     │  S  │  Q  │     │  X  │ V=0 │ q=pop S=swap Q=quit X=xpose V=diff
 p   │ prec      │     │     │     │     │     │     │     │ (space p </>)
 w   │ width     │     │     │     │     │     │     │     │ (space w </>)
 T   │ thm       │     │     │     │     │     │     │     │ (space T </>)
 i   │ info      │     │     │  I  │     │     │     │     │ I=toggle overlay
 B   │ prev      │  {  │  }  │     │     │     │     │     │ preview scroll
 --- Views ---
 M   │ metaV     │     │     │ ⏎   │     │  M  │     │0  1 │ M=push, 0=null, 1=single
 F   │ freq      │     │     │ ⏎   │     │  F  │     │     │ ⏎=filter by row, F=push
 D   │ fld       │     │     │ ⏎   │  d  │  D  │     │     │ ⏎=enter, d=trash, D=push
 P   │ plot      │     │     │     │     │     │     │     │ (space P for type selection)
 K   │ colShift  │S-←  │S-→  │     │     │     │     │     │ reorder key cols
 m   │ heat      │     │     │     │     │     │     │0-3  │ heatmap mode (0=off)
 y   │ yank      │     │  >  │  ~  │     │     │     │     │ ~=cell, >=row, <=col
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
| Cmd          | Object + Verb command pattern (22 objects)   |
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
