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
├─────────────────────────────────────────────────────────┤
│  ExecOp α                                               │
│    exec : Op → IO α                                     │
└───────────────────────────┬─────────────────────────────┘
                            │ instance
                            ▼
┌─────────────────────────────────────────────────────────┐
│  Table = AdbcTable | KdbTable  (closed sum type)        │
│    lift/liftM/liftW/liftIO combinators                  │
└───────────────────────────┬─────────────────────────────┘
                            │ wrapped by
                            ▼
┌─────────────────────────────────────────────────────────┐
│  View + ViewStack (View.lean)                           │
│    View: wraps NavState, exposes update/doRender        │
│    ViewStack: cur + parents, push/pop/swap/dup          │
└───────────────────────────┬─────────────────────────────┘
                            │ composed by
                            ▼
┌─────────────────────────────────────────────────────────┐
│  AppState (Dispatch.lean)                               │
│    stk : ViewStack, vs : ViewState                      │
│    theme : Theme.State, info : UI.Info.State            │
│    update chains: theme → info → stk → fld → meta → ...│
└───────────────────────────┬─────────────────────────────┘
                            │ returns Effect
                            ▼
┌─────────────────────────────────────────────────────────┐
│  Effect (Cmd.lean)                                      │
│    none | quit | fzf* | query* | folder* | theme* | ...│
└───────────────────────────┬─────────────────────────────┘
                            │ interpreted by
                            ▼
┌─────────────────────────────────────────────────────────┐
│  Runner (Runner.lean)                                   │
│    runStackEffect : ViewStack → Effect → IO ViewStack   │
└───────────────────────────┬─────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────┐
│  appMain (App/Common.lean)                              │
│    evToCmd → AppState.update → runEffect → render       │
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
│  - Meta/Freq.update (returns query effects)             │
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
| ExecOp      | exec                                 | IO: Op → IO Table          |

## Backends

```
┌─────────────────────────────────────────────────────────┐
│  Table = AdbcTable | KdbTable                           │
└───────────────────────────┬─────────────────────────────┘
                            │
          ┌─────────────────┴─────────────────┐
          ▼                                   ▼
┌─────────────────────┐             ┌─────────────────────┐
│ AdbcTable           │             │ KdbTable             │
│ DuckDB via ADBC/C   │             │ kdb+ via C FFI       │
│ PRQL → SQL queries  │             │ q expressions        │
│ nanoarrow types     │             │ char type codes      │
│ parquet, CSV, S3,   │             │ remote kdb server    │
│ HF datasets         │             │                      │
└─────────────────────┘             └─────────────────────┘
```

| Backend | Query Language | File Formats | Type System |
|---------|---------------|--------------|-------------|
| ADBC    | PRQL → SQL    | parquet, CSV, S3, HF | nanoarrow (string names) |
| Kdb     | q             | kdb server   | char codes (j, f, s, ...) |

## Remote Browsing

```
┌─────────────────────────────────────────────────────────┐
│  Remote.lean (shared URI path operations)               │
│    join, parent, dispName                               │
├─────────────────────────────────────────────────────────┤
│  S3.lean              │  HF.lean                        │
│  s3:// via aws CLI    │  hf:// via HF Hub API           │
│  --no-sign-request    │  curl + jq for listing          │
│  for public buckets   │  ~/.cache/tc/hf/ disk cache     │
└─────────────────────────────────────────────────────────┘
```

## Cmd System (Cmd.lean)

### Verb (5 actions)

| Verb | Char | Meaning                       |
|------|------|-------------------------------|
| inc  | +    | increment, forward, next      |
| dec  | -    | decrement, backward, prev     |
| ent  | ~    | enter, toggle                 |
| del  | d    | delete                        |
| dup  | c    | copy, push, create            |

### Cmd Objects (18 objects)

| Obj    | Char | Purpose                          |
|--------|------|----------------------------------|
| row    | r    | row cursor                       |
| col    | c    | column cursor                    |
| vPage  | v    | vertical page scroll             |
| hPage  | h    | horizontal page scroll           |
| ver    | V    | vertical end (top/bottom)        |
| hor    | H    | horizontal end (first/last col)  |
| rowSel | R    | row selection/search/filter      |
| colSel | C    | column selection/sort/delete     |
| grp    | g    | group (key columns)              |
| stk    | s    | view stack operations            |
| prec   | p    | decimal precision                |
| width  | w    | column width                     |
| thm    | T    | theme                            |
| info   | i    | info overlay                     |
| metaV  | M    | meta view                        |
| freq   | F    | frequency view                   |
| fld    | D    | folder view                      |
| plot   | P    | gnuplot chart (line/bar)         |

### Isomorphism

```lean
theorem parse_toString (c : Cmd) : Parse.parse? (toString c) = some c
theorem ofChar_toChar (v : Verb) : Verb.ofChar? (Verb.toChar v) = some v
```

## Obj/Verb Matrix (Key.lean)

Direct key bindings and command mode (`space` + obj + verb):

```
                 │ DEC │ INC │ ENT │ DEL │ DUP │
                 │  -  │  +  │  ~  │  d  │  c  │
Char │ Obj       │  ,  │  .  │     │     │     │ Description
─────┴───────────┴─────┴─────┴─────┴─────┴─────┴──────────────────
 --- Navigation (direct keys) ---
 r   │ row       │  k  │  j  │     │     │     │ Row cursor up/down
 c   │ col       │  h  │  l  │  s  │     │     │ Col cursor, s=fzf jump
 v   │ vPage     │ ^U  │ ^D  │     │     │     │ Vertical page (also JK)
 h   │ hPage     │     │     │     │     │     │ Horizontal page
 V   │ ver       │Home │End  │     │     │     │ Top/bottom
 H   │ hor       │  ←  │  →  │     │     │     │ First/last col
 --- Selection ---
 R   │ rowSel    │  \  │  /  │  T  │     │     │ \=filter, /=search, T=toggle
 C   │ colSel    │  ]  │  [  │  t  │  d  │     │ ]=sortDesc, [=sortAsc, t=toggle
 g   │ grp       │  N  │  n  │  !  │     │     │ N=prev, n=next, !=toggle
 --- Options ---
 s   │ stk       │  q  │     │  S  │     │     │ q=pop, S=swap
 p   │ prec      │     │     │     │     │     │ (space p ,/.)
 w   │ width     │     │     │     │     │     │ (space w ,/.)
 T   │ thm       │     │     │     │     │     │ (space T ,/.)
 i   │ info      │     │     │  I  │     │     │ I=toggle overlay
 --- Views ---
 M   │ metaV     │  0  │  1  │ ⏎   │     │  M  │ 0=selNull, 1=selSingle, M=push
 F   │ freq      │     │     │ ⏎   │     │  F  │ ⏎=filter by row, F=push
 D   │ fld       │     │     │ ⏎   │  d  │  D  │ ⏎=enter, d=trash, D=push
 P   │ plot      │     │  .  │     │     │     │ .=line chart (space P ,=bar)
```

**Command mode**: Press `space` to open fzf object picker, then fzf verb picker.

## Effect DSL (Cmd.lean)

Effect describes IO operations without executing them:

```lean
inductive Effect where
  | none | quit
  -- fzf (user selection)
  | fzfCmd | fzfCol
  | fzfRow (colIdx : Nat) (colName : String)
  | fzfFilter (colIdx : Nat) (colName : String)
  -- query (database/table ops)
  | queryMeta | queryFreq (colNames : Array String)
  | freqFilter (cols : Array String) (row : Nat)
  | queryFilter (expr : String)
  | querySort (colIdx : Nat) (sels : Array Nat) (grp : Array Nat) (asc : Bool)
  | queryDel (colIdx : Nat) (sels : Array Nat) (grp : Array String)
  -- folder (filesystem)
  | folderPush | folderEnter | folderDel
  | folderDepth (delta : Int)
  -- search
  | findNext | findPrev
  -- theme
  | themeLoad (delta : Int)
  -- plot
  | plotLine | plotBar
  -- misc
  | fetchMore | metaSelNull | metaSelSingle | metaSetKey
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
│     gnuplot → PNG → viu                                 │
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
| Verb         | Action type: inc/dec/ent/del/dup (5 verbs)   |
| Cmd          | Object + Verb command pattern (18 objects)   |
| Effect       | IO operation descriptor (24 variants)        |
| NavState     | Table + row/col cursors + selections + group |
| NavAxis      | Generic axis: cur (Fin n) + sels (Array)     |
| View         | Existential wrapper hiding table type        |
| ViewKind     | View type: tbl, colMeta, freqV, fld          |
| ViewStack    | Non-empty stack of Views (cur + parents)     |
| ViewState    | Scroll offsets for rendering                 |
| AppState     | Top-level: stk + vs + theme + info           |
| Theme.State  | Theme styles + index                         |
| Info.State   | Info overlay visibility                      |
| Interval     | Plot downsample: label, truncLen, timefmt    |

## Testing (Test.lean)

Tests use tmux for screen capture instead of custom C FFI:

```
┌─────────────────────────────────────────────────────────┐
│  Test Harness                                           │
│  1. tmux new-session -d -s tctest -e TC_TEST_MODE=1     │
│  2. tmux send-keys (with key notation conversion)       │
│  3. tmux capture-pane -p                                │
│  4. tmux kill-session                                   │
└─────────────────────────────────────────────────────────┘
```

### Key Notation Conversion

| Test notation | tmux send-keys |
|---------------|----------------|
| `<C-d>`       | `C-d`          |
| `<C-u>`       | `C-u`          |
| `<ret>`       | `Enter`        |
| `abc`         | `-l abc`       |

### Test Mode

`TC_TEST_MODE=1` env var enables test mode (Fzf.lean):
- fzf auto-selects first item without spawning
- Folder delete auto-declines confirmation

### Render Performance

Column width computation scans visible rows only (`r0` to `r1`), not all rows.
This prevents CPU burn on large tables (e.g., 300M row parquet).

## Module Dependency Graph

```
LAYER 1: FOUNDATION (35 files)
┌─────────────────────────────────────────────────────────────────┐
│ Types (Cell,Column,TblOps,ModifyTable,Agg,Op,ExecOp)           │
│ Cmd (Verb,Cmd,Effect,Update)   Error   Term                    │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 2: DATA
┌─────────────────────────────────────────────────────────────────┐
│ Data/CSV   Data/Text                                            │
│                                                                 │
│ Data/ADBC/FFI ──→ Data/ADBC/Table ──→ Data/ADBC/{Meta,Ops}     │
│              └──→ Data/ADBC/Prql (+ funcs.prql)                │
│                                                                 │
│ Data/Kdb/FFI ──→ Data/Kdb/Table ──→ Data/Kdb/Ops               │
│             └──→ Data/Kdb/Q                                     │
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
│ Meta  Freq  Filter  Folder  Theme  Fzf  Plot  UI/Info          │
│ Remote  S3  HF  Validity                                       │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 5: TABLE ABSTRACTION
┌─────────────────────────────────────────────────────────────────┐
│ Table.lean ──→ AdbcTable | KdbTable (closed sum + lift)        │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 6: STATE & DISPATCH
┌─────────────────────────────────────────────────────────────────┐
│ Dispatch (AppState, update) ──→ Runner (runEffect)             │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 7: ENTRY POINT
┌─────────────────────────────────────────────────────────────────┐
│ App/Common.lean ──→ appMain                                    │
│ App.lean        ──→ imports Table (tc executable)              │
└─────────────────────────────────────────────────────────────────┘

TESTS
┌─────────────────────────────────────────────────────────────────┐
│ test/Test.lean (tmux-based integration tests)                  │
│ Validity.lean (compile-time #guard tests)                      │
└─────────────────────────────────────────────────────────────────┘
```

### Build

| Executable | Backends | Use case |
|------------|----------|----------|
| `tc`       | ADBC + Kdb | Full (CSV, parquet, S3, HF, kdb) |
| `test`     | ADBC | Integration tests via tmux |

Generic code (View, Meta, Freq, etc.) uses typeclasses (`TblOps`, `ModifyTable`) so it works with any backend wrapped in the `Table` sum type.
