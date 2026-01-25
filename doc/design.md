# Tc Design

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  TblOps α                 ModifyTable α [TblOps α]      │
│    nRows, colNames          delCols, sortBy             │
│    queryMeta, queryFreq                                 │
│    filter, distinct, findRow                            │
│    render, fromFile                                     │
├─────────────────────────────────────────────────────────┤
│  MemConvert M α                                         │
│    wrap : M → α, unwrap : α → Option M                  │
└───────────────────────────┬─────────────────────────────┘
                            │ instance
                            ▼
┌─────────────────────────────────────────────────────────┐
│  Backends: AdbcTable, MemTable, KdbTable                │
└───────────────────────────┬─────────────────────────────┘
                            │ wrapped by
                            ▼
┌─────────────────────────────────────────────────────────┐
│  View (existential wrapper)                             │
│    hides table type, exposes update/doRender            │
├─────────────────────────────────────────────────────────┤
│  ViewStack                                              │
│    cur : View, parents : Array View                     │
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
│  Effect (Effect.lean)                                   │
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
│  App.mainLoop                                           │
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
│  - terminal rendering (Term.lean)                       │
└─────────────────────────────────────────────────────────┘
```

## Classes

| Class       | Methods                              | Purpose                    |
|-------------|--------------------------------------|----------------------------|
| TblOps      | nRows, colNames, totalRows, isAdbc   | Unified table interface    |
|             | queryMeta, queryFreq, filter         | Query operations           |
|             | distinct, findRow, render, fromFile  | Search + render + load     |
| ModifyTable | delCols, sortBy                      | Table mutations            |
| MemConvert  | wrap, unwrap                         | MemTable ↔ T conversion    |
| Update      | update                               | Pure: Cmd → (State, Effect)|
| Exec        | exec                                 | IO: Cmd → IO State (compat)|
| ExecOp      | exec                                 | IO: Op → IO Table          |

## Cmd System (Cmd.lean)

### Verb (5 actions)

| Verb | Char | Meaning                       |
|------|------|-------------------------------|
| inc  | +    | increment, forward, next      |
| dec  | -    | decrement, backward, prev     |
| ent  | ~    | enter, toggle                 |
| del  | d    | delete                        |
| dup  | c    | copy, push, create            |

### Cmd Objects (17 objects)

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
```

**Command mode**: Press `space` to open fzf object picker, then fzf verb picker.

## Effect DSL (Effect.lean)

Effect describes IO operations without executing them:

```lean
inductive Effect where
  | none                                              -- no effect
  | quit                                              -- exit app
  -- fzf (user selection)
  | fzfCmd                                            -- command mode: space
  | fzfCol                                            -- column picker: s
  | fzfRow (colIdx : Nat) (colName : String)          -- row search: /
  | fzfFilter (colIdx : Nat) (colName : String)       -- row filter: \
  -- query (database/table ops)
  | queryMeta                                         -- push meta view: M
  | queryFreq (cols : Array Nat) (colNames : Array String)  -- push freq: F
  | freqFilter (cols : Array String) (row : Nat)      -- filter from freq
  | queryFilter (expr : String)                       -- apply filter expr
  | querySort (colIdx : Nat) (grp : Array Nat) (asc : Bool)  -- sort: [/]
  | queryDel (colIdx : Nat) (sels : Array Nat) (grp : Array String)  -- del: d
  -- folder (filesystem)
  | folderPush                                        -- push folder view: D
  | folderEnter                                       -- enter dir/file: ⏎
  | folderDel                                         -- delete file: d
  | folderDepth (delta : Int)                         -- change find depth
  -- search
  | findNext                                          -- search next: n
  | findPrev                                          -- search prev: N
  -- theme
  | themeLoad (delta : Int)                           -- cycle theme
```

**Functor pattern**: `update` maps `Cmd → Effect`:

```lean
class Update (α : Type) where
  update : α → Cmd → Option (α × Effect)
```

## Op Interface

Common table operations across backends (ADBC, Mem, Kdb):

```
┌─────────────────────────────────────────────────────────┐
│  Tc/Op.lean (common operations)                         │
│    inductive Agg = count | sum | avg | min | max | ...  │
│    inductive Op = filter | sort | select | derive | ... │
│    structure Query = ops : Array Op                     │
│    class ExecOp α = exec : α → Op → IO α                │
└───────────────────────────┬─────────────────────────────┘
                            │ interpreted by
          ┌─────────────────┼─────────────────┐
          ▼                 ▼                 ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│ ADBC/Prql.lean  │ │ Mem/Exec.lean   │ │ Kdb/Q.lean      │
│ Op → PRQL → SQL │ │ Op → native     │ │ Op → q expr     │
└─────────────────┘ └─────────────────┘ └─────────────────┘
```

## Structures

| Struct       | Purpose                                      |
|--------------|----------------------------------------------|
| Verb         | Action type: inc/dec/ent/del/dup (5 verbs)   |
| Cmd          | Object + Verb command pattern (17 objects)   |
| Effect       | IO operation descriptor (fzf/query/folder)   |
| NavState     | Table + row/col cursors + selections + group |
| NavAxis      | Generic axis: cur (Fin n) + sels (Array)     |
| View         | Existential wrapper hiding table type        |
| ViewKind     | View type: tbl, colMeta, freqV, fld          |
| ViewStack    | Non-empty stack of Views (cur + parents)     |
| ViewState    | Scroll offsets for rendering                 |
| AppState     | Top-level: stk + vs + theme + info           |
| Theme.State  | Theme styles + index                         |
| Info.State   | Info overlay visibility                      |

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
LAYER 1: FOUNDATION
┌─────────────────────────────────────────────────────────────────┐
│ Types   Offset   Error   Cmd   Effect   Term   Op              │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 2: DATA
┌─────────────────────────────────────────────────────────────────┐
│ Data/CSV ──→ Data/Mem/Table ──→ Data/Mem/{Meta,Freq,Text}      │
│                                                                 │
│ Data/ADBC/FFI ──→ Data/ADBC/Table ──→ Data/ADBC/Meta           │
│              └──→ Data/ADBC/Prql                                │
│                                                                 │
│ Data/Kdb/FFI ──→ Data/Kdb/Table                                │
│             └──→ Data/Kdb/Q                                     │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 3: VIEW
┌─────────────────────────────────────────────────────────────────┐
│ Nav ──→ Render ──→ Key                                         │
│     └──→ View ──→ ViewStack                                    │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 4: FEATURES
┌─────────────────────────────────────────────────────────────────┐
│ Meta   Freq   Filter   Folder   Theme   Fzf   UI/Info          │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 5: TABLE ABSTRACTION (Plugin Architecture)
┌─────────────────────────────────────────────────────────────────┐
│ Table/Mem.lean     ──→ MemTable only                           │
│ Table/DuckDB.lean  ──→ MemTable | AdbcTable                    │
│ Table.lean         ──→ MemTable | AdbcTable | KdbTable         │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 6: STATE & DISPATCH
┌─────────────────────────────────────────────────────────────────┐
│ Dispatch (AppState, update) ──→ Runner (runEffect)             │
└─────────────────────────────────────────────────────────────────┘
                              ↓
LAYER 7: ENTRY POINTS
┌─────────────────────────────────────────────────────────────────┐
│ App/Core.lean   ──→ imports Table/Mem     (tc-core)            │
│ App/DuckDB.lean ──→ imports Table/DuckDB  (tc-duckdb)          │
│ App.lean        ──→ imports Table         (tc)                 │
└─────────────────────────────────────────────────────────────────┘

TESTS
┌─────────────────────────────────────────────────────────────────┐
│ TestLib ──→ CoreTest ──→ CoreTestMain (core-test exe)          │
│         └──→ Test (test exe, imports CoreTest + ADBC)          │
└─────────────────────────────────────────────────────────────────┘
```

### Plugin Architecture

Each `Table/*.lean` imports only its needed backends:

| Variant | Table constructors | Backends |
|---------|-------------------|----------|
| `Table/Mem.lean` | `.mem` | none |
| `Table/DuckDB.lean` | `.mem \| .adbc` | ADBC/DuckDB |
| `Table.lean` | `.mem \| .adbc \| .kdb` | ADBC + Kdb |

Generic code (View, Meta, Freq, etc.) uses typeclasses (`TblOps`, `ModifyTable`, `MemConvert`) so it works with any Table type.
