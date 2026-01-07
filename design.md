# Tc Design

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  ReadTable α              ModifyTable α [ReadTable α]   │
│    nRows, colNames          delRows, delCols            │
│    colWidths, cell                                      │
├─────────────────────────────────────────────────────────┤
│  RenderTable α [ReadTable α]                            │
│    render : NavState → ViewState → IO ViewState         │
└───────────────────────────┬─────────────────────────────┘
                            │ instance
                            ▼
┌─────────────────────────────────────────────────────────┐
│  Backends: AdbcTable, MemTable                          │
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

| Class       | Methods            | Purpose                    |
|-------------|--------------------|----------------------------|
| ReadTable   | nRows, colNames    | Read-only table access     |
|             | colWidths, cell    |                            |
| ModifyTable | delRows, delCols   | Table mutations            |
| RenderTable | render             | Render to terminal         |
| Update      | update             | Pure: Cmd → (State, Effect)|
| Exec        | exec               | IO: Cmd → IO State (compat)|

## Obj/Verb Matrix

Commands follow `Obj Verb` pattern. Grouped by: navigation, selection, options.

**Command mode**: Press `space` to open fzf object picker, then fzf verb picker.

```
                 │ D │ I │ E │ D │ D │
                 │ E │ N │ N │ E │ U │
                 │ C │ C │ T │ L │ P │
Char │ Obj       │ , │ . │ ~ │ d │ c │ Description
─────┴───────────┴───┴───┴───┴───┴───┴──────────────────
 --- Navigation ---
 r   │ row       │ k │ j │   │   │   │ Row cursor
 c   │ col       │ h │ l │ s │   │   │ Column cursor, ~=fzf jump
 v   │ vPage     │ K │ J │   │   │   │ Vertical page
 h   │ hPage     │ H │ L │   │   │   │ Horizontal page
 V   │ ver       │Hom│End│   │   │   │ Vertical end
 H   │ hor       │ ← │ → │   │   │   │ Horizontal end (,=first, .=last col)
 --- Selection ---
 R   │ rowSel    │ \ │ / │ T │   │   │ Row: ,=filter, .=search, ~=toggle
 C   │ colSel    │ ] │ [ │ t │ d │   │ Col: ,=sortDesc, .=sortAsc, ~=toggle
 g   │ grp       │ N │ n │ ! │   │   │ Grp: ,=prev, .=next, ~=toggle
 --- Options ---
 s   │ stk       │ q │   │ S │   │ c │ View stack (q=pop, S=swap, c=dup)
 p   │ prec      │   │   │   │   │   │ Display precision (space p ,/.)
 w   │ width     │   │   │   │   │   │ Column width (space w ,/.)
 T   │ thm       │   │   │   │   │   │ Theme cycle (space T ,/.)
 i   │ info      │   │   │ I │   │   │ Info overlay toggle
 --- Views ---
 M   │ metaV     │ 0 │ 1 │ ⏎ │   │ M │ Meta view (c=push, ,=selNull, .=selSingle)
 F   │ freq      │   │   │ ⏎ │   │ F │ Freq view (c=push, ~=filter by row when in freq)
 D   │ fld       │   │   │ ⏎ │ d │ D │ Folder view (c=push, space D ,/.=depth, ~=enter, d=trash)
```

## Structures

| Struct       | Purpose                                      |
|--------------|----------------------------------------------|
| Verb         | Action type: inc/dec/ent/del/dup (5 verbs)   |
| Cmd          | Object + Verb command pattern                |
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

## Op Interface

Common table operations across backends (ADBC, Mem, future kdb/q):

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
│ ADBC/Prql.lean  │ │ Mem/Exec.lean   │ │ Kdb/Exec.lean   │
│ Op → PRQL → SQL │ │ Op → native     │ │ Op → q expr     │
└─────────────────┘ └─────────────────┘ └─────────────────┘
```

**Tc/Op.lean**:
```lean
inductive Agg where
  | count | sum | avg | min | max | stddev | dist

inductive Op where
  | filter (expr : String)
  | sort (cols : Array (String × Bool))  -- Bool = asc
  | select (cols : Array String)
  | derive (bindings : Array (String × String))
  | group (keys : Array String) (aggs : Array (Agg × String × String))
  | take (n : Nat)

class ExecOp (α : Type) where
  exec : α → Op → IO α
```

**Backend implementations**:
- ADBC: `Op → PRQL string → SQL → DuckDB`
- Mem: `Op → direct Array ops`
- Kdb: `Op → q expression → IPC`

## Effect DSL

Effect describes IO operations without executing them. Pure `update` returns Effect; Runner interprets it.

```lean
inductive Effect where
  | none                                    -- no effect
  | quit                                    -- exit app
  -- fzf (user selection)
  | fzfCmd                                  -- command mode: space
  | fzfCol                                  -- column picker: s
  | fzfRow (col : Nat) (name : String)      -- row search: /
  | fzfFilter (col : Nat) (name : String)   -- row filter: \
  -- query (database/table ops)
  | queryMeta                               -- push meta view: M
  | queryFreq (cols : Array Nat) (names : Array String)  -- push freq: F
  | freqFilter (cols : Array String) (row : Nat)         -- filter from freq
  | queryFilter (expr : String)             -- apply filter expr
  | querySort (col : Nat) (grp : Array Nat) (asc : Bool) -- sort: [/]
  | queryDel (col : Nat) (sels : Array Nat) (grp : Array String)  -- delete: d
  -- folder (filesystem)
  | folderPush                              -- push folder view: D
  | folderEnter                             -- enter dir/file: Enter
  | folderDel                               -- delete file: d
  | folderDepth (delta : Int)               -- change find depth: ,d/.d
  -- search
  | findNext                                -- search next: n
  | findPrev                                -- search prev: N
  -- theme
  | themeLoad (delta : Int)                 -- cycle theme: ,T/.T
```

**Functor pattern**: `update` maps `Cmd → Effect` (state passes through):

```lean
class Update (α : Type) where
  update : α → Cmd → Option (α × Effect)

-- Example: Folder.update
def update (s : ViewStack) (cmd : Cmd) : Option (ViewStack × Effect) :=
  match cmd with
  | .fld .dup => some (s, .folderPush)      -- Cmd.fld.dup → Effect.folderPush
  | .fld .inc => some (s, .folderDepth 1)   -- Cmd.fld.inc → Effect.folderDepth 1
  | .fld .dec => some (s, .folderDepth (-1))
  | .colSel .del => some (s, .folderDel)
  | .view .ent => some (s, .folderEnter)
  | _ => none
```
