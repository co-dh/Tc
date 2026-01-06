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
│    hides table type, exposes exec/doRender              │
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
│    exec chains: theme → info → stk → fld → meta → freq → ...│
└───────────────────────────┬─────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────┐
│  App.mainLoop                                           │
│    evToCmd → AppState.exec → View.doRender              │
└─────────────────────────────────────────────────────────┘
```

## Classes

| Class       | Methods            | Purpose                    |
|-------------|--------------------|----------------------------|
| ReadTable   | nRows, colNames    | Read-only table access     |
|             | colWidths, cell    |                            |
| ModifyTable | delRows, delCols   | Table mutations            |
| RenderTable | render             | Render to terminal         |
| Exec        | exec               | Execute Cmd, return state  |

## Obj/Verb Matrix

Commands follow `Obj Verb` pattern. Grouped by: navigation, selection, options.

```
                 │ I │ D │ E │ D │ A │ D │ D │ S │ F │
                 │ N │ E │ N │ E │ S │ S │ U │ R │ L │
                 │ C │ C │ T │ L │ C │ C │ P │ C │ T │
Char │ Obj       │ + │ - │ ~ │ d │ [ │ ] │ c │ s │ f │ Description
─────┴───────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴──────────────────
 --- Navigation ---
 r   │ row       │ j │ k │   │   │   │   │   │ / │ \ │ Row cursor, /=search, \=filter
 c   │ col       │ l │ h │   │   │   │   │   │ s │   │ Column cursor, s=fzf jump
 v   │ vPage     │ J │ K │   │   │   │   │   │   │   │ Vertical page
 h   │ hPage     │ L │ H │   │   │   │   │   │   │   │ Horizontal page
 V   │ ver       │+j │+k │   │   │   │   │   │   │   │ Vertical end
 H   │ hor       │+l │+h │   │   │   │   │   │   │   │ Horizontal end
 --- Selection ---
 R   │ rowSel    │ n │ N │ T │   │   │   │   │   │   │ Row selection, n/N=search next/prev
 C   │ colSel    │   │   │ t │ d │ [ │ ] │   │   │   │ Column selection
 g   │ grp       │   │   │ ! │   │   │   │   │   │   │ Group (pin left)
 --- Options ---
 s   │ stk       │   │ q │ S │   │   │   │ c │   │   │ View stack (q=pop, S=swap, c=dup)
 p   │ prec      │+p │-p │   │   │   │   │   │   │   │ Display precision
 w   │ width     │+w │-w │   │   │   │   │   │   │   │ Column width
 T   │ thm       │+T │-T │   │   │   │   │   │   │   │ Theme cycle
 i   │ info      │+i │-i │ I │   │   │   │   │   │   │ Info overlay toggle
 --- Views ---
 M   │ metaV     │ 1 │ 0 │ ⏎ │   │   │   │ M │   │   │ Meta view (c=push, -=selNull, +=selSingle)
 f   │ freq      │   │   │ ⏎ │   │   │   │ F │   │   │ Freq view (c=push, ~=filter)
 D   │ fld       │+d │-d │ ⏎ │ d │   │   │ D │   │   │ Folder view (c=push, +/-=depth, ~=enter, d=trash)
```

## Structures

| Struct       | Purpose                                      |
|--------------|----------------------------------------------|
| Verb         | Action type: inc/dec/ent/del/sort/dup/search/filter |
| Cmd          | Object + Verb command pattern                |
| NavState     | Table + row/col cursors + selections + group |
| NavAxis      | Generic axis: cur (Fin n) + sels (Array)     |
| View         | Existential wrapper hiding table type        |
| ViewKind     | View type: tbl, colMeta, freqV, fld          |
| ViewStack    | Non-empty stack of Views (cur + parents)     |
| ViewState    | Scroll offsets for rendering                 |
| AppState     | Top-level: stk + vs + theme + info           |
| Theme.State  | Theme styles + index                         |
| Info.State   | Info overlay visibility                      |

## Design Notes

**Obj/Verb pattern**: Commands are `Cmd obj verb`. Verbs are reusable (+/-/~/d/[/]/c).

**View existential**: Wraps `NavState nRows nCols t` to hide type params, enabling heterogeneous stack.

**Dispatch chain**: AppState.exec tries handlers in order: theme → info → stk → fld → meta → freq → filter → view. First to return `some` wins.

**Exec typeclass**: Unifies command handling. `exec : Cmd → IO (Option α)` returns new state or none.

**Fin bounds**: Cursor uses `Fin n` for type-safe bounds. `Fin.clamp` handles delta movement.

**Group columns**: Pinned left via `dispOrder`. Selection uses `Array.toggle`.

**No-file mode**: Running without args shows folder view of current directory via `find -maxdepth 1`.

**Folder view**: Uses `find` with configurable depth. Enter on dir pushes new folder view, on file opens with `bat`/`less`. Delete (`d`) trashes selected files via `trash-put` or `gio trash` with y/n confirmation. Stores path and depth in `ViewKind.fld`.

## Op Interface (planned)

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
