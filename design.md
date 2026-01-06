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
│    exec chains: theme → info → stk → meta → freq → ...  │
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
```

## Structures

| Struct       | Purpose                                      |
|--------------|----------------------------------------------|
| Verb         | Action type: inc/dec/ent/del/sort/dup/search/filter |
| Cmd          | Object + Verb command pattern                |
| NavState     | Table + row/col cursors + selections + group |
| NavAxis      | Generic axis: cur (Fin n) + sels (Array)     |
| View         | Existential wrapper hiding table type        |
| ViewStack    | Non-empty stack of Views (cur + parents)     |
| ViewState    | Scroll offsets for rendering                 |
| AppState     | Top-level: stk + vs + theme + info           |
| Theme.State  | Theme styles + index                         |
| Info.State   | Info overlay visibility                      |

## Design Notes

**Obj/Verb pattern**: Commands are `Cmd obj verb`. Verbs are reusable (+/-/~/d/[/]/c).

**View existential**: Wraps `NavState nRows nCols t` to hide type params, enabling heterogeneous stack.

**Dispatch chain**: AppState.exec tries handlers in order: theme → info → stk → meta → freq → filter → view. First to return `some` wins.

**Exec typeclass**: Unifies command handling. `exec : Cmd → IO (Option α)` returns new state or none.

**Fin bounds**: Cursor uses `Fin n` for type-safe bounds. `Fin.clamp` handles delta movement.

**Group columns**: Pinned left via `dispOrder`. Selection uses `Array.toggle`.

**No-file mode**: Running without args shows `ls -l` of current directory via MemTable.fromText.
