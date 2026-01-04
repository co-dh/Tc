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
│    exec dispatches Cmd to View or handles stk commands  │
└─────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────┐
│  App.mainLoop                                           │
│    evToCmd → ViewStack.exec → View.doRender             │
└─────────────────────────────────────────────────────────┘
```

## Classes

| Class       | Methods            | Purpose                    |
|-------------|--------------------|----------------------------|
| ReadTable   | nRows, colNames    | Read-only table access     |
|             | colWidths, cell    |                            |
| ModifyTable | delRows, delCols   | Table mutations            |
| RenderTable | render             | Render to terminal         |

## Obj/Verb Matrix

Commands follow `Obj Verb` pattern. Grouped by: navigation, selection, options.

```
                 │ I │ D │ T │ D │ A │ D │ D │ F │
                 │ N │ E │ O │ E │ S │ S │ U │ R │
                 │ C │ C │ G │ L │ C │ C │ P │ Q │
Char │ Obj       │ + │ - │ ~ │ d │ [ │ ] │ c │ F │ Description
─────┴───────────┴───┴───┴───┴───┴───┴───┴───┴───┴──────────────────
 --- Navigation ---
 r   │ row       │ j │ k │   │   │   │   │   │   │ Row cursor
 c   │ col       │ l │ h │   │   │   │   │   │   │ Column cursor
 v   │ vPage     │ J │ K │   │   │   │   │   │   │ Vertical page
 h   │ hPage     │ L │ H │   │   │   │   │   │   │ Horizontal page
 V   │ ver       │+j │+k │   │   │   │   │   │   │ Vertical end
 H   │ hor       │+l │+h │   │   │   │   │   │   │ Horizontal end
 --- Selection ---
 R   │ rowSel    │   │   │ T │   │   │   │   │   │ Row selection
 C   │ colSel    │   │   │ t │ d │ [ │ ] │   │ F │ Column selection
 g   │ grp       │   │   │ ! │   │   │   │   │   │ Group (pin left)
 --- Options ---
 s   │ stk       │   │ q │ S │   │   │   │   │   │ View stack
 p   │ prec      │+p │-p │   │   │   │   │   │   │ Display precision
 w   │ width     │+w │-w │   │   │   │   │   │   │ Column width
 M   │ metaCol   │ M │   │   │   │   │   │ 1 │ 0 │ Meta view
 f   │ freqCol   │   │   │   │   │   │   │   │   │ Freq view (TODO)
```

## Structures

| Struct    | Purpose                                      |
|-----------|----------------------------------------------|
| Verb      | Action type: inc/dec/toggle/del/sort/dup     |
| Cmd       | Object + Verb command pattern                |
| NavState  | Table + row/col cursors + selections + group |
| NavAxis   | Generic axis: cur (Fin n) + sels (Array)     |
| View      | Existential wrapper hiding table type        |
| ViewStack | Non-empty stack of Views (cur + parents)     |
| ViewState | Scroll offsets for rendering                 |

## Design Notes

**Obj/Verb pattern**: Commands are `Cmd obj verb`. Verbs are reusable (+/-/~/d/[/]/c).

**View existential**: Wraps `NavState nRows nCols t` to hide type params, enabling heterogeneous stack.

**ViewStack dispatch**: `stk` commands handled by stack, others delegated to `View.exec`.

**Fin bounds**: Cursor uses `Fin n` for type-safe bounds. `Fin.clamp` handles delta movement.

**Group columns**: Pinned left via `dispOrder`. Selection uses `Array.toggle`.
