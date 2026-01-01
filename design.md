# Tc/ Typeclass Navigation Design

## Diagram

```
┌─────────────────────────────────────────────────────────┐
│                      NavState n                         │
│  nRows : Nat, colNames : Array String, hNames : proof   │
│  ┌──────────────────────────────────────────────────┐   │
│  │  NavAxis n elem  (generic, single CurOps inst)   │   │
│  │    cur  : Fin n                                  │   │
│  │    sels : OrdSet elem                            │   │
│  ├──────────────────────────────────────────────────┤   │
│  │  RowNav m = NavAxis m Nat      (row)             │   │
│  │  ColNav n = NavAxis n String   (col)             │   │
│  └──────────────────────────────────────────────────┘   │
│  group : OrdSet String                                  │
│                                                         │
│  3 OrdSets: row.sels, col.sels, group (SetOps toggle)   │
└─────────────────────────────────────────────────────────┘
                      │
                      ▼ (view layer)
┌─────────────────────────────────────────────────────────┐
│                    ViewState n                          │
│  rowOff : Nat          (first visible row)              │
│  colOff : Fin n        (first visible col)              │
└─────────────────────────────────────────────────────────┘
```

## Classes

| Class   | Methods               | Purpose                    |
|---------|-----------------------|----------------------------|
| CurOps  | pos : α → Fin bound   | Get cursor position        |
|         | setPos : Fin → α → α  | Set cursor position        |
|         | move : Int → α → α    | Move by delta (default)    |
|         | find : (e→Bool) → α   | Search (default no-op)     |
| SetOps  | toggle : e → α → α    | Toggle element in set      |

## Object

| Symbol | Target     | Description                  |
|--------|------------|------------------------------|
| r      | row.cur    | Current row position         |
| c      | col.cur    | Current column position      |
| R      | row.sels   | Selected row indices         |
| C      | col.sels   | Selected column names        |
| G      | group      | Columns pinned to left       |

## Verb

| Op | r/c (cursor) | R/C/G (set) |
|----|--------------|-------------|
| +  | move +1      |             |
| -  | move -1      |             |
| <  | move -page   |             |
| >  | move +page   |             |
| 0  | move to 0    |             |
| $  | move to end  |             |
| ~  |              | toggle      |

## Keys

| Key  | Command | Action              |
|------|---------|---------------------|
| hjkl | r±/c±   | Step nav            |
| HJKL | r</>/c  | Page nav            |
| g+dir| r0/$/c  | Home/end            |
| t    | R^      | Toggle row select   |
| T    | C^      | Toggle col select   |
| !    | G^      | Toggle group        |
| q    |         | Quit                |

## Structures

| Struct    | Purpose                                    |
|-----------|--------------------------------------------|
| NavState  | Dims + RowNav + ColNav + group             |
| NavAxis   | Generic axis: cur (Fin n) + sels (OrdSet)  |
| RowNav m  | NavAxis m Nat (type alias)                 |
| ColNav n  | NavAxis n String (type alias)              |
| OrdSet    | Ordered set of selected elements           |
| ViewState | Scroll offsets (view concern)              |

## Key Design Decisions

**NavState**: Contains dims (nRows, colNames) + navigation (row, col, group). Single structure.

**NavAxis**: Generic `NavAxis n elem` with single CurOps instance. RowNav/ColNav are type aliases.

**ViewState**: Scroll offsets only. Separate from NavState.

**Fin bounds**: Cursor uses `Fin n` for type-safe bounds. `Fin.clamp` handles delta movement.

**CurOps defaults**: `move` and `find` have default implementations using `pos`/`setPos`.

**SetOps.toggle**: Only toggle needed. Add/remove/clear/all/invert removed.

**Display order**: Group columns first via `dispOrder`.
