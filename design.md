# Tc/ Typeclass Navigation Design

## Diagram

```
┌─────────────────────────────────────────────────────────┐
│                        Nav n                            │
│  nRows : Nat, colNames : Array String, hNames : proof   │
└─────────────────────┬───────────────────────────────────┘
                      │ parameterizes
                      ▼
┌─────────────────────────────────────────────────────────┐
│                    NavState n t                         │
│  ┌──────────────────────┐  ┌──────────────────────────┐ │
│  │       RowNav         │  │      ColNav n            │ │
│  │  cur : Nat           │  │  cur : Fin n             │ │
│  │  sels: OrdSet Nat ───┼──┼─ sels: OrdSet String ────┤ │
│  │    (CurOps)          │  │    (CurOps)              │ │
│  └──────────────────────┘  └──────────────────────────┘ │
│                                                         │
│  group : OrdSet String ─────────────────────────────────┤ │
│                                                         │
│  3 OrdSets total: row.sels, col.sels, group             │
│  All use SetOps (toggle only)                           │
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

| Class   | Methods              | Purpose                    |
|---------|----------------------|----------------------------|
| CurOps  | move : Int → α → α   | Cursor movement by delta   |
|         | find : (e→Bool) → α  | Search (placeholder)       |
| SetOps  | toggle : e → α → α   | Toggle element in set      |

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
| Nav n     | Query dims (nRows, colNames, proof)        |
| OrdSet    | Ordered set with invert flag               |
| RowNav    | Row cursor + selections                    |
| ColNav n  | Column cursor (Fin n) + selections         |
| NavState  | Composes RowNav + ColNav + group           |
| ViewState | Scroll offsets (view concern)              |

## Key Design Decisions

**Separation**: NavState = navigation logic, ViewState = scroll offsets.

**Fin n**: ColNav cursor is `Fin n` for type-safe bounds.

**CurOps.move**: Unified movement by Int delta, clamped. Home = move(-cur), end = move(bound-1-cur).

**SetOps.toggle**: Only toggle needed. Add/remove/clear/all/invert removed.

**OrdSet.inv**: Invert flag avoids materializing large inverted selections.

**Display order**: Group columns first via `dispOrder`.
