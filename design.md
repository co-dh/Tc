# Tc/ Typeclass Navigation Design

## Diagram

```
┌─────────────────────────────────────────────────────────┐
│                      NavState n                         │
│  nRows, colNames, hNames, hGroup                        │
│  ┌──────────────────────────────────────────────────┐   │
│  │  NavAxis n elem  (single CurOps instance)        │   │
│  │    cur  : Fin n                                  │   │
│  │    sels : Array elem                             │   │
│  ├──────────────────────────────────────────────────┤   │
│  │  RowNav m = NavAxis m Nat      (row)             │   │
│  │  ColNav n = NavAxis n String   (col)             │   │
│  └──────────────────────────────────────────────────┘   │
│  group : Array String                                   │
│  helpers: nKeys, selRows, selColIdxs, curColIdx, etc    │
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
| NavState  | Dims + RowNav + ColNav + group + helpers   |
| NavAxis   | Generic axis: cur (Fin n) + sels (Array)   |
| RowNav m  | NavAxis m Nat (type alias)                 |
| ColNav n  | NavAxis n String (type alias)              |
| ViewState | Scroll offsets (view concern)              |

## Key Design Decisions

**NavState**: Contains dims (nRows, colNames) + navigation (row, col, group). Single structure.

**NavAxis**: Generic `NavAxis n elem` with single CurOps instance. RowNav/ColNav are type aliases.

**ViewState**: Scroll offsets only. Separate from NavState.

**Fin bounds**: Cursor uses `Fin n` for type-safe bounds. `Fin.clamp` handles delta movement.

**CurOps defaults**: `move` and `find` have default implementations using `pos`/`setPos`.

**Array.toggle**: Selection uses plain Array with `toggle` extension. No wrapper type.

**Display order**: Group columns first via `dispOrder`.
