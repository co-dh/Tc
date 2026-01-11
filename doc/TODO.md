# Missing Features: Tc vs tvl

## 1. ViewStack System (CORE MISSING)
| Feature | tvl | Tc |
|---------|-----|-----|
| Non-empty view stack | ✅ `curView` + `parents` | ❌ Single NavState |
| push/pop operations | ✅ | ❌ |
| swapViews (S) | ✅ | ❌ |
| dupView (T) | ✅ | ❌ |
| Tab line rendering | ✅ | ❌ |
| View cache per view | ✅ | ❌ |
| View kinds (tbl/freq/meta/fld) | ✅ | ❌ |

## 2. Specialized Views
| Feature | tvl | Tc |
|---------|-----|-----|
| Frequency view (F) | ✅ group by key+cursor | ❌ |
| Meta view (M) | ✅ column stats | ❌ |
| Quick stats (m) | ✅ | ❌ |
| Enter → filter parent | ✅ | ❌ |

## 3. Filtering & fzf
| Feature | tvl | Tc |
|---------|-----|-----|
| PRQL filter (\) via fzf | ✅ | ❌ |
| Value filter (Enter on cell) | ✅ | ❌ |
| Jump to row (@) | ✅ | ❌ |
| fzf multi-select | ✅ | ❌ |

## 4. Column Operations
| Feature | tvl | Tc |
|---------|-----|-----|
| Rename column (^) | ✅ input mode | ❌ |
| Derive new column | ✅ | ❌ |
| Select columns (s) | ✅ | ❌ |

## 5. Aggregation
| Feature | tvl | Tc |
|---------|-----|-----|
| Group by (b) | ✅ | ❌ |
| Aggregate funcs | ✅ count/sum/avg/min/max/stddev | ❌ |

## 6. System Sources
| Feature | tvl | Tc |
|---------|-----|-----|
| ps (processes) | ✅ | ❌ |
| df (disk) | ✅ | ❌ |
| env (environment) | ✅ | ❌ |
| ls/lr (directory) | ✅ | ❌ |

## 7. Display
| Feature | tvl | Tc |
|---------|-----|-----|
| Info overlay (I) | ✅ | ❌ |
| Decimal control (./,) | ✅ | ❌ |
| Memory in status | ✅ | ❌ |

---

# Implementation Plan

## Phase 1: ViewStack Foundation
1. Add ViewStack structure (State.lean)
   - curView: View
   - parents: Array View
   - View: { path, query, nav, vkind, cache, disp }

2. Add ViewKind enum
   - tbl, freqV, colMeta, fld

3. Implement stack ops
   - push: save current, create new
   - pop: restore parent (q key)
   - swap: swap top two (S key)
   - dup: clone current (T key)

4. Render tab line
   - show all views in stack
   - highlight current

## Phase 2: Meta View
1. Add meta query (column stats via SQL UNION)
   - col_name, type, count, distinct, null%, min, max

2. Key handlers in meta view
   - 0: select null columns
   - 1: select single-value columns
   - Enter: navigate to source column

3. Cache meta results alongside source

## Phase 3: Frequency View
1. Add freq query builder
   - GROUP BY key_cols + cursor_col
   - SELECT: value, count, pct, bar

2. Enter → filter parent view
   - push filter op to parent query
   - pop freq view

## Phase 4: fzf Integration
1. Add Fzf.lean module
   - run fzf with options
   - multi-select support

2. Filter picker (\)
   - show distinct values
   - build PRQL filter

3. Jump picker (@)
   - enter row number

## Phase 5: Column Operations
1. Rename (^)
   - input mode for collecting name
   - update query with derive + select

2. Derive (=)
   - fzf expression input
   - add derive op

3. Aggregation (b)
   - group by key columns
   - fzf to select agg funcs

## Phase 6: System Sources
1. Add Source.lean
   - ps: process list
   - df: disk usage
   - env: environment vars
   - ls/lr: directory listing

2. Register as virtual tables

## Phase 7: Polish
1. Info overlay (I)
2. Decimal control (./,)
3. Memory display
4. Error display in status bar

## change the Verb letter to +-<>0$~
## Backend.lean not used? getIdx not used?
