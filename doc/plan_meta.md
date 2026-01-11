# Phase 2: Meta View Implementation Plan

## Overview
Meta view shows column statistics: col_name, type, count, distinct, null%, min, max.
Started via ViewStack command `.stk .inc` with argument (Meta, Freq, ls, etc.).

## Architecture

```
ViewStack.exec(.stk .inc)
       │
       ▼
PushArg? ──► Meta ──► ReadTable.queryMeta ──► MemTable ──► View(.colMeta)
         │                    │
         ├► Freq              ├── For ADBC: SQL UNION query
         │                    └── For MemTable: scan columns in Lean
         └► ls/cmd
```

## Changes Required

### 1. Extend ReadTable with `queryMeta`
**File: `Tc/Data/Table.lean`**
```lean
class ReadTable (α : Type) where
  nRows     : α → Nat
  colNames  : α → Array String
  totalRows : α → Nat := nRows
  queryMeta : α → IO MemTable  -- NEW: generate column stats
```

### 2. Add PushArg enum
**File: `Tc/Cmd.lean`**
```lean
inductive PushArg where
  | meta      -- column stats view
  | freq      -- frequency view (Phase 3)
  | cmd (s : String)  -- os command output
```

Modify ViewStack command to accept argument:
```lean
-- In ViewStack.lean or Cmd.lean
def pushWith : ViewStack → PushArg → IO (Option ViewStack)
```

### 3. ADBC Meta Query Builder
**File: `Tc/Data/ADBC/Meta.lean`** (new)

Reuse from tvl/Tv/Meta.lean:
- `quoteCol`: quote column name for SQL
- `colStatsSql`: build SQL for one column's stats
- SQL UNION query for all columns

```lean
-- Build SQL: SELECT 'col' AS column, 'type' AS type, COUNT(...), ...
-- UNION ALL for each column
def queryMeta (t : AdbcTable) : IO MemTable := do
  -- Extract format char → type name mapping
  -- Build UNION query
  -- Execute and convert to MemTable
```

### 4. MemTable Meta Generation
**File: `Tc/Data/MemTable.lean`**

Pure Lean column scan:
```lean
def MemTable.queryMeta (t : MemTable) : IO MemTable := do
  let mut rows : Array (Array String) := #[]
  for i in [:t.names.size] do
    let col := t.cols.getD i default
    let (typ, cnt, dist, nullPct, minV, maxV) := scanCol col
    rows := rows.push #[t.names.getD i "", typ, cnt, dist, nullPct, minV, maxV]
  -- Build MemTable from rows
```

### 5. Add cache field to View
**File: `Tc/View.lean`**

```lean
structure View where
  ...
  cache : Option MemTable := none  -- meta result cache
```

Cache path for ADBC (file-based caching):
```lean
def cachePath (path : String) : String := path ++ ".tc.meta.parquet"
```

### 6. Key handlers for meta view
**File: `Tc/Key.lean` or new `Tc/Meta.lean`**

| Key | Action |
|-----|--------|
| `0` | Select 100% null columns |
| `1` | Select single-value columns (distinct=1) |
| `Enter` | Pop and apply selection to parent |

```lean
def metaKey (v : View) (stk : ViewStack) (key : Char) : Option ViewStack :=
  match key with
  | '0' => -- select null columns
  | '1' => -- select single-value columns
  | _ => none  -- delegate to normal nav
```

### 7. ViewStack.pushMeta
**File: `Tc/ViewStack.lean`**

```lean
def pushMeta (s : ViewStack) : IO (Option ViewStack) := do
  let metaTbl ← @ReadTable.queryMeta s.cur.t s.cur.instR s.cur.tbl
  match View.fromTbl metaTbl s.cur.path with
  | some v => pure (some (s.push { v with vkind := .colMeta, disp := "meta" }))
  | none => pure (some s)  -- empty meta, stay
```

### 8. Update App.mainLoop for M key
**File: `Tc/App.lean`**

```lean
-- In evToCmd or key handler:
| 'M' => pushMeta stk  -- push meta view
```

## File Summary

| File | Changes |
|------|---------|
| `Tc/Data/Table.lean` | Add `queryMeta` to ReadTable |
| `Tc/Data/ADBC/Meta.lean` | NEW: SQL UNION query builder |
| `Tc/Data/ADBC/Table.lean` | Implement `queryMeta` for AdbcTable |
| `Tc/Data/MemTable.lean` | Implement `queryMeta` for MemTable |
| `Tc/View.lean` | Add `cache` field |
| `Tc/ViewStack.lean` | Add `pushMeta` function |
| `Tc/App.lean` | Add 'M' key handler |
| `Tc/Cmd.lean` | Add `PushArg` enum (optional) |

## Column Indices (from tvl)
```lean
def colDist : Nat := 3   -- distinct count
def colNull : Nat := 4   -- null%
```

## Type Mapping (Arrow format char → type name)
```lean
def fmtToType : Char → String
  | 'l' => "int64" | 'i' => "int32" | 's' => "int16" | 'c' => "int8"
  | 'L' => "uint64" | 'I' => "uint32" | 'S' => "uint16" | 'C' => "uint8"
  | 'g' => "float64" | 'f' => "float32" | 'd' => "decimal"
  | 'u' | 'U' => "str" | 'b' => "bool"
  | _ => "?"
```

## Implementation Order
1. Add `queryMeta` to ReadTable class
2. Implement for MemTable (simpler, pure Lean)
3. Implement for AdbcTable (SQL UNION)
4. Add cache to View
5. Add ViewStack.pushMeta
6. Add M key
7. Add meta key handlers (0/1/Enter)
8. Add parquet caching (ADBC only)
