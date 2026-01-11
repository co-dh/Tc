# Plan: Common Op Interface

## Goal
Extract `Op` and `Agg` from `ADBC/Prql.lean` into common `Tc/Op.lean`, implement `ExecOp` for each backend.

## Steps

### 1. Create Tc/Op.lean
- Move `Agg` inductive from Prql.lean
- Move `Op` inductive from Prql.lean
- Add `ExecOp` typeclass: `exec : α → Op → IO α`
- Add lakefile entry

### 2. Refactor ADBC/Prql.lean
- Import Tc/Op
- Keep PRQL-specific: `Query`, `render`, `compile`, `quote`, `buildFilter`
- Add `Op.toPrql : Op → String` (rename from `Op.render`)
- Implement `ExecOp AdbcTable` via PRQL compilation

### 3. Implement ExecOp for MemTable
- Create Tc/Data/Mem/Exec.lean (or extend Table.lean)
- `Op.filter` → reuse existing `MemTable.filter`
- `Op.sort` → reuse existing `MemTable.sort`
- `Op.select` → filter columns by name
- `Op.group` → implement or defer (complex)
- `Op.take` → slice rows

### 4. Update callers
- Meta.lean: use `ExecOp` instead of direct PRQL
- Freq.lean: use `ExecOp` instead of direct PRQL
- Filter.lean: use `ExecOp` for row filter

### 5. Test
- Verify ADBC path unchanged
- Verify Mem path works with new interface
- Run full test suite

## Files Changed
- New: `Tc/Op.lean`
- New: `Tc/Data/Mem/Exec.lean`
- Modify: `Tc/Data/ADBC/Prql.lean`
- Modify: `Tc/Meta.lean`, `Tc/Freq.lean`, `Tc/Filter.lean`
- Modify: `lakefile.lean`

## Notes
- Start with filter/sort/select (already implemented for Mem)
- Defer group/aggregate for Mem (complex, ADBC handles well)
- Keep Prql.lean for PRQL-specific rendering/compilation
