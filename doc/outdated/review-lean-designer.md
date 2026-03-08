# Code Review: Lean Language Designer Perspective

## Summary
Review of `tc` codebase from the perspective of a Lean language designer and stdlib writer.

## Strengths

### Type-Level Invariants
The `NavState` design lifts row/column counts to type level with proof terms:
```lean
structure NavState (nRows nCols : Nat) (t : Type) [ReadTable t] where
  tbl   : t
  hRows : ReadTable.nRows tbl = nRows  -- proof row count matches
  hCols : (ReadTable.colNames tbl).size = nCols
  row   : RowNav nRows
  col   : ColNav nCols
```
Using `Fin n` for cursors provides compile-time bounds checking. This is idiomatic Lean.

### Typeclass Hierarchy
Clean separation: `ReadTable` → `ModifyTable` → `QueryTable`. Follows stdlib patterns (like `Monad` hierarchy).

### Closed Sum Types
`Table` as closed sum avoids universe issues:
```lean
inductive Table where
  | mem : MemTable → Table
  | adbc : AdbcTable → Table
```
Pragmatic choice over existential types.

## Issues

### Incomplete Proofs
```lean
theorem dispOrder_size ... := by
  sorry -- filter removes exactly valid group.size elements
```
This is acceptable for a TUI app, but should be addressed. Consider using `Nat.card` properties.

### Missing Decidability Instances
`ViewKind` derives `BEq` but some code patterns suggest equality comparisons could benefit from `DecidableEq`.

### Array vs List
Good use of `Array` throughout (per project guidelines). However, some intermediate conversions:
```lean
def Array.join (arr : Array String) (sep : String) : String :=
  String.intercalate sep arr.toList
```
Consider using `Array.foldl` to avoid allocation.

### Typeclass Instance Discovery
The `ReadTable`, `ModifyTable` pattern is clean, but some functions take `[ReadTable α]` when `[ModifyTable α]` would be clearer. Document which operations require which constraints.

## Suggestions

### Use `Std.Data.Array` Extensions
Std4 provides `Array.findIdx?`, `Array.modify`, etc. Prefer these over custom implementations.

### Consider `Subtype` for Invariants
For `View.widths`, could use:
```lean
widths : { w : Array Nat // w.size = nCols ∨ w.isEmpty }
```
This would make the "widths match columns" invariant explicit.

### Leverage `do`-notation More
Some match chains could be flattened:
```lean
-- Before
match s.pop with
| some s' => match ← QueryTable.filter s'.cur.nav.tbl expr with
  | some tbl' => ...
-- After
let some s' := s.pop | return (some s)
let some tbl' ← QueryTable.filter s'.cur.nav.tbl expr | return (some s')
```

### Document IO Effects
Functions returning `IO` should document what effects they perform (file I/O, FFI calls, etc.).

## Overall
Solid Lean 4 code. The type-level navigation bounds are well-designed. Main improvements: complete proofs, reduce intermediate allocations, and strengthen invariants with dependent types where beneficial.
