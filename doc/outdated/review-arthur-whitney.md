# Code Review: Arthur Whitney Perspective

## Summary
Review of `tc` codebase from the perspective of Arthur Whitney (creator of K, Q, kdb+).

## Observations

### Good: Data-Centric Design
Column-oriented storage is correct:
```lean
inductive Column where
  | ints   (data : Array Int64)
  | floats (data : Array Float)
  | strs   (data : Array String)
```
Homogeneous arrays. No per-cell type tags. This is the right approach.

### Good: Short Local Names
Names like `n`, `v`, `s`, `r`, `c` are fine for local scope. Code is readable.

### Good: No Magic Numbers
Constants are named: `reservedLines`, `maxColWidth`, `colPageSize`.

## Issues

### Verbose Function Names
```lean
def colIdxAt (group : Array String) (names : Array String) (i : Nat) : Nat
def dispOrder (group : Array String) (names : Array String) : Array Nat
```
Consider: `cidx`, `dord`. Context makes meaning clear.

### Redundant Structure Fields
```lean
structure NavAxis (n : Nat) (elem : Type) where
  cur  : Fin n
  sels : Array elem := #[]
```
`sels` could be derived or computed lazily. Selection is sparse - consider bitset for large n.

### Unnecessary Intermediate Allocations
```lean
def Array.join (arr : Array String) (sep : String) : String :=
  String.intercalate sep arr.toList  -- allocates list
```
Should fold directly.

### Match Chains
```lean
match s.cur.vkind with
| .colMeta => pure s.metaSetKey
| .freqV _ => s.freqFilter
| _ => pure (some s)
```
Consider dispatch table: `#[(.colMeta, metaSetKey), (.freqV, freqFilter)]`.

### Verb Enum Too Fine-Grained
```lean
inductive Verb where
  | inc | dec | ent | del | sortAsc | sortDesc | dup | freq | search | filter
```
10 verbs. In K: just `+ - ! # & |`. Consider: movement is `+/-`, selection is `!`, sort is `</>`. Fewer primitives, more composition.

## Suggestions

### Flatten ViewStack
`ViewStack` = `(View, Array View)`. Could be `Array View` with `cur` being `a[0]`. One structure.

### Unify Table Operations
```lean
class QueryTable (α : Type) where
  queryMeta : α → IO MetaTuple
  queryFreq : α → Array Nat → IO FreqTuple
  filter    : α → String → IO (Option α)
  distinct  : α → Nat → IO (Array String)
```
These are all queries. Could be one function: `q : α → Cmd → IO α`. Command encodes operation.

### Remove Proof Overhead
```lean
hRows : ReadTable.nRows tbl = nRows
hCols : (ReadTable.colNames tbl).size = nCols
```
Runtime proofs carried in hot path. For TUI: trust the invariant, omit proofs. Use `sorry` or `unsafe` if needed.

### Bit-Level Selection
Row selection as `Array Nat` allocates. Use `BitVec` or `UInt64` bitmask for < 64 selections. Common case.

## Code Density
Current: ~1400 lines for TUI table viewer.
K equivalent: ~100-200 lines.
Lean overhead: types, proofs, FFI. Acceptable for typed language.

## Overall
Reasonable. Not minimal but not bloated. Main waste: type machinery carried at runtime, match chains instead of dispatch tables, List allocations in joins. For production: inline hot paths, use bitsets, reduce allocations.
