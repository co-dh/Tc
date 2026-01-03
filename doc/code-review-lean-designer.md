# Code Review: Tc (Lean Designer's Perspective)

This is a well-architected terminal CSV viewer demonstrating sophisticated use of Lean 4's type system.

---

## Strengths

### 1. Excellent use of `Fin` for cursor bounds
```lean
-- Nav.lean:36
structure NavAxis (n : Nat) (elem : Type) [BEq elem] where
  cur  : Fin n              -- cursor position is statically bounded
```
This eliminates off-by-one errors at compile time. The `Fin.clamp` helper (Nav.lean:15-18) is a clean pattern.

### 2. Clean typeclass hierarchy
```lean
-- Table.lean
class ReadTable (α : Type) where ...
class ModifyTable (α : Type) extends ReadTable α where
  delCols : Array Nat → α → α
```
Proper inheritance, minimal surface area. `ModifyTable` only adds what's needed.

### 3. Proof-carrying navigation state
```lean
-- Nav.lean:75-82
structure NavState (nRows nCols : Nat) (t : Type) [ReadTable t] where
  tbl_   : t
  hRows_ : ReadTable.nRows tbl_ = nRows   -- proves table matches type params
  hCols_ : (ReadTable.colNames tbl_).size = nCols
```
Lifting runtime values to type level with proofs is the right approach for `Fin` bounds.

### 4. CumW as function type
```lean
-- Offset.lean:11
abbrev CumW (n : Nat) := Fin (n + 1) → Nat
```
Elegant: encapsulates cumulative width as a pure function, not memoized array. Works naturally with `Fin`.

### 5. Zero-copy FFI pattern
```lean
-- Types.lean:186-200
@[inline] unsafe def getIdxImpl (t : SomeTable) ...
@[implemented_by getIdxImpl]
def getIdx (t : SomeTable) (_ _ : Nat) : Cell := .null
```
Correct use of `unsafe`/`@[implemented_by]` for FFI with safe interface.

---

## Issues to Address

### 1. `sorry` proofs (3 instances)

| Location        | Issue                             |
|----------       |-------                            |
| Nav.lean:65     | `dispOrder_size` theorem unproven |
| Render.lean:102 | `curColIdx` proof elided          |
| App.lean:60,74  | `colWidths.size = nCols` unproven |

These are real proof obligations. The `dispOrder_size` theorem is straightforward but needs work:
```lean
theorem dispOrder_size (group names : Array String) :
    (dispOrder group names).size = names.size := by
  simp only [dispOrder, Array.size_append]
  sorry -- filter removes exactly valid group.size elements
```

The invariant that `colWidths.size = colNames.size` should be enforced by construction in `ReadTable`, not assumed via `sorry`.

### 2. Inconsistent index semantics

`NavState.curCol` returns display index, but `NavState.curColIdx` returns original index:
```lean
def curCol (nav : ...) : Nat := nav.col_.cur.val  -- display index
def curColIdx (nav : ...) : Nat := colIdxAt nav.group_ nav.colNames nav.col_.cur.val
```
This naming is confusing. Consider `dispCol`/`origCol` or `curDispIdx`/`curOrigIdx`.

### 3. Redundant `DispIdx` newtype

`DispIdx` (Types.lean:9) is defined but underused. Either commit to it for type safety (use it in `curCol`) or remove it.

### 4. Cell accessor inefficiency

```lean
-- Types.lean:142-145
def str? : Cell → Option String | .str s => some s | _ => none
def int? : Cell → Option Int64 | .int n => some n | _ => none
```
These should use `@[inline]` for elimination of intermediate `Option` in tight loops.

### 5. `partial` functions without termination proofs

```lean
-- App.lean:19,39,68
partial def mainLoop ...
partial def loopRO ...
partial def runViewerMod ...
```
Event loops are inherently non-terminating, but `runViewerMod` recurses on column deletion. Consider using a fuel parameter or proving termination via decreasing column count.

### 6. Missing `@[inline]` on hot paths

The render path should inline more aggressively:
- `cellStyle` (Render.lean:46)
- `styleFg`/`styleBg` (Render.lean:42-43)
- `Column.get` (Types.lean:49)

### 7. Unnecessary boxing in `fmtInt`

```lean
-- Types.lean:76-87
def fmtInt (n : Int64) : String :=
  let v := n.toInt  -- boxing to arbitrary precision Int
  let s := s!"{v.natAbs}"
  ...
```
`Int64` -> `Int` -> `Nat` -> `String` is wasteful. Consider direct digit extraction from `Int64`.

---

## Architectural Suggestions

### 1. Make `colWidths.size = nCols` a typeclass law

```lean
class ReadTable (α : Type) where
  nRows     : α → Nat
  colNames  : α → Array String
  colWidths : α → Array Nat
  cell      : α → Nat → Nat → String
  -- Law: widths matches columns
  widths_eq_cols : ∀ a, (colWidths a).size = (colNames a).size
```

### 2. Consider `Subtype` for cursor bounds instead of existential proofs

Current approach works but is verbose. Alternative:
```lean
structure NavState (t : Type) [ReadTable t] where
  tbl : t
  row : Fin (ReadTable.nRows tbl)
  col : Fin (ReadTable.colNames tbl).size
```
This requires `nRows`/`colNames` to be in `Type` context, which is more complex but eliminates proof fields.

### 3. Split `PureKey` by category

```lean
inductive PureKey where
  | j | k | l | h | g | G | _0 | _1 | dollar | c_d | c_u
  | colJump (idx : DispIdx)
  | asc | desc | D | I | dup | swap
  ...
```
This is a flat union. Consider:
```lean
inductive NavKey where | j | k | l | h | ...
inductive ViewKey where | asc | desc | ...
inductive PureKey where | nav (k : NavKey) | view (k : ViewKey) | ...
```

### 4. Use `StateT` for view loop

Current explicit state threading:
```lean
partial def mainLoop ... (nav : NavState ...) (view : ViewState ...) (gPrefix : Bool) : IO ...
```
Could be:
```lean
abbrev ViewM := StateT (NavState × ViewState × Bool) IO
```

---

## Summary

| Aspect           | Rating | Notes                                                  |
|--------          |--------|-------                                                 |
| Type Safety      | 4/5    | Good `Fin` use, but `sorry` holes undermine guarantees |
| Typeclass Design | 5/5    | Clean hierarchy, minimal surface                       |
| Naming           | 3/5    | `curCol`/`curColIdx` confusion                         |
| Performance      | 4/5    | Zero-copy FFI is excellent; needs more `@[inline]`     |
| Idiomatic Lean   | 4/5    | Good overall; could use `StateT` for loops             |

## Priority Fixes

1. Fill `sorry` holes (especially `dispOrder_size` and `widths` invariant)
2. Add `@[inline]` to hot-path functions
3. Rename `curCol` -> `curDispCol` for clarity
4. Consider enforcing `colWidths.size` as typeclass law
