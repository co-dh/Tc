# Plan: Pure Functional Idioms for Tc

## 1. Lenses/Optics (High Impact)

**Problem**: Nested state updates are verbose and error-prone
```lean
-- Current pattern (deeply nested):
{ a with stk := a.stk.setCur { v with nav := { nav with row := ... } } }
```

**Solution**: Simple lens combinators
```lean
structure Lens (s a : Type) where
  get : s → a
  set : a → s → s

def Lens.modify (l : Lens s a) (f : a → a) (s : s) : s := l.set (f (l.get s)) s

-- Usage: viewNavRowLens.modify (·.move 1) view
```

**Files**: New `Tc/Lens.lean`, update `View.lean`, `Nav.lean`

---

## 2. Effect Monoid (Medium Impact)

**Problem**: Can't compose effects, must handle sequentially

**Solution**: Make Effect appendable for batching
```lean
instance : Append Effect where
  append := fun e1 e2 => match e1 with | .none => e2 | _ => e1

-- Allows: eff1 ++ eff2 (first non-none wins, or sequence them)
```

**Files**: `Tc/Effect.lean`

---

## 3. More Invariant Theorems (High Value)

**Problem**: Runtime errors possible, proofs incomplete

**Theorems to add**:
```lean
-- Nav.lean: cursor bounds
theorem row_cursor_valid (n : NavState nr nc t) : n.row.cur.val < nr := n.row.cur.isLt

-- ViewStack: non-empty invariant (already subtype, add more)
theorem viewstack_cur_exists (s : ViewStack) : s.val.size > 0 := s.property

-- Toggle involution (complete the sorry)
theorem toggle_toggle [BEq α] [LawfulBEq α] (a : Array α) (x : α) (h : !a.contains x) :
    (a.toggle x).toggle x = a

-- dispOrder preserves size (complete the sorry)
theorem dispOrder_size (group names : Array String) :
    (dispOrder group names).size = names.size
```

**Files**: `Tc/Nav.lean`, `Tc/Types.lean`

---

## 4. Kleisli/Option Combinators (Medium Impact)

**Problem**: Chained Option matching is verbose
```lean
-- Current:
match f x with | some y => g y | none => none
```

**Solution**: Use bind/map consistently
```lean
-- Already available: f x >>= g
-- Add: Option.andThen, Option.orElse usage
```

**Files**: Throughout codebase, style cleanup

---

## 5. Smart Constructors (Already Partial)

**Current**: `View.fromTbl` returns `Option View`

**Extend**: Add more validated constructors
```lean
def NavState.new (tbl : t) (h1 : nRows > 0) (h2 : nCols > 0) : NavState nRows nCols t
```

---

## Priority Order

1. **Theorems** - Fill in `sorry` proofs, add cursor bounds theorems
2. **Kleisli style** - Cleanup Option chaining with >>=
3. **Lenses** - Add Lens module for View/Nav updates
4. **Effect Monoid** - If needed for batching

## Estimated Scope

- Theorems: 2-3 files, ~50 lines
- Kleisli cleanup: style changes across 5+ files
- Lenses: new module ~80 lines, updates ~20 lines
- Effect Monoid: ~10 lines
