# Universe Issue in View/ViewStack

## Problem

In Lean 4, types live in a hierarchy of "universes":
- `Type` = `Type 0` - regular types (Nat, String, Bool, etc.)
- `Type 1` - types whose values are themselves types in `Type 0`
- `Type 2` - and so on...

The original View used an existential to hide the table type:

```lean
structure View where
  t : Type              -- This field stores a TYPE, not a value
  nav : NavState ... t  -- Uses t as a type parameter
  instR : ReadTable t   -- Typeclass instance for t
  ...
```

Because `View` has a field `t : Type`, the structure itself must live in `Type 1`.
Then `ViewStack` contains `View`, so it's also in `Type 1`.

This breaks IO:
```lean
IO : Type → Type           -- IO only works with Type 0
Option : Type u → Type u   -- Option preserves universe

-- This fails:
def pushMeta : IO (Option ViewStack)  -- ViewStack : Type 1, but IO needs Type 0!
```

## Solution: Closed Sum Type

Instead of existential (open, any type), use a closed sum (fixed set of types):

```lean
inductive Table where
  | mem : MemTable → Table
  | adbc : AdbcTable → Table
```

## Trade-offs

| Aspect | Existential (Type 1) | Closed Sum (Type 0) |
|--------|---------------------|---------------------|
| Add new backend | Just implement typeclasses | Must modify Table enum |
| Code duplication | One generic View | Pattern match on variants |
| Type safety | Compiler enforces | Manual consistency |
| IO monad | Need unsafe | Works naturally |
| unsafeCast | Required | Not needed |
| Debugging | Hard (type erased) | Easy (concrete) |

## Decision

For this codebase with only 2 backends (MemTable, AdbcTable), the closed sum is better:
- "Open extension" is theoretical - adding a backend is major work anyway
- Simpler, safer, standard IO
- No unsafe/unsafeCast needed
