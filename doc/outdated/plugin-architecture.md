# Plugin Architecture Analysis

## Interface a Backend Must Implement

4 typeclasses, ~20 functions total:

```lean
class ReadTable α where
  nRows     : α → Nat
  colNames  : α → Array String
  totalRows : α → Nat

class ModifyTable α extends ReadTable α where
  delCols : Array Nat → α → IO α
  sortBy  : Array Nat → Bool → α → IO α

class QueryTable α where
  queryMeta : α → IO MetaTuple
  queryFreq : α → Array Nat → IO FreqTuple
  filter    : α → String → IO (Option α)
  distinct  : α → Nat → IO (Array String)
  findRow   : α → Nat → String → Nat → Bool → IO (Option Nat)

class RenderTable α [ReadTable α] where
  render : NavState → Array Nat → ... → IO (Array Nat)
```

## Data Types Crossing Boundary

```lean
inductive Cell where
  | null | int (v : Int64) | float (v : Float) | str (v : String) | bool (v : Bool)

inductive Column where
  | ints (data : Array Int64) | floats (data : Array Float) | strs (data : Array String)

inductive Table where
  | mem : MemTable → Table | adbc : AdbcTable → Table | kdb : KdbTable → Table
```

## Current C FFI Pattern

```c
// c/adbc_shim.c - backend as static .a
lean_obj_res lean_adbc_query(b_lean_obj_arg sql_obj, lean_obj_arg world) {
    const char* sql = lean_string_cstr(sql_obj);
    // ... execute via libduckdb ...
    return lean_io_result_mk_ok(lean_alloc_external(get_qr_class(), qr));
}
```

```lean
-- Tc/Data/ADBC/FFI.lean
@[extern "lean_adbc_query"]
opaque query : @& String → IO QueryResult
```

## Coupling Points

| File | Coupling |
|------|----------|
| `Table.lean` | Closed sum with 170 lines of instance dispatch |
| `App.lean` | Hardcoded imports and `AdbcTable.init/shutdown` |
| `View.fromFile` | Hardcoded `.csv → MemTable`, else `AdbcTable` |

## Approaches

| Approach | Pros | Cons |
|----------|------|------|
| Compile-time flags | Zero overhead, type-safe | Must recompile |
| Separate executables | ~30MB each vs 50MB | Can't mix backends |
| Process-based (IPC) | True runtime plugins | Serialization overhead |

## Recommended: Multi-Target Build

```lean
-- lakefile.lean
lean_exe tc          -- MemTable only (stdin/csv)
lean_exe tc-duckdb   -- + AdbcTable (parquet)
lean_exe tc-full     -- + KdbTable
```

Sizes:
- `tc` ~25MB (minimal)
- `tc-duckdb` ~35MB (+ DuckDB)
- `tc-full` ~50MB (all backends)

## Lean .so Plugin Research

### Yes, Lean 4 CAN Build Shared Libraries

From [Lake documentation](https://lean-lang.org/doc/reference/latest/Build-Tools-and-Distribution/Lake/):

```lean
-- lakefile.lean
lean_lib MyPlugin where
  defaultFacets := #["shared"]  -- builds .so/.dylib/.dll
```

Or via command line:
```bash
lake build MyPlugin:shared
```

### Two Loading Mechanisms

1. **`--plugin`** - For Lean code (tactics, macros)
   ```bash
   lean --plugin=./plugin.so file.lean
   ```

2. **`--load-dynlib`** - For foreign functions (C FFI)
   ```bash
   lean --load-dynlib=./backend.so file.lean
   ```

### Runtime Duplication Question

Each .so plugin compiled from Lean DOES include Lean runtime components.
However, the OS dynamic linker can share read-only segments between processes.

For Tc backends:
- Plugin .so would contain: backend code + Lean runtime stubs
- Main tc would contain: full Lean runtime
- Memory: Some duplication, but not as bad as static linking each

### Plugin Interface Pattern

```lean
-- plugin/Backend.lean
structure MyBackend where
  handle : UInt64

@[export my_backend_open]
def open (path : String) : IO MyBackend := ...

@[export my_backend_nrows]
def nRows (b : MyBackend) : Nat := ...

-- Main app loads via --load-dynlib and calls exported functions
```

### References

- [Lake documentation](https://lean-lang.org/doc/reference/latest/Build-Tools-and-Distribution/Lake/)
- [Lake library facets](https://leanprover-community.github.io/mathlib4_docs/Lake/Build/Facets.html)
- [Using foreign functions with --plugin](https://leanprover-community.github.io/archive/stream/270676-lean4/topic/Using.20foreign.20functions.20with.20--plugin.html)
- [lean4-plugin-example](https://github.com/cpehle/lean4-plugin-example)

### Conclusion

Lean 4 supports .so plugins via:
1. `lake build lib:shared` - produces .so
2. `--load-dynlib` - loads at runtime for FFI
3. `@[export name]` - exports Lean functions to C ABI

This enables true runtime plugins, though with some Lean runtime overhead per plugin.

## Implementation Status

### Completed (Phase 1-3)

1. **Dead import removed**: `Types.lean` no longer imports `ADBC.FFI`

2. **C FFI split**:
   - `c/adbc_core.c` - Generic ADBC + DuckDB driver: query, cells, formatting
   - `lean_adbc_init_driver(driver, entrypoint)` - parameterized init

3. **Table variants**:
   - `Tc/Table/Mem.lean` - Table with only `.mem` variant
   - `Tc/Table/Full.lean` - Table with `.mem | .adbc | .kdb`

4. **Backend abstraction**:
   - `Tc/Backend/Core.lean` - no-op init/shutdown
   - `Tc/Backend/Full.lean` - AdbcTable.init/shutdown
   - `Tc/Backend.lean` - re-exports Full for backwards compatibility

### Remaining Work

1. **App variants**: Create `Tc/App/Core.lean` that:
   - Uses `Table.Mem` instead of `Table`
   - Uses `Backend.Core` instead of `Backend.Full`
   - Removes kdb:// and folder view support

2. **Lakefile targets**: Add conditional build targets:
   ```lean
   lean_exe «tc-core» where root := `Tc.App.Core
   lean_exe «tc-duckdb» where root := `Tc.App
   ```

3. **View dispatch**: Make `View.fromFile` backend-aware:
   - Core: `.csv` only, error on parquet
   - DuckDB: `.csv` → MemTable, else AdbcTable
