# Rust ADBC FFI Plan

## Goal
Replace `c/adbc_core.c` (668 lines) with Rust, keeping dynamic `libduckdb.so` loading.

## Key Constraint
NO static DuckDB bundling - use `libloading` for runtime dlopen.

## Binary Size Impact

| Format | Static lib size | Notes |
|--------|-----------------|-------|
| C shim (current) | 71 KB | Manual Arrow parsing |
| Rust + Arrow IPC | 8.9 MB | Read/write RecordBatch |
| Rust + Parquet | 16 MB | +7 MB for parquet codec |

**Trade-off:** 125x size increase for memory safety + maintainability.

Arrow IPC format:
- Simpler than Parquet (no compression codec)
- Zero-copy compatible
- Can serialize query results to binary

## Architecture

```
rust/
  Cargo.toml
  src/
    lib.rs          # extern "C" for Lean
    adbc.rs         # ADBC structs, dlopen via libloading
    query.rs        # QueryResult, cell access
    format.rs       # cell formatting
```

## Deps (Cargo.toml)

```toml
[package]
name = "adbc_rs"
version = "0.1.0"
edition = "2024"

[lib]
crate-type = ["staticlib"]

[dependencies]
libloading = "0.8"      # dlopen/dlsym
arrow = { version = "54", default-features = false, features = ["ipc", "ffi"] }
libc = "0.2"

[profile.release]
lto = true
opt-level = "z"
strip = true
```

No `duckdb` crate - load `libduckdb.so` at runtime.

## ADBC Loading

```rust
use libloading::{Library, Symbol};

struct AdbcDriver {
    lib: Library,
    db_new: Symbol<'static, fn(*mut AdbcDatabase, *mut AdbcError) -> u8>,
    // ...
}

impl AdbcDriver {
    fn load(path: &str) -> Result<Self, Error> {
        let lib = unsafe { Library::new(path)? };
        let db_new = unsafe { lib.get(b"AdbcDatabaseNew")? };
        // ...
    }
}
```

## Arrow FFI

Use `arrow::ffi` for safe conversion from C Arrow structs:

```rust
use arrow::ffi::{FFI_ArrowSchema, FFI_ArrowArray};

// ADBC returns ArrowArrayStream -> convert to RecordBatch
let batch = unsafe { arrow::ffi::from_ffi(schema_ptr, array_ptr)? };
```

## Arrow IPC (binary serialization)

```rust
use arrow::ipc::writer::FileWriter;

let mut writer = FileWriter::try_new(&mut file, &batch.schema())?;
writer.write(&batch)?;
writer.finish()?;
```

## Implementation Steps

1. Setup `rust/Cargo.toml`, basic structure
2. ADBC structs - mirror C exactly
3. Driver loading via `libloading`
4. Arrow FFI - safe batch conversion
5. QueryResult - `Vec<RecordBatch>` + prefix sums
6. Cell access - safe downcasting
7. Lean FFI - extern "C" matching current interface

## Verification

1. `lake build` succeeds
2. `lake exe test` passes
3. `./tc` opens parquet/DuckDB correctly
4. valgrind clean

## Files

Create:
- `rust/Cargo.toml`
- `rust/src/{lib,adbc,query,format}.rs`

Modify:
- `lakefile.lean` - add rust extern_lib

---

## In-Memory Table: Rust vs Lean

### Rust Options

| Crate | Size | Notes |
|-------|------|-------|
| Polars | 24 MB | Full DataFrame, SIMD, parallel |
| DataFusion | 6.8 MB+ | SQL engine on Arrow |
| Arrow only | 8.9 MB | Raw RecordBatch, manual ops |

### Current Lean MemTable (~300 lines)

Already implements in pure Lean:
- `sort` - multi-column qsort
- `filter` - expr parsing ("a == 1 && b == 'x'")
- `delCols` - column deletion
- `distinct` - unique values
- `selCols` - column selection
- `take` - first N rows
- `queryFreq` - group by + count/pct/bar

### Recommendation

**Keep Lean MemTable** - no Rust needed for in-memory ops.

| | Lean MemTable | Polars |
|--|---------------|--------|
| Binary | 0 | +24 MB |
| Complexity | 300 lines | huge dep tree |
| Performance | O(n log n) | SIMD, parallel |

Polars only wins on large data (>1M rows) with parallel execution.
For typical interactive use, Lean is sufficient.

### Scope

Use Rust **only for ADBC FFI** (Arrow parsing safety).
Keep MemTable in pure Lean.

---

## Arrow Type Coverage (Current C Shim)

### Handled (26 types)

| Format | Type | Format | Type |
|--------|------|--------|------|
| `l/L` | int64/uint64 | `i/I` | int32/uint32 |
| `s/S` | int16/uint16 | `c/C` | int8/uint8 |
| `g` | float64 | `f` | float32 |
| `e` | float16 | `b` | bool |
| `u/U` | utf8/large_utf8 | `z/Z` | binary/large_binary |
| `d:p,s` | decimal128 | `ts` | timestamp |
| `tt` | time64 | `td` | date32 |
| `tD` | duration | `+l/+L` | list/large_list |
| `+w:n` | fixed_size_list | `+s` | struct |

### Missing (~10 types)

| Format | Type | Risk |
|--------|------|------|
| `n` | null | low |
| `tD` | date64 | medium |
| `tss:tz` | timestamp w/ timezone | medium |
| `+m` | map | medium |
| `+ud/+us` | union (dense/sparse) | low |
| `vu/vz` | utf8_view/binary_view | Arrow 1.4+ |

### Known Bugs in C Shim

1. **decimal128**: Only reads low 64 bits - wrong for large values
2. **timestamp**: Hardcoded microseconds, ignores unit (`tsn/tsu/tsm/tss`)
3. **time64**: Same - ignores unit
4. **fixed_size_list**: Assumes float64 child, breaks for other types
5. **list/large_list**: Assumes int64 child, breaks for other types

### Why Rust Helps

Rust `arrow` crate handles all types via `DataType` enum + `downcast_ref`:

```rust
match array.data_type() {
    DataType::Int64 => array.as_primitive::<Int64Type>().value(i),
    DataType::Utf8 => array.as_string::<i32>().value(i),
    DataType::Timestamp(unit, tz) => /* handles unit + tz */,
    DataType::Decimal128(p, s) => /* full 128-bit */,
    // ... exhaustive match
}
```

No manual buffer parsing, no forgotten type units.

---

## Full Rust Rewrite Analysis

### Current Project

| Metric | Value |
|--------|-------|
| Lean code | 4,347 lines |
| C FFI code | 1,554 lines |
| Binary size | 13 MB |

### Equivalent Rust (estimated)

| Component | Crate | Binary |
|-----------|-------|--------|
| TUI | ratatui + crossterm | 302 KB |
| Arrow/ADBC | arrow + libloading | +41 KB |
| Async | tokio (minimal) | included |
| **Total** | | **~350 KB - 2 MB** |

### Gains (Rust)

| Aspect | Benefit |
|--------|---------|
| Binary size | 13 MB → ~1-2 MB (10x smaller) |
| Compile time | lake ~30s vs cargo ~15s |
| Ecosystem | ratatui, polars, arrow, serde, tokio |
| Memory safety | Built-in, no manual FFI |
| Tooling | cargo, clippy, rust-analyzer mature |
| Hiring | More Rust devs than Lean devs |

### Losses (Rust)

| Aspect | Loss |
|--------|------|
| Type proofs | No dependent types, no `#guard`, no theorems |
| Algebraic effects | No `do` notation for custom monads |
| Metaprogramming | Macros weaker than Lean's `macro_rules` |
| Pattern matching | No structural recursion proofs |
| Learning investment | Existing Lean codebase knowledge |

### Key Trade-off

```
Lean: Correctness by construction (proofs, theorems)
Rust: Correctness by testing (strong types, but no proofs)
```

### Recommendation

| Goal | Choice |
|------|--------|
| Correctness proofs | Keep Lean |
| Smaller binary, faster iteration | Rust |
| Best of both | **Lean core + Rust FFI** (current path) |

Hybrid approach gives:
- Proofs where they matter (data transformations)
- Safety where C is risky (Arrow parsing)
