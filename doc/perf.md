# Render Performance

Measured on sample.csv/sample.parquet (10001 rows, 42 columns).

| Backend | Render Time | FPS | Notes |
|---------|-------------|-----|-------|
| C (parquet) | 7-11μs | ~90,000+ | Direct termbox writes |
| Lean (CSV) v1 | 230,000μs | ~4 | Per-char FFI calls |
| Lean (CSV) v2 | 52,000μs | ~19 | Batch printPad FFI |
| Lean (CSV) v3 | 16μs | ~60,000+ | Unified C render |

## Optimization History

**v1→v2**: Added `lean_tb_print_pad` C function that handles padding in C.
Eliminated per-character setCell FFI calls. **4.4x speedup**.

**v2→v3**: Unified C render (`lean_render_table`) that takes Cell arrays directly.
- Pass raw Cell data to C, format numbers in C (no Lean string alloc)
- Cell uses Int64 instead of Int (avoids MPZ boxing)
- Single FFI call renders entire visible table
- **3,250x speedup** (52ms → 16μs)

## Final Result

CSV render now matches parquet performance (~16μs vs 7-11μs).
Total improvement: **14,375x** (230ms → 16μs).
