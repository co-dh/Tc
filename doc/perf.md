# Render Performance

Measured on sample.csv/sample.parquet (10001 rows, 42 columns).

| Backend | Render Time | FPS | Notes |
|---------|-------------|-----|-------|
| C (parquet) | 7-11μs | ~90,000+ | Direct termbox writes |
| Lean (CSV) v1 | 230,000μs | ~4 | Per-char FFI calls |
| Lean (CSV) v2 | 52,000μs | ~19 | Batch printPad FFI |

## Optimization

v1→v2: Added `lean_tb_print_pad` C function that handles padding in C.
Eliminated per-character setCell FFI calls. **4.4x speedup**.

## Remaining Bottleneck

Lean version still ~5000x slower than C due to:
- Cell.toString creates String per cell
- Still one FFI call per cell (vs batch in C)
- Lean loop overhead vs C tight loop
