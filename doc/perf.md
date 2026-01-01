# Render Performance

Measured on sample.csv/sample.parquet (10001 rows, 42 columns).

| Backend | Render Time | FPS |
|---------|-------------|-----|
| C (parquet) | 7-11μs | ~90,000+ |
| Lean (CSV) | 230,000μs | ~4 |

## Analysis

C version is ~20,000x faster because:
- Direct termbox buffer writes in tight loop
- Zero allocation per frame
- Batch processing

Lean version bottlenecks:
- Per-cell FFI calls to `Term.printPad`
- String allocation per cell
- No batch processing
