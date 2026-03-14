// Heatmap: numeric column coloring by value
#ifndef HEAT_H
#define HEAT_H

#include <lean/lean.h>
#include <stdint.h>
#include <stddef.h>

// Column type tags (matches Lean Column inductive order)
#ifndef COL_INTS
#define COL_INTS   0
#define COL_FLOATS 1
#endif

#define MAX_HEAT_COLS 256
#define HEAT_FG 16  // black text on colored bg

// Per-column heatmap state (min/max range for visible rows)
typedef struct {
    double mn, mx;
    int active;  // 1 if numeric with range
} HeatCol;

// Extract numeric value from Column at row. Returns 1 if valid, 0 if NaN/non-numeric.
int col_num_val(lean_obj_arg col, size_t row, double *out);

// 5-stop blue→red gradient (256-color indices).
// Snap to nearest stop rather than interpolate — 256-color mode's 6×6×6 cube
// produces muddy intermediate colors. 5 hand-picked stops give clean, distinct bands.
uint32_t heat_color(double t);

// Scan visible rows to compute per-column min/max for numeric columns.
// adapts contrast to what's on screen and avoids scanning all rows for ADBC tables.
void heat_scan(b_lean_obj_arg allCols, b_lean_obj_arg colIdxs,
               size_t *dispIdxs, size_t nVisCols, size_t nRows, uint64_t r0,
               HeatCol *cols);

// Return heatmap bg color for a cell, or -1 if no override applies.
// si: style index (cursor/selection styles suppress heatmap).
int heat_cell_bg(lean_obj_arg col, uint64_t row, size_t c,
                 int si, const HeatCol *cols, uint32_t *bg);

#endif
