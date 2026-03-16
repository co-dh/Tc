// Heatmap: column coloring by value (numeric gradient, string categorical)
#ifndef HEAT_H
#define HEAT_H

#include <lean/lean.h>
#include <stdint.h>
#include <stddef.h>

// Column type tags (matches Lean Column inductive order)
#ifndef COL_INTS
#define COL_INTS   0
#define COL_FLOATS 1
#define COL_STRS   2
#endif

#define MAX_HEAT_COLS 256
#define HEAT_FG 16  // black text on colored bg

// Heat kind: how to map cell value → color
#define HEAT_NONE 0  // inactive
#define HEAT_NUM  1  // numeric min/max gradient
#define HEAT_STR  2  // string categorical (hash-based)

// Per-column heatmap state
typedef struct {
    double mn, mx;   // numeric range (HEAT_NUM only)
    int kind;        // HEAT_NONE, HEAT_NUM, or HEAT_STR
    char date;       // 1 if HEAT_NUM from date string (needs date_to_num in cell_bg)
} HeatCol;

// Extract numeric value from Column at row. Returns 1 if valid, 0 if NaN/non-numeric.
int col_num_val(lean_obj_arg col, size_t row, double *out);

// Viridis-inspired purple→teal→green→yellow ramp (xterm-256 cube indices).
// Adjacent stops differ by one RGB channel step for clean transitions.
uint32_t heat_color(double t);

// Scan visible rows to compute per-column heatmap state.
// Numeric and date columns get min/max gradient, other strings get categorical hash.
// fmts: format chars from Arrow schema ('t'=date/time), nFmts: length of fmts array.
void heat_scan(b_lean_obj_arg allCols, b_lean_obj_arg colIdxs,
               size_t *dispIdxs, size_t nVisCols, size_t nRows, uint64_t r0,
               b_lean_obj_arg fmts, size_t nFmts,
               HeatCol *cols);

// Return heatmap bg color for a cell. Returns 1 if color applied, 0 otherwise.
// si: style index (cursor/selection styles suppress heatmap).
int heat_cell_bg(lean_obj_arg col, uint64_t row, size_t c,
                 int si, const HeatCol *cols, uint32_t *bg);

#endif
