// Heatmap: numeric column coloring by value
#include "heat.h"
#include <math.h>

// style indices that suppress heatmap (must match term_shim.c STYLE_* defines)
#define STYLE_CURSOR   0
#define STYLE_SEL_ROW  1
#define STYLE_SEL_CUR  2

int col_num_val(lean_obj_arg col, size_t row, double *out) {
    unsigned tag = lean_obj_tag(col);
    lean_obj_arg data = lean_ctor_get(col, 0);
    if (tag == COL_INTS) {
        *out = (double)(int64_t)lean_unbox_uint64(lean_array_get_core(data, row));
        return 1;
    } else if (tag == COL_FLOATS) {
        lean_obj_arg fbox = lean_array_get_core(data, row);
        double v = lean_ctor_get_float(fbox, 0);
        if (isnan(v)) return 0;
        *out = v;
        return 1;
    }
    return 0;
}

uint32_t heat_color(double t) {
    static const uint32_t stops[] = {27, 39, 77, 220, 196};
    if (t <= 0.0) return stops[0];
    if (t >= 1.0) return stops[4];
    double pos = t * 4.0;
    int lo = (int)pos;
    if (lo >= 4) lo = 3;
    return (pos - lo < 0.5) ? stops[lo] : stops[lo + 1];
}

static int col_is_num(lean_obj_arg col) {
    unsigned tag = lean_obj_tag(col);
    return tag == COL_INTS || tag == COL_FLOATS;
}

void heat_scan(b_lean_obj_arg allCols, b_lean_obj_arg colIdxs,
               size_t *dispIdxs, size_t nVisCols, size_t nRows, uint64_t r0,
               HeatCol *cols) {
    for (size_t c = 0; c < nVisCols && c < MAX_HEAT_COLS; c++) {
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, dispIdxs[c]));
        lean_obj_arg col = lean_array_get_core(allCols, origIdx);
        cols[c].active = 0;
        if (!col_is_num(col)) continue;
        double mn = 1e308, mx = -1e308;
        for (size_t ri = 0; ri < nRows; ri++) {
            double v;
            if (!col_num_val(col, r0 + ri, &v)) continue;
            if (v < mn) mn = v;
            if (v > mx) mx = v;
        }
        if (mx > mn) { cols[c].mn = mn; cols[c].mx = mx; cols[c].active = 1; }
    }
}

int heat_cell_bg(lean_obj_arg col, uint64_t row, size_t c,
                 int si, const HeatCol *cols, uint32_t *bg) {
    if (c >= MAX_HEAT_COLS || !cols[c].active) return 0;
    if (si == STYLE_CURSOR || si == STYLE_SEL_ROW || si == STYLE_SEL_CUR) return 0;
    double v;
    if (!col_num_val(col, row, &v)) return 0;
    double t = (v - cols[c].mn) / (cols[c].mx - cols[c].mn);
    *bg = heat_color(t);
    return 1;
}
