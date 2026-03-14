// Heatmap: numeric column coloring by value
#include "heat.h"
#include <math.h>

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
