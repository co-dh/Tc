// Heatmap: column coloring by value (numeric gradient, string categorical)
#include "heat.h"
#include <math.h>
#include <string.h>

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

// FNV-1a hash → [0, 1] for categorical string coloring.
// Same string always gets same color.
static double str_hash01(const char *s) {
    uint32_t h = 2166136261u;
    while (*s) { h ^= (unsigned char)*s++; h *= 16777619u; }
    return (double)(h & 0xFFFF) / 65535.0;
}

// Extract digits from date/time string → monotonic double.
// "2026-03-13 18:17:48" → 20260313181748.0
static double date_to_num(const char *s) {
    double v = 0;
    while (*s) {
        if (*s >= '0' && *s <= '9') v = v * 10 + (*s - '0');
        s++;
    }
    return v;
}

// Check if format char indicates date/time type
static int is_date_fmt(char fmt) {
    return fmt == 't';  // Arrow date/time/timestamp all use 't' prefix
}

uint32_t heat_color(double t) {
    // Viridis-inspired purple→teal→green→yellow ramp via xterm-256 cube.
    // Each adjacent pair differs by one RGB channel step → clean transitions.
    // Perceptually uniform, colorblind-safe, no aggressive red.
    static const uint32_t ramp[] = {
        53, 54, 55,            // dark purple → purple → blue-purple
        61, 25, 31,            // indigo → blue → teal-blue
        30, 36, 42,            // teal → green-teal → bright teal
        41, 77, 113,           // green → light green → yellow-green
        149, 148, 184,         // chartreuse → yellow-green → dark yellow
        190, 226,              // bright yellow → yellow
    };
    static const int N = sizeof(ramp) / sizeof(ramp[0]);
    if (t <= 0.0) return ramp[0];
    if (t >= 1.0) return ramp[N - 1];
    double pos = t * (N - 1);
    int lo = (int)pos;
    if (lo >= N - 1) lo = N - 2;
    return (pos - lo < 0.5) ? ramp[lo] : ramp[lo + 1];
}

void heat_scan(b_lean_obj_arg allCols, b_lean_obj_arg colIdxs,
               size_t *dispIdxs, size_t nVisCols, size_t nRows, uint64_t r0,
               b_lean_obj_arg fmts, size_t nFmts,
               HeatCol *cols) {
    for (size_t c = 0; c < nVisCols && c < MAX_HEAT_COLS; c++) {
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, dispIdxs[c]));
        lean_obj_arg col = lean_array_get_core(allCols, origIdx);
        cols[c].kind = HEAT_NONE;
        unsigned tag = lean_obj_tag(col);
        char fmt = (origIdx < nFmts)
            ? (char)lean_unbox_uint32(lean_array_get_core(fmts, origIdx)) : 0;
        cols[c].date = 0;
        if (tag == COL_INTS || tag == COL_FLOATS) {
            double mn = 1e308, mx = -1e308;
            for (size_t ri = 0; ri < nRows; ri++) {
                double v;
                if (!col_num_val(col, r0 + ri, &v)) continue;
                if (v < mn) mn = v;
                if (v > mx) mx = v;
            }
            if (mx > mn) { cols[c].mn = mn; cols[c].mx = mx; cols[c].kind = HEAT_NUM; }
        } else if (tag == COL_STRS && is_date_fmt(fmt)) {
            // Date/time strings: extract digits → numeric gradient
            lean_obj_arg data = lean_ctor_get(col, 0);
            double mn = 1e308, mx = -1e308;
            for (size_t ri = 0; ri < nRows; ri++) {
                const char *s = lean_string_cstr(lean_array_get_core(data, r0 + ri));
                if (!s[0]) continue;
                double v = date_to_num(s);
                if (v < mn) mn = v;
                if (v > mx) mx = v;
            }
            if (mx > mn) { cols[c].mn = mn; cols[c].mx = mx; cols[c].kind = HEAT_NUM; cols[c].date = 1; }
        } else if (tag == COL_STRS) {
            // Regular strings: categorical hash coloring
            lean_obj_arg data = lean_ctor_get(col, 0);
            const char *first = NULL;
            int diverse = 0;
            for (size_t ri = 0; ri < nRows && !diverse; ri++) {
                const char *s = lean_string_cstr(lean_array_get_core(data, r0 + ri));
                if (!first) first = s;
                else if (strcmp(first, s) != 0) diverse = 1;
            }
            if (diverse) cols[c].kind = HEAT_STR;
        }
    }
}

int heat_cell_bg(lean_obj_arg col, uint64_t row, size_t c,
                 int si, uint8_t mode, const HeatCol *cols, uint32_t *bg) {
    if (c >= MAX_HEAT_COLS || cols[c].kind == HEAT_NONE) return 0;
    if (si == STYLE_CURSOR || si == STYLE_SEL_ROW || si == STYLE_SEL_CUR) return 0;
    // mode: 1=numeric(HEAT_NUM), 2=categorical(HEAT_STR), 3=both
    if (cols[c].kind == HEAT_NUM && !(mode & 1)) return 0;
    if (cols[c].kind == HEAT_STR && !(mode & 2)) return 0;
    double t;
    if (cols[c].kind == HEAT_NUM) {
        double v;
        if (cols[c].date) {
            lean_obj_arg data = lean_ctor_get(col, 0);
            const char *s = lean_string_cstr(lean_array_get_core(data, row));
            if (!s[0]) return 0;
            v = date_to_num(s);
        } else {
            if (!col_num_val(col, row, &v)) return 0;
        }
        t = (v - cols[c].mn) / (cols[c].mx - cols[c].mn);
    } else if (cols[c].kind == HEAT_STR) {
        lean_obj_arg data = lean_ctor_get(col, 0);
        const char *s = lean_string_cstr(lean_array_get_core(data, row));
        if (!s[0]) return 0;
        t = str_hash01(s);
    } else {
        return 0;
    }
    *bg = heat_color(t);
    return 1;
}
