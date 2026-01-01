/*
 * termbox2 FFI shim for Lean 4
 */
#include <lean/lean.h>
#include <termbox2.h>

// tb_init() -> Int32
lean_obj_res lean_tb_init(lean_obj_arg world) {
    int r = tb_init();
    return lean_io_result_mk_ok(lean_box((uint32_t)(int32_t)r));
}

// tb_shutdown() -> Unit
lean_obj_res lean_tb_shutdown(lean_obj_arg world) {
    tb_shutdown();
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_width() -> UInt32
lean_obj_res lean_tb_width(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)tb_width()));
}

// tb_height() -> UInt32
lean_obj_res lean_tb_height(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)tb_height()));
}

// tb_clear() -> Unit
lean_obj_res lean_tb_clear(lean_obj_arg world) {
    tb_clear();
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_present() -> Unit
lean_obj_res lean_tb_present(lean_obj_arg world) {
    tb_present();
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_set_cell(x, y, ch, fg, bg) -> Unit
lean_obj_res lean_tb_set_cell(uint32_t x, uint32_t y, uint32_t ch,
                               uint32_t fg, uint32_t bg, lean_obj_arg world) {
    tb_set_cell((int)x, (int)y, ch, fg, bg);
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_poll_event() -> Event
lean_obj_res lean_tb_poll_event(lean_obj_arg world) {
    struct tb_event ev;
    tb_poll_event(&ev);

    // Create Event structure
    // Lean 4 sorts scalars by SIZE DESC, then declaration order
    // UInt32 (ch,w,h) | UInt16 (key) | UInt8 (type,mod) = 16 bytes
    lean_object* obj = lean_alloc_ctor(0, 0, 16);
    uint8_t* data = (uint8_t*)lean_ctor_scalar_cptr(obj);
    // Layout: UInt32s by decl order, then UInt16, then UInt8s by decl order
    *(uint32_t*)(data + 0) = ev.ch;           // ch: 1st UInt32
    *(uint32_t*)(data + 4) = (uint32_t)ev.w;  // w: 2nd UInt32
    *(uint32_t*)(data + 8) = (uint32_t)ev.h;  // h: 3rd UInt32
    *(uint16_t*)(data + 12) = ev.key;         // key: UInt16
    data[14] = ev.type;                       // type: 1st UInt8
    data[15] = ev.mod;                        // mod: 2nd UInt8
    return lean_io_result_mk_ok(obj);
}

// tb_print_pad(x, y, w, fg, bg, str, right_align) -> Unit
// Prints string left/right aligned, padded to width - all in C
lean_obj_res lean_tb_print_pad(uint32_t x, uint32_t y, uint32_t w,
                                uint32_t fg, uint32_t bg,
                                lean_obj_arg str, uint8_t right, lean_obj_arg world) {
    const char *s = lean_string_cstr(str);
    size_t len = 0;
    for (const char *p = s; *p && len < w; p++, len++) {}  // count up to w
    size_t pad = w > len ? w - len : 0;
    uint32_t cx = x;
    if (right) {  // right align: pad first
        for (size_t i = 0; i < pad; i++) tb_set_cell((int)cx++, (int)y, ' ', fg, bg);
    }
    for (const char *p = s; *p && (cx - x) < w; p++) {
        tb_set_cell((int)cx++, (int)y, (uint32_t)(unsigned char)*p, fg, bg);
    }
    if (!right) {  // left align: pad after
        for (size_t i = 0; i < pad; i++) tb_set_cell((int)cx++, (int)y, ' ', fg, bg);
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_render_col(x, w, y0, fgs, bgs, strs, rights) -> Unit
// Batch render a column: one row per array element starting at y0
lean_obj_res lean_tb_render_col(uint32_t x, uint32_t w, uint32_t y0,
                                 lean_obj_arg fgs, lean_obj_arg bgs,
                                 lean_obj_arg strs, lean_obj_arg rights,
                                 lean_obj_arg world) {
    size_t n = lean_array_size(strs);
    for (size_t i = 0; i < n; i++) {
        uint32_t fg = lean_unbox_uint32(lean_array_get_core(fgs, i));
        uint32_t bg = lean_unbox_uint32(lean_array_get_core(bgs, i));
        uint8_t right = lean_unbox(lean_array_get_core(rights, i));
        lean_obj_arg str = lean_array_get_core(strs, i);
        const char *s = lean_string_cstr(str);
        size_t len = 0;
        for (const char *p = s; *p && len < w; p++, len++) {}
        size_t pad = w > len ? w - len : 0;
        uint32_t cx = x;
        uint32_t y = y0 + (uint32_t)i;
        if (right) {
            for (size_t j = 0; j < pad; j++) tb_set_cell((int)cx++, (int)y, ' ', fg, bg);
        }
        for (const char *p = s; *p && (cx - x) < w; p++) {
            tb_set_cell((int)cx++, (int)y, (uint32_t)(unsigned char)*p, fg, bg);
        }
        if (!right) {
            for (size_t j = 0; j < pad; j++) tb_set_cell((int)cx++, (int)y, ' ', fg, bg);
        }
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* === Unified Table Rendering === */

// | Style indices (shared with Lean)
#define STYLE_CURSOR     0
#define STYLE_SEL_ROW    1
#define STYLE_SEL_CUR    2
#define STYLE_SEL_COL    3
#define STYLE_CUR_ROW    4
#define STYLE_CUR_COL    5
#define STYLE_DEFAULT    6
#define NUM_STYLES       7

// | Cell type tags (matches Lean Cell inductive)
#define CELL_NULL  0
#define CELL_INT   1
#define CELL_FLOAT 2
#define CELL_STR   3
#define CELL_BOOL  4

// | Format integer with comma separators
static int fmt_int_comma(char* buf, size_t buflen, int64_t v) {
    char tmp[32];
    int neg = v < 0;
    uint64_t u = neg ? (uint64_t)(-(v+1)) + 1 : (uint64_t)v;
    int i = 0;
    do { tmp[i++] = '0' + (u % 10); u /= 10; } while (u);
    int len = i + (i - 1) / 3 + neg;
    if ((size_t)len >= buflen) return snprintf(buf, buflen, "%ld", (long)v);
    char* p = buf;
    if (neg) *p++ = '-';
    int g = (i - 1) % 3 + 1;
    while (i > 0) {
        *p++ = tmp[--i];
        if (--g == 0 && i > 0) { *p++ = ','; g = 3; }
    }
    *p = '\0';
    return len;
}

// | Format Cell to buffer (Cell.null is scalar, others are ctors)
static int format_cell(lean_obj_arg cell, char* buf, size_t buflen) {
    // .null is represented as scalar 0
    if (lean_is_scalar(cell)) { buf[0] = '\0'; return 0; }
    unsigned tag = lean_obj_tag(cell);
    switch (tag) {
    case CELL_INT: {
        lean_obj_arg v = lean_ctor_get(cell, 0);
        if (lean_is_scalar(v)) {
            int64_t n = lean_scalar_to_int64(v);
            return fmt_int_comma(buf, buflen, n);
        } else {
            // Big int - just print "INT" for now
            return snprintf(buf, buflen, "INT");
        }
    }
    case CELL_FLOAT: return snprintf(buf, buflen, "%.3f", lean_unbox_float(lean_ctor_get(cell, 0)));
    case CELL_STR: {
        const char* s = lean_string_cstr(lean_ctor_get(cell, 0));
        size_t len = strlen(s);
        if (len >= buflen) len = buflen - 1;
        memcpy(buf, s, len); buf[len] = '\0';
        return len;
    }
    case CELL_BOOL: return snprintf(buf, buflen, "%s", lean_unbox(lean_ctor_get(cell, 0)) ? "true" : "false");
    default: buf[0] = '\0'; return 0;
    }
}

// | Check if cell is numeric
static int cell_is_num(lean_obj_arg cell) {
    if (lean_is_scalar(cell)) return 0;  // .null
    unsigned tag = lean_obj_tag(cell);
    return tag == CELL_INT || tag == CELL_FLOAT;
}

// | Print padded string
static void print_pad(int x, int y, int w, uint32_t fg, uint32_t bg, const char* s, int right) {
    int len = strlen(s);
    if (len > w) len = w;
    int pad = w - len, cx = x;
    if (right) for (int i = 0; i < pad; i++) tb_set_cell(cx++, y, ' ', fg, bg);
    for (int i = 0; i < len; i++) tb_set_cell(cx++, y, (uint32_t)(unsigned char)s[i], fg, bg);
    if (!right) for (int i = 0; i < pad; i++) tb_set_cell(cx++, y, ' ', fg, bg);
}

// | Get cell style
static int get_style(int isCursor, int isSelRow, int isSel, int isCurRow, int isCurCol) {
    if (isCursor)          return STYLE_CURSOR;
    if (isSelRow)          return STYLE_SEL_ROW;
    if (isSel && isCurRow) return STYLE_SEL_CUR;
    if (isSel)             return STYLE_SEL_COL;
    if (isCurRow)          return STYLE_CUR_ROW;
    if (isCurCol)          return STYLE_CUR_COL;
    return STYLE_DEFAULT;
}

// | Build selection bitset
static void build_sel_bits(b_lean_obj_arg arr, size_t n, uint64_t* bits) {
    bits[0] = bits[1] = bits[2] = bits[3] = 0;
    for (size_t i = 0; i < n; i++) {
        size_t v = lean_unbox(lean_array_get_core(arr, i));
        if (v < 256) bits[v / 64] |= 1ULL << (v % 64);
    }
}
#define IS_SEL(bits, v) ((v) < 256 && ((bits)[(v)/64] & (1ULL << ((v)%64))))

// | Unified table render - works with Array (Array Cell)
// cols: column-major cell data (only visible columns, indexed 0..visCols-1)
// colIdxs: original column indices for each visible column
// r0: starting row in original table (for selection check)
lean_obj_res lean_render_table(
    b_lean_obj_arg cols,      // Array (Array Cell) - visible columns only
    b_lean_obj_arg names,     // Array String - all column names
    b_lean_obj_arg widths,    // Array Nat - all column widths
    b_lean_obj_arg colIdxs,   // Array Nat - original indices of visible cols
    uint64_t nKeys,           // number of key columns (for separator)
    uint64_t colOff,          // column offset (for separator calc)
    uint64_t r0,              // first row index (in original table)
    uint64_t curRow, uint64_t curCol,
    b_lean_obj_arg selCols,
    b_lean_obj_arg selRows,
    b_lean_obj_arg styles,
    lean_obj_arg world)
{
    int screenW = tb_width();
    size_t nVisCols = lean_array_size(cols);
    size_t nRows = nVisCols > 0 ? lean_array_size(lean_array_get_core(cols, 0)) : 0;
    char buf[64];

    // extract styles
    uint32_t stFg[NUM_STYLES], stBg[NUM_STYLES];
    for (int s = 0; s < NUM_STYLES; s++) {
        stFg[s] = lean_unbox_uint32(lean_array_get_core(styles, s * 2));
        stBg[s] = lean_unbox_uint32(lean_array_get_core(styles, s * 2 + 1));
    }

    // build selection bitsets
    uint64_t colBits[4], rowBits[4];
    build_sel_bits(selCols, lean_array_size(selCols), colBits);
    build_sel_bits(selRows, lean_array_size(selRows), rowBits);

    // compute x positions for visible columns
    int* xs = malloc(nVisCols * sizeof(int));
    int* ws = malloc(nVisCols * sizeof(int));
    int x = 0;
    for (size_t c = 0; c < nVisCols && x < screenW; c++) {
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, c));
        int w = lean_unbox(lean_array_get_core(widths, origIdx));
        if (x + w > screenW) w = screenW - x;
        xs[c] = x;
        ws[c] = w;
        x += w + 1;
    }

    // separator position (after last visible key column)
    size_t visKeys = (colOff < nKeys) ? nKeys - colOff : 0;
    if (visKeys > nVisCols) visKeys = nVisCols;
    int sepX = visKeys > 0 ? xs[visKeys - 1] + ws[visKeys - 1] : 0;

    // header/footer
    int yFoot = nRows + 1;
    for (size_t c = 0; c < nVisCols; c++) {
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, c));
        const char* name = lean_string_cstr(lean_array_get_core(names, origIdx));
        int isSel = IS_SEL(colBits, origIdx);
        int isCur = (origIdx == curCol);
        int si = isCur ? STYLE_CURSOR : (isSel ? STYLE_SEL_COL : STYLE_DEFAULT);
        uint32_t fg = stFg[si] | 0x02000000;  // underline
        print_pad(xs[c], 0, ws[c], fg, stBg[si], name, 0);
        print_pad(xs[c], yFoot, ws[c], fg, stBg[si], name, 0);
    }
    if (sepX > 0) {
        tb_set_cell(sepX, 0, '|', stFg[STYLE_DEFAULT], stBg[STYLE_DEFAULT]);
        tb_set_cell(sepX, yFoot, '|', stFg[STYLE_DEFAULT], stBg[STYLE_DEFAULT]);
    }

    // render data rows
    for (size_t ri = 0; ri < nRows; ri++) {
        uint64_t row = r0 + ri;
        int y = ri + 1;
        int isSelRow = IS_SEL(rowBits, row);
        int isCurRow = (row == curRow);

        for (size_t c = 0; c < nVisCols; c++) {
            size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, c));
            int isSel = IS_SEL(colBits, origIdx);
            int isCurCol = (origIdx == curCol);
            int si = get_style(isCurRow && isCurCol, isSelRow, isSel, isCurRow, isCurCol);

            lean_obj_arg col = lean_array_get_core(cols, c);
            lean_obj_arg cell = lean_array_get_core(col, ri);
            format_cell(cell, buf, sizeof(buf));
            print_pad(xs[c], y, ws[c], stFg[si], stBg[si], buf, cell_is_num(cell));
        }
        if (sepX > 0) tb_set_cell(sepX, y, '|', stFg[STYLE_DEFAULT], stBg[STYLE_DEFAULT]);
    }

    free(xs);
    free(ws);
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_buffer_str() -> String (screen as string, no escape sequences)
lean_obj_res lean_tb_buffer_str(lean_obj_arg world) {
    int w = tb_width();
    int h = tb_height();
    struct tb_cell *buf = tb_cell_buffer();
    if (!buf || w <= 0 || h <= 0) {
        char hdr[64];
        snprintf(hdr, sizeof(hdr), "(no buffer: %dx%d buf=%p)\n", w, h, (void*)buf);
        return lean_io_result_mk_ok(lean_mk_string(hdr));
    }
    // w chars per row + newline, for h rows
    char *str = malloc((size_t)(w + 1) * h + 1);
    if (!str) return lean_io_result_mk_ok(lean_mk_string("(malloc fail)"));
    size_t pos = 0;
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            uint32_t ch = buf[y * w + x].ch;
            str[pos++] = (ch >= 32 && ch < 127) ? (char)ch : ' ';
        }
        str[pos++] = '\n';
    }
    str[pos] = '\0';
    lean_object *res = lean_mk_string(str);
    free(str);
    return lean_io_result_mk_ok(res);
}
