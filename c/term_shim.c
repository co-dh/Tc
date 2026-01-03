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
#define STYLE_HEADER     7
#define NUM_STYLES       8

// | Column type tags (matches Lean Column inductive)
#define COL_INTS   0
#define COL_FLOATS 1
#define COL_STRS   2

// | Minimum header text width (chars shown before truncation)
#define MIN_HDR_WIDTH 3

// | VisiData-style type chars from Arrow format
// # int, % float, ? bool, @ date, space string
static char type_char_fmt(char fmt) {
    switch (fmt) {
    case 'l': case 'i': case 's': case 'c':  // signed int
    case 'L': case 'I': case 'S': case 'C':  // unsigned int
        return '#';
    case 'g': case 'f': case 'e':  // float
        return '%';
    case 'b':  // bool
        return '?';
    case 't':  // date/time (td/ts/tt)
        return '@';
    default:
        return ' ';
    }
}

// | Type char from Column tag (fallback when no format info)
static char type_char_col(lean_obj_arg col) {
    switch (lean_obj_tag(col)) {
    case COL_INTS:   return '#';
    case COL_FLOATS: return '%';
    default:         return ' ';
    }
}

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

// | Format value from Column at row index
static int format_col_cell(lean_obj_arg col, size_t row, char* buf, size_t buflen) {
    unsigned tag = lean_obj_tag(col);
    lean_obj_arg data = lean_ctor_get(col, 0);
    switch (tag) {
    case COL_INTS: {
        int64_t v = (int64_t)lean_unbox_uint64(lean_array_get_core(data, row));
        return fmt_int_comma(buf, buflen, v);
    }
    case COL_FLOATS: {
        // Float stored as boxed object in array
        lean_obj_arg fbox = lean_array_get_core(data, row);
        double f = lean_ctor_get_float(fbox, 0);
        if (f != f) { buf[0] = '\0'; return 0; }  // NaN = null
        return snprintf(buf, buflen, "%.3f", f);
    }
    case COL_STRS: {
        const char* s = lean_string_cstr(lean_array_get_core(data, row));
        if (!s[0]) { buf[0] = '\0'; return 0; }  // empty = null
        size_t len = strlen(s);
        if (len >= buflen) len = buflen - 1;
        memcpy(buf, s, len); buf[len] = '\0';
        return len;
    }
    default: buf[0] = '\0'; return 0;
    }
}

// | Check if column is numeric (for right-align)
static int col_is_num(lean_obj_arg col) {
    unsigned tag = lean_obj_tag(col);
    return tag == COL_INTS || tag == COL_FLOATS;
}

// | Compute column data width (not including header)
static int compute_data_width(lean_obj_arg col, size_t r0, size_t r1) {
    int w = 1;  // minimum 1 char
    char buf[256];
    for (size_t r = r0; r < r1; r++) {
        int len = format_col_cell(col, r, buf, sizeof(buf));
        if (len > w) w = len;
    }
    return w;
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

// | Unified table render - reads Column directly, computes widths if needed
// allCols: Array Column (ALL columns)
// fmts: Array Char - format chars for type indicators (empty = use Column tag)
// colIdxs: display order indices
// nTotalRows: total rows in table (for width calc)
// moveDir: -1 = moved left, 0 = none, 1 = moved right (for tooltip direction)
// inWidths: input widths (empty = compute), returns computed widths
lean_obj_res lean_render_table(
    b_lean_obj_arg allCols,   // Array Column - ALL columns
    b_lean_obj_arg names,     // Array String - all column names
    b_lean_obj_arg fmts,      // Array Char - format chars (empty = use Column tag)
    b_lean_obj_arg inWidths,  // Array Nat - input widths (empty = compute)
    b_lean_obj_arg colIdxs,   // Array Nat - display order (all cols, keys first)
    uint64_t nTotalRows,      // total row count
    uint64_t nKeys,           // number of key columns (for separator)
    uint64_t colOff,          // first visible column offset
    uint64_t r0, uint64_t r1, // visible row range [r0, r1)
    uint64_t curRow, uint64_t curCol,
    int64_t moveDir,          // -1 = left, 0 = none, 1 = right
    b_lean_obj_arg selCols,
    b_lean_obj_arg selRows,
    b_lean_obj_arg styles,
    lean_obj_arg world)
{
    int screenW = tb_width();
    size_t nCols = lean_array_size(allCols);
    size_t nRows = r1 - r0;  // visible row count
    size_t nFmts = lean_array_size(fmts);  // format chars available?
    char buf[256];

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

    // compute or use widths for ALL columns (+2 for leading space + type char)
    // use max(data_width, MIN_HDR_WIDTH) + 2 for minimum header visibility
    int* allWidths = malloc(nCols * sizeof(int));
    int needCompute = lean_array_size(inWidths) == 0;
    for (size_t c = 0; c < nCols; c++) {
        if (needCompute) {
            lean_obj_arg col = lean_array_get_core(allCols, c);
            int dw = compute_data_width(col, 0, nTotalRows);
            allWidths[c] = (dw > MIN_HDR_WIDTH ? dw : MIN_HDR_WIDTH) + 2;
        } else {
            allWidths[c] = lean_unbox(lean_array_get_core(inWidths, c));
        }
    }

    // compute x positions for visible columns (starting from colOff)
    size_t nDispCols = lean_array_size(colIdxs);
    int* xs = malloc(nDispCols * sizeof(int));
    int* ws = malloc(nDispCols * sizeof(int));
    size_t nVisCols = 0;  // actual visible count
    int x = 0;
    for (size_t c = colOff; c < nDispCols && x < screenW; c++) {
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, c));
        int w = allWidths[origIdx];
        if (x + w > screenW) w = screenW - x;
        xs[nVisCols] = x;
        ws[nVisCols] = w;
        nVisCols++;
        x += w + 1;
    }

    // visible key columns (for double separator ║ after keys)
    size_t visKeys = (colOff < nKeys) ? nKeys - colOff : 0;
    if (visKeys > nVisCols) visKeys = nVisCols;

    // header/footer (+ separators and type chars)
    int yFoot = nRows + 1;
    for (size_t c = 0; c < nVisCols; c++) {
        size_t dispIdx = colOff + c;  // index into colIdxs
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, dispIdx));
        const char* name = lean_string_cstr(lean_array_get_core(names, origIdx));
        int isSel = IS_SEL(colBits, origIdx);
        int isCur = (origIdx == curCol);
        int si = isCur ? STYLE_CURSOR : (isSel ? STYLE_SEL_COL : STYLE_HEADER);
        uint32_t fg = stFg[si] | TB_BOLD | TB_UNDERLINE;
        // leading space
        tb_set_cell(xs[c], 0, ' ', fg, stBg[si]);
        tb_set_cell(xs[c], yFoot, ' ', fg, stBg[si]);
        // print header with 2 chars reserved for leading space + type char
        int hw = ws[c] > 2 ? ws[c] - 2 : 0;
        if (hw > 0) {
            print_pad(xs[c] + 1, 0, hw, fg, stBg[si], name, 0);
            print_pad(xs[c] + 1, yFoot, hw, fg, stBg[si], name, 0);
        }
        // type char at last position (VisiData style: # int, % float, ? bool, @ date)
        lean_obj_arg col = lean_array_get_core(allCols, origIdx);
        char tc = (origIdx < nFmts)
            ? type_char_fmt((char)lean_unbox_uint32(lean_array_get_core(fmts, origIdx)))
            : type_char_col(col);
        tb_set_cell(xs[c] + ws[c] - 1, 0, tc, fg, stBg[si]);
        tb_set_cell(xs[c] + ws[c] - 1, yFoot, tc, fg, stBg[si]);
        // separator after each column except last
        if (c + 1 < nVisCols) {
            int sX = xs[c] + ws[c];  // x after column content
            uint32_t sc = (c + 1 == visKeys) ? 0x2551 : 0x2502;  // ║ after keys, │ otherwise
            tb_set_cell(sX, 0, sc, stFg[STYLE_DEFAULT], stBg[STYLE_DEFAULT]);
            tb_set_cell(sX, yFoot, sc, stFg[STYLE_DEFAULT], stBg[STYLE_DEFAULT]);
        }
    }

    // render data rows (r0..r1 in original table, 0..nRows in screen)
    for (size_t ri = 0; ri < nRows; ri++) {
        uint64_t row = r0 + ri;  // original table row
        int y = ri + 1;          // screen y
        int isSelRow = IS_SEL(rowBits, row);
        int isCurRow = (row == curRow);

        for (size_t c = 0; c < nVisCols; c++) {
            size_t dispIdx = colOff + c;
            size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, dispIdx));
            int isSel = IS_SEL(colBits, origIdx);
            int isCurCol = (origIdx == curCol);
            int si = get_style(isCurRow && isCurCol, isSelRow, isSel, isCurRow, isCurCol);

            lean_obj_arg col = lean_array_get_core(allCols, origIdx);
            format_col_cell(col, row, buf, sizeof(buf));
            // leading space
            tb_set_cell(xs[c], y, ' ', stFg[si], stBg[si]);
            // print cell with 2 chars reserved for leading+trailing space
            int cw = ws[c] > 2 ? ws[c] - 2 : 0;
            if (cw > 0) print_pad(xs[c] + 1, y, cw, stFg[si], stBg[si], buf, col_is_num(col));
            // trailing space
            tb_set_cell(xs[c] + ws[c] - 1, y, ' ', stFg[si], stBg[si]);
            // separator after each column except last
            if (c + 1 < nVisCols) {
                int sX = xs[c] + ws[c];
                uint32_t sc = (c + 1 == visKeys) ? 0x2551 : 0x2502;  // ║ after keys, │ otherwise
                tb_set_cell(sX, y, sc, stFg[STYLE_DEFAULT], stBg[STYLE_DEFAULT]);
            }
        }
    }

    // tooltip: show full header name if truncated (overlay on header row)
    // moveDir > 0 (moved right): tooltip extends left; moveDir < 0: extends right
    for (size_t c = 0; c < nVisCols; c++) {
        size_t dispIdx = colOff + c;
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, dispIdx));
        if (origIdx != curCol) continue;  // only for focused column
        const char* name = lean_string_cstr(lean_array_get_core(names, origIdx));
        int nameLen = strlen(name);
        int colW = ws[c] - 2;  // available width for name (minus leading space + type char)
        if (nameLen <= colW) break;  // fits, no tooltip needed
        uint32_t fg = stFg[STYLE_CURSOR] | TB_BOLD | TB_UNDERLINE;
        if (moveDir > 0) {
            // moved right: tooltip extends left (so it doesn't cover next column)
            int endX = xs[c] + ws[c] - 1;  // last char position (type char spot)
            int startX = endX - nameLen;
            if (startX < 0) startX = 0;
            int tipW = endX - startX;
            for (int i = 0; i < tipW; i++)
                tb_set_cell(startX + i, 0, (uint32_t)(unsigned char)name[nameLen - tipW + i], fg, stBg[STYLE_CURSOR]);
        } else {
            // moved left or no move: tooltip extends right
            int maxW = screenW - xs[c] - 1;
            int tipW = nameLen < maxW ? nameLen : maxW;
            for (int i = 0; i < tipW; i++)
                tb_set_cell(xs[c] + 1 + i, 0, (uint32_t)(unsigned char)name[i], fg, stBg[STYLE_CURSOR]);
        }
        break;
    }

    // build return Array Nat for widths
    lean_object* outWidths = lean_alloc_array(nCols, nCols);
    for (size_t c = 0; c < nCols; c++) {
        lean_array_set_core(outWidths, c, lean_box(allWidths[c]));
    }

    free(xs);
    free(ws);
    free(allWidths);
    return lean_io_result_mk_ok(outWidths);
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

// String -> Float (returns NaN on parse failure)
double lean_string_to_float(b_lean_obj_arg s) {
    const char *str = lean_string_cstr(s);
    char *end;
    double v = strtod(str, &end);
    return (end == str || *end != '\0') ? __builtin_nan("") : v;
}
