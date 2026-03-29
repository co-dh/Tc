/*
 * Table rendering: styles, formatting, column layout, unified render
 */
#include <lean/lean.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "term_core.h"
#include "heat.h"

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
#define STYLE_GROUP      8   // group/key column background
#define NUM_STYLES       9

// COL_INTS, COL_FLOATS, COL_STRS defined in heat.h

// | Minimum header text width (chars shown before truncation)
#define MIN_HDR_WIDTH 3
#define MAX_DISP_WIDTH 50

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

// | Float display: DEFAULT_PREC decimals at precAdj=0, up to MAX_PREC (double has 17 sig digits)
#define DEFAULT_PREC 3
#define MAX_PREC 17

// | Format value from Column at row index (precAdj adjusts float decimals)
static int format_col_cell(lean_obj_arg col, size_t row, char* buf, size_t buflen, int precAdj) {
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
        int prec = DEFAULT_PREC + precAdj;
        if (prec < 0) prec = 0;
        if (prec > MAX_PREC) prec = MAX_PREC;
        return snprintf(buf, buflen, "%.*f", prec, f);
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
static int compute_data_width(lean_obj_arg col, size_t r0, size_t r1, int precAdj) {
    int w = 1;  // minimum 1 char
    char buf[256];
    for (size_t r = r0; r < r1; r++) {
        int len = format_col_cell(col, r, buf, sizeof(buf), precAdj);
        if (len > w) w = len;
    }
    return w;
}

// | Print padded string (UTF-8 aware)
static void print_pad(int x, int y, int w, uint32_t fg, uint32_t bg, const char* s, int right) {
    int len = (int)utf8_len(s, w);
    if (len > w) len = w;
    int pad = w - len, cx = x;
    if (right) for (int i = 0; i < pad; i++) tb_set_cell(cx++, y, ' ', fg, bg);
    const char *p = s;
    for (int i = 0; i < len && *p; i++) {
        uint32_t ch = utf8_decode(&p);
        tb_set_cell(cx++, y, ch, fg, bg);
    }
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
// precAdj: precision adjustment for floats, widthAdj: column width offset
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
    b_lean_obj_arg hiddenCols,  // Array Nat - hidden column indices (width=1)
    b_lean_obj_arg styles,
    int64_t precAdj,          // precision adjustment for floats
    int64_t widthAdj,         // column width offset
    uint8_t heatMode,         // 0=off, 1=numeric, 2=categorical, 3=both
    b_lean_obj_arg sparklines, // Array String - sparkline strings per column (empty = off)
    lean_obj_arg world)
{
    int screenW = screen_w();
    int screenH = screen_h();
    size_t nCols = lean_array_size(allCols);
    size_t nRows = r1 - r0;  // visible row count
    size_t nFmts = lean_array_size(fmts);  // format chars available?
    char buf[256];

    // sparkline: active if array is non-empty and has at least one non-empty string
    size_t nSparklines = lean_array_size(sparklines);
    int sparkOn = 0;
    for (size_t i = 0; i < nSparklines && !sparkOn; i++) {
        const char *sp = lean_string_cstr(lean_array_get_core(sparklines, i));
        if (sp[0]) sparkOn = 1;
    }

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

    // build hidden column bitset
    uint64_t hidBits[4] = {0};
    build_sel_bits(hiddenCols, lean_array_size(hiddenCols), hidBits);

    // compute base widths for ALL columns (before widthAdj)
    // C owns all width logic: compute, cap, cache. Lean stores base widths opaquely.
    // hidden columns get width 1 (just a separator marker)
    // NOTE: only scan visible rows [r0,r1) for width - scanning all rows kills perf
    int* baseWidths = malloc(nCols * sizeof(int));  // base width (no widthAdj)
    int* allWidths = malloc(nCols * sizeof(int));    // render width (with widthAdj)
    size_t nInWidths = lean_array_size(inWidths);
    for (size_t c = 0; c < nCols; c++) {
        if (IS_SEL(hidBits, c)) { baseWidths[c] = 0; allWidths[c] = 3; continue; }
        int base;
        int cached = (c < nInWidths) ? lean_unbox(lean_array_get_core(inWidths, c)) : 0;
        lean_obj_arg col = lean_array_get_core(allCols, c);
        int dw = compute_data_width(col, r0, r1, (int)precAdj);  // visible rows only
        base = (dw > MIN_HDR_WIDTH ? dw : MIN_HDR_WIDTH) + 2;
        if (cached > base) base = cached;  // keep max of cached and current
        baseWidths[c] = base;  // uncapped, for cache
        int disp = base > MAX_DISP_WIDTH ? MAX_DISP_WIDTH : base;  // cap for display
        int w = disp + (int)widthAdj;
        if (w < 3) w = 3;  // minimum width
        allWidths[c] = w;
    }

    // compute x positions: key columns pinned left, then scrollable columns
    size_t nDispCols = lean_array_size(colIdxs);
    size_t* dispIdxs = malloc(nDispCols * sizeof(size_t));  // indices into colIdxs
    int* xs = malloc(nDispCols * sizeof(int));
    int* ws = malloc(nDispCols * sizeof(int));
    size_t nVisCols = 0;

    // compute pinned key width
    int keyWidth = 0;
    for (size_t c = 0; c < nKeys && c < nDispCols; c++) {
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, c));
        keyWidth += allWidths[origIdx] + 1;  // width + separator
    }

    // find cursor's display index (position in colIdxs, relative to non-key columns)
    size_t curDispIdx = 0;
    for (size_t c = nKeys; c < nDispCols; c++) {
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, c));
        if (origIdx == curCol) { curDispIdx = c - nKeys; break; }
    }

    // adjust colOff so cursor column is visible (using actual display widths)
    int scrollW = screenW - keyWidth;  // available width for non-key columns
    if (scrollW < 1) scrollW = 1;

    // if cursor is before current offset, snap offset to cursor
    if (curDispIdx < colOff) colOff = curDispIdx;

    // if cursor's right edge is past visible area, increase offset
    // compute cumulative width from colOff to cursor (inclusive)
    for (;;) {
        int cumX = 0;
        for (size_t c = colOff; c <= curDispIdx && (nKeys + c) < nDispCols; c++) {
            size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, nKeys + c));
            cumX += allWidths[origIdx] + 1;
        }
        if (cumX <= scrollW || colOff >= curDispIdx) break;
        colOff++;
    }

    // 1. Always render key columns first (pinned)
    int x = 0;
    for (size_t c = 0; c < nKeys && c < nDispCols && x < screenW; c++) {
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, c));
        int w = allWidths[origIdx];
        if (x + w > screenW) w = screenW - x;
        dispIdxs[nVisCols] = c;
        xs[nVisCols] = x;
        ws[nVisCols] = w;
        nVisCols++;
        x += w + 1;
    }
    size_t visKeys = nVisCols;  // number of visible key columns

    // 2. Then render non-key columns starting from colOff (adjusted for keys)
    size_t nonKeyStart = nKeys + colOff;  // skip key cols, apply scroll offset
    for (size_t c = nonKeyStart; c < nDispCols && x < screenW; c++) {
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, c));
        int w = allWidths[origIdx];
        if (x + w > screenW) w = screenW - x;
        dispIdxs[nVisCols] = c;
        xs[nVisCols] = x;
        ws[nVisCols] = w;
        nVisCols++;
        x += w + 1;
    }

    // expand cursor column to absorb trailing screen whitespace
    // all columns keep their normal positions; cursor col gets wider, cols after shift right
    if (nVisCols > 0) {
        // compute trailing whitespace after last visible column
        size_t last = nVisCols - 1;
        int usedW = xs[last] + ws[last] + 1;  // +1 for separator
        int slack = screenW - usedW;
        if (slack > 0) {
            // find cursor among visible columns
            size_t curVisIdx = nVisCols;
            for (size_t c = 0; c < nVisCols; c++) {
                size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, dispIdxs[c]));
                if (origIdx == curCol) { curVisIdx = c; break; }
            }
            if (curVisIdx < nVisCols) {
                // cap expansion by column's uncapped base width
                size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, dispIdxs[curVisIdx]));
                int base = baseWidths[origIdx] + (int)widthAdj;
                if (base < 3) base = 3;
                int expand = base - ws[curVisIdx];  // how much wider it wants to be
                if (expand > slack) expand = slack;
                if (expand > 0) {
                    ws[curVisIdx] += expand;
                    // shift columns after cursor right by expand
                    for (size_t c = curVisIdx + 1; c < nVisCols; c++)
                        xs[c] += expand;
                }
            }
        }
    }

    // header/footer (+ separators and type chars)
    int yFoot = screenH - 3;  // fixed: above status (h-2) and tab (h-1)
    for (size_t c = 0; c < nVisCols; c++) {
        size_t dispIdx = dispIdxs[c];  // index into colIdxs (pinned keys first)
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, dispIdx));
        const char* name = lean_string_cstr(lean_array_get_core(names, origIdx));
        int isSel = IS_SEL(colBits, origIdx);
        int isCur = (origIdx == curCol);
        int isGrp = (dispIdx < nKeys);  // group/key column
        // style for fg: cursor > sel > group > header
        int si = isCur ? STYLE_CURSOR : (isSel ? STYLE_SEL_COL : (isGrp ? STYLE_GROUP : STYLE_HEADER));
        uint32_t fg = stFg[si] | TB_BOLD | TB_UNDERLINE;
        // group cols keep group bg even when selected (only change fg)
        uint32_t bg = isGrp ? stBg[STYLE_GROUP] : stBg[si];
        // leading space
        tb_set_cell(xs[c], 0, ' ', fg, bg);
        tb_set_cell(xs[c], yFoot, ' ', fg, bg);
        // print header with 2 chars reserved for leading space + type char
        int hw = ws[c] > 2 ? ws[c] - 2 : 0;
        if (hw > 0) {
            print_pad(xs[c] + 1, 0, hw, fg, bg, name, 0);
            print_pad(xs[c] + 1, yFoot, hw, fg, bg, name, 0);
        }
        // type char at last position (VisiData style: # int, % float, ? bool, @ date)
        lean_obj_arg col = lean_array_get_core(allCols, origIdx);
        char tc = (origIdx < nFmts)
            ? type_char_fmt((char)lean_unbox_uint32(lean_array_get_core(fmts, origIdx)))
            : type_char_col(col);
        tb_set_cell(xs[c] + ws[c] - 1, 0, tc, fg, bg);
        tb_set_cell(xs[c] + ws[c] - 1, yFoot, tc, fg, bg);
        // separator after each column
        int sX = xs[c] + ws[c];
        if (sX < screenW) {
            uint32_t sc = (c + 1 == visKeys) ? 0x2551 : 0x2502;  // ║ after keys, │ otherwise
            uint32_t sf = (c + 1 == visKeys) ? stFg[STYLE_GROUP] : stFg[STYLE_DEFAULT];
            tb_set_cell(sX, 0, sc, sf, stBg[STYLE_DEFAULT]);
            tb_set_cell(sX, yFoot, sc, sf, stBg[STYLE_DEFAULT]);
        }
    }

    // sparkline row at y=1 (below header, above data)
    if (sparkOn) {
        uint32_t spFg = stFg[STYLE_HEADER];
        uint32_t spBg = stBg[STYLE_DEFAULT];
        for (size_t c = 0; c < nVisCols; c++) {
            size_t dispIdx = dispIdxs[c];
            size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, dispIdx));
            const char *sp = (origIdx < nSparklines)
                ? lean_string_cstr(lean_array_get_core(sparklines, origIdx)) : "";
            print_pad(xs[c], 1, ws[c], spFg, spBg, sp, 0);
            // separator
            int sX = xs[c] + ws[c];
            if (sX < screenW) {
                uint32_t sc = (c + 1 == visKeys) ? 0x2551 : 0x2502;
                tb_set_cell(sX, 1, sc, stFg[STYLE_DEFAULT], stBg[STYLE_DEFAULT]);
            }
        }
    }

    int dataY0 = sparkOn ? 2 : 1;  // data rows start after sparkline row

    HeatCol hcols[MAX_HEAT_COLS] = {{0}};
    if (heatMode && nVisCols <= MAX_HEAT_COLS)
        heat_scan(allCols, colIdxs, dispIdxs, nVisCols, nRows, r0, fmts, nFmts, hcols);

    // render data rows (r0..r1 in original table, 0..nRows in screen)
    for (size_t ri = 0; ri < nRows; ri++) {
        uint64_t row = r0 + ri;  // original table row
        int y = ri + dataY0;     // screen y (shifted down when sparklines active)
        int isSelRow = IS_SEL(rowBits, row);
        int isCurRow = (row == curRow);

        for (size_t c = 0; c < nVisCols; c++) {
            size_t dispIdx = dispIdxs[c];
            size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, dispIdx));
            int isSel = IS_SEL(colBits, origIdx);
            int isCurCol = (origIdx == curCol);
            int isGrp = (dispIdx < nKeys);
            int si = get_style(isCurRow && isCurCol, isSelRow, isSel, isCurRow, isCurCol);
            // group columns keep group bg even when selected (only change fg)
            uint32_t bg = isGrp ? stBg[STYLE_GROUP] : stBg[si];
            uint32_t fg = stFg[si];

            lean_obj_arg col = lean_array_get_core(allCols, origIdx);

            if (heatMode && heat_cell_bg(col, row, c, si, heatMode, hcols, &bg))
                fg = HEAT_FG;
            format_col_cell(col, row, buf, sizeof(buf), (int)precAdj);
            // leading space
            tb_set_cell(xs[c], y, ' ', fg, bg);
            // print cell with 2 chars reserved for leading+trailing space
            int cw = ws[c] > 2 ? ws[c] - 2 : 0;
            if (cw > 0) print_pad(xs[c] + 1, y, cw, fg, bg, buf, col_is_num(col));
            // trailing space
            tb_set_cell(xs[c] + ws[c] - 1, y, ' ', fg, bg);
            // separator after each column
            {
                int sX = xs[c] + ws[c];
                if (sX < screenW) {
                    uint32_t sc = (c + 1 == visKeys) ? 0x2551 : 0x2502;  // ║ after keys, │ otherwise
                    tb_set_cell(sX, y, sc, stFg[STYLE_DEFAULT], stBg[STYLE_DEFAULT]);
                }
            }
        }
    }

    // tooltip: show full header name if truncated (overlay on header row)
    // moveDir > 0 (moved right): tooltip extends left; moveDir < 0: extends right
    for (size_t c = 0; c < nVisCols; c++) {
        size_t dispIdx = dispIdxs[c];  // correct for both key and non-key columns
        size_t origIdx = lean_unbox(lean_array_get_core(colIdxs, dispIdx));
        if (origIdx != curCol) continue;  // only for focused column
        const char* name = lean_string_cstr(lean_array_get_core(names, origIdx));
        int nameLen = (int)utf8_len(name, 256);
        int colW = ws[c] - 2;  // available width for name (minus leading space + type char)
        if (nameLen <= colW) break;  // fits, no tooltip needed
        uint32_t fg = stFg[STYLE_CURSOR] | TB_BOLD | TB_UNDERLINE;
        const char* p = name;
        if (moveDir > 0) {
            // moved right: tooltip extends left (so it doesn't cover next column)
            int endX = xs[c] + ws[c] - 1;  // last char position (type char spot)
            int startX = endX - nameLen;
            if (startX < 0) startX = 0;
            int tipW = endX - startX;
            // skip to the right portion of name
            int skip = nameLen - tipW;
            for (int i = 0; i < skip && *p; i++) utf8_decode(&p);
            for (int i = 0; i < tipW && *p; i++)
                tb_set_cell(startX + i, 0, utf8_decode(&p), fg, stBg[STYLE_CURSOR]);
        } else {
            // moved left or no move: tooltip extends right
            int maxW = screenW - xs[c] - 1;
            int tipW = nameLen < maxW ? nameLen : maxW;
            for (int i = 0; i < tipW && *p; i++)
                tb_set_cell(xs[c] + 1 + i, 0, utf8_decode(&p), fg, stBg[STYLE_CURSOR]);
        }
        break;
    }

    // return base widths (no widthAdj, 0 for hidden → recompute when unhidden)
    lean_object* outWidths = lean_alloc_array(nCols, nCols);
    for (size_t c = 0; c < nCols; c++) {
        lean_array_set_core(outWidths, c, lean_box(baseWidths[c]));
    }

    free(dispIdxs);
    free(xs);
    free(ws);
    free(baseWidths);
    free(allWidths);
    return lean_io_result_mk_ok(outWidths);
}
