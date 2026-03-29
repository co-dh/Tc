/*
 * Terminal core: init/shutdown, event polling, buffer reading, print helpers
 */
#include <lean/lean.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include "termbox2.h"

// | Local time as "HH:MM:SS.mmm"
lean_obj_res lean_local_timestamp(lean_obj_arg world) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    struct tm tm;
    localtime_r(&ts.tv_sec, &tm);
    char buf[16];
    snprintf(buf, sizeof(buf), "%02d:%02d:%02d.%03d",
             tm.tm_hour, tm.tm_min, tm.tm_sec, (int)(ts.tv_nsec / 1000000));
    return lean_io_result_mk_ok(lean_mk_string(buf));
}

// Headless mode: fake cell buffer when tb_init fails (no tty)
struct tb_cell *fake_buf = NULL;
int fake_w = 80, fake_h = 24;
int headless = 0;
static int tb_inited = 0;  // true after lean_tb_init called (regardless of success/headless)

// | Check if terminal has been initialized (tb_init called)
lean_obj_res lean_tb_inited(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box(tb_inited ? 1 : 0));
}

// | Check if stdin is a tty (false = piped input)
lean_obj_res lean_isatty_stdin(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box(isatty(STDIN_FILENO) ? 1 : 0));
}

// | Reopen stdin from /dev/tty (for pipe mode after reading stdin)
lean_obj_res lean_reopen_tty(lean_obj_arg world) {
    if (freopen("/dev/tty", "r", stdin) == NULL) {
        return lean_io_result_mk_ok(lean_box(0));  // failed
    }
    return lean_io_result_mk_ok(lean_box(1));  // success
}

// tb_init() -> Int32
lean_obj_res lean_tb_init(lean_obj_arg world) {
    int r = tb_init();
    tb_inited = 1;
    if (r != 0) {
        // No terminal — headless mode (for -c tests)
        headless = 1;
        fake_buf = calloc(fake_w * fake_h, sizeof(struct tb_cell));
    } else {
        tb_set_output_mode(TB_OUTPUT_256);  // enable 256-color mode
        // Drain any pending escape sequence responses (cursor position, etc)
        struct tb_event ev;
        while (tb_peek_event(&ev, 50) > 0) { /* discard */ }
    }
    return lean_io_result_mk_ok(lean_box((uint32_t)(int32_t)r));
}

// tb_shutdown() -> Unit
lean_obj_res lean_tb_shutdown(lean_obj_arg world) {
    if (headless) { free(fake_buf); fake_buf = NULL; }
    else tb_shutdown();
    return lean_io_result_mk_ok(lean_box(0));
}

// | Get screen width (headless-aware)
int screen_w(void) {
    if (headless) return fake_w;
    int w = tb_width();
    return w < 1 ? 80 : w;
}

// | Get screen height (headless-aware)
int screen_h(void) {
    if (headless) return fake_h;
    int h = tb_height();
    return h < 1 ? 24 : h;
}

// tb_width() -> UInt32
lean_obj_res lean_tb_width(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)screen_w()));
}

// tb_height() -> UInt32
lean_obj_res lean_tb_height(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)screen_h()));
}

// tb_clear() -> Unit
lean_obj_res lean_tb_clear(lean_obj_arg world) {
    if (headless) { if (fake_buf) memset(fake_buf, 0, fake_w * fake_h * sizeof(struct tb_cell)); }
    else tb_clear();
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_present() -> Unit
lean_obj_res lean_tb_present(lean_obj_arg world) {
    if (!headless) tb_present();
    return lean_io_result_mk_ok(lean_box(0));
}

// tb_set_cell(x, y, ch, fg, bg) -> Unit
lean_obj_res lean_tb_set_cell(uint32_t x, uint32_t y, uint32_t ch,
                               uint32_t fg, uint32_t bg, lean_obj_arg world) {
    if (headless && fake_buf && (int)x < fake_w && (int)y < fake_h) {
        fake_buf[y * fake_w + x].ch = ch;
        fake_buf[y * fake_w + x].fg = fg;
        fake_buf[y * fake_w + x].bg = bg;
    } else if (!headless) {
        tb_set_cell((int)x, (int)y, ch, fg, bg);
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// Defined in sock_shim.c — non-zero when a socket command is pending
extern volatile char g_sock_cmd[256];

// tb_poll_event() -> Event
// Uses peek loop (100ms) so socket commands can interrupt blocking wait
lean_obj_res lean_tb_poll_event(lean_obj_arg world) {
    struct tb_event ev;
    memset(&ev, 0, sizeof(ev));
    if (headless) {
        usleep(16000);  // headless: no terminal events, just sleep briefly
    } else {
        // Loop: peek with timeout, break on real event or socket command.
        // IMPORTANT: rc < 0 must break immediately (with throttle) — Lean callers
        // like waitForQ loop on this, so a missing break causes 100% CPU spin.
        for (;;) {
            int rc = tb_peek_event(&ev, 100);
            if (rc < 0) { usleep(16000); break; }  // not initialized or error
            if (rc == 0 && ev.type != 0) break;     // 0 = success
            if (g_sock_cmd[0]) break;
        }
    }

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

// | Read termbox internal cell buffer as a string (rows separated by newlines)
lean_obj_res lean_tb_buffer_str(lean_obj_arg world) {
    int w = screen_w(), h = screen_h();
    struct tb_cell *buf = headless ? fake_buf : tb_cell_buffer();
    if (!buf) return lean_io_result_mk_ok(lean_mk_string(""));
    // worst case: 4 bytes per char + newline per row
    size_t cap = (size_t)w * h * 4 + h + 1;
    char *out = malloc(cap);
    size_t pos = 0;
    for (int y = 0; y < h; y++) {
        // find last non-space char in row for trimming
        int last = -1;
        for (int x = w - 1; x >= 0; x--) {
            uint32_t ch = buf[y * w + x].ch;
            if (ch != 0 && ch != ' ') { last = x; break; }
        }
        for (int x = 0; x <= last; x++) {
            uint32_t ch = buf[y * w + x].ch;
            if (ch == 0) ch = ' ';
            // encode as UTF-8
            if (ch < 0x80) { out[pos++] = (char)ch; }
            else if (ch < 0x800) { out[pos++] = 0xC0 | (ch >> 6); out[pos++] = 0x80 | (ch & 0x3F); }
            else if (ch < 0x10000) { out[pos++] = 0xE0 | (ch >> 12); out[pos++] = 0x80 | ((ch >> 6) & 0x3F); out[pos++] = 0x80 | (ch & 0x3F); }
            else { out[pos++] = 0xF0 | (ch >> 18); out[pos++] = 0x80 | ((ch >> 12) & 0x3F); out[pos++] = 0x80 | ((ch >> 6) & 0x3F); out[pos++] = 0x80 | (ch & 0x3F); }
        }
        out[pos++] = '\n';
    }
    out[pos] = '\0';
    lean_obj_res s = lean_mk_string_from_bytes(out, pos);
    free(out);
    return lean_io_result_mk_ok(s);
}

// | Headless-aware tb_set_cell wrapper (used by internal C rendering)
void hd_set_cell(int x, int y, uint32_t ch, uint32_t fg, uint32_t bg) {
    if (headless) {
        if (fake_buf && x >= 0 && x < fake_w && y >= 0 && y < fake_h) {
            fake_buf[y * fake_w + x].ch = ch;
            fake_buf[y * fake_w + x].fg = fg;
            fake_buf[y * fake_w + x].bg = bg;
        }
    } else {
        tb_set_cell(x, y, ch, fg, bg);
    }
}

// | Decode UTF-8 codepoint, advance pointer, return codepoint
uint32_t utf8_decode(const char **pp) {
    const unsigned char *p = (const unsigned char *)*pp;
    uint32_t ch;
    if (p[0] < 0x80) { ch = p[0]; *pp += 1; }
    else if ((p[0] & 0xE0) == 0xC0) { ch = ((p[0] & 0x1F) << 6) | (p[1] & 0x3F); *pp += 2; }
    else if ((p[0] & 0xF0) == 0xE0) { ch = ((p[0] & 0x0F) << 12) | ((p[1] & 0x3F) << 6) | (p[2] & 0x3F); *pp += 3; }
    else if ((p[0] & 0xF8) == 0xF0) { ch = ((p[0] & 0x07) << 18) | ((p[1] & 0x3F) << 12) | ((p[2] & 0x3F) << 6) | (p[3] & 0x3F); *pp += 4; }
    else { ch = '?'; *pp += 1; }  // invalid UTF-8
    return ch;
}

// | Count UTF-8 codepoints in string (up to max)
size_t utf8_len(const char *s, size_t max) {
    size_t len = 0;
    while (*s && len < max) { utf8_decode(&s); len++; }
    return len;
}

// Redirect tb_set_cell after this point to headless-aware version
#undef tb_set_cell
#define tb_set_cell(x, y, ch, fg, bg) hd_set_cell(x, y, ch, fg, bg)

// tb_print_pad(x, y, w, fg, bg, str, right_align) -> Unit
// Prints string left/right aligned, padded to width - all in C
lean_obj_res lean_tb_print_pad(uint32_t x, uint32_t y, uint32_t w,
                                uint32_t fg, uint32_t bg,
                                lean_obj_arg str, uint8_t right, lean_obj_arg world) {
    const char *s = lean_string_cstr(str);
    size_t len = utf8_len(s, w);
    size_t pad = w > len ? w - len : 0;
    uint32_t cx = x;
    if (right) {  // right align: pad first
        for (size_t i = 0; i < pad; i++) tb_set_cell((int)cx++, (int)y, ' ', fg, bg);
    }
    const char *p = s;
    while (*p && (cx - x) < w) {
        uint32_t ch = utf8_decode(&p);
        tb_set_cell((int)cx++, (int)y, ch, fg, bg);
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
        size_t len = utf8_len(s, w);
        size_t pad = w > len ? w - len : 0;
        uint32_t cx = x;
        uint32_t y = y0 + (uint32_t)i;
        if (right) {
            for (size_t j = 0; j < pad; j++) tb_set_cell((int)cx++, (int)y, ' ', fg, bg);
        }
        const char *p = s;
        while (*p && (cx - x) < w) {
            uint32_t ch = utf8_decode(&p);
            tb_set_cell((int)cx++, (int)y, ch, fg, bg);
        }
        if (!right) {
            for (size_t j = 0; j < pad; j++) tb_set_cell((int)cx++, (int)y, ' ', fg, bg);
        }
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// String -> Float (returns NaN on parse failure)
double lean_string_to_float(b_lean_obj_arg s) {
    const char *str = lean_string_cstr(s);
    char *end;
    double v = strtod(str, &end);
    return (end == str || *end != '\0') ? __builtin_nan("") : v;
}
