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
