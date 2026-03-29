// Terminal core: headless globals, screen helpers, UTF-8 utils
// Shared between term_core.c and render.c
#ifndef TERM_CORE_H
#define TERM_CORE_H

#include <stdint.h>
#include "termbox2.h"

// Headless mode globals (defined in term_core.c)
extern struct tb_cell *fake_buf;
extern int fake_w, fake_h, headless;

// Headless-aware screen dimensions
int screen_w(void);
int screen_h(void);

// Headless-aware tb_set_cell wrapper
void hd_set_cell(int x, int y, uint32_t ch, uint32_t fg, uint32_t bg);

// Redirect tb_set_cell to headless-aware version
#undef tb_set_cell
#define tb_set_cell(x, y, ch, fg, bg) hd_set_cell(x, y, ch, fg, bg)

// UTF-8 utilities
uint32_t utf8_decode(const char **pp);
size_t utf8_len(const char *s, size_t max);

#endif
