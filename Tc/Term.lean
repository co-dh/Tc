/-
  termbox2 FFI bindings for TUI rendering
-/

namespace Term

-- | Key codes (from termbox2.h)
def keyArrowUp    : UInt16 := 0xFFFF - 18
def keyArrowDown  : UInt16 := 0xFFFF - 19
def keyArrowLeft  : UInt16 := 0xFFFF - 20
def keyArrowRight : UInt16 := 0xFFFF - 21
def keyPageUp     : UInt16 := 0xFFFF - 23
def keyPageDown   : UInt16 := 0xFFFF - 24
def keyHome       : UInt16 := 0xFFFF - 25
def keyEnd        : UInt16 := 0xFFFF - 26
def keyEsc        : UInt16 := 0x1B
def keyEnter      : UInt16 := 0x0D

-- | Event types
def eventKey : UInt8 := 1

-- | Modifiers (from termbox2.h)
def modAlt   : UInt8 := 1
def modCtrl  : UInt8 := 2
def modShift : UInt8 := 4

-- | Ctrl+letter codes (Ctrl+A=1, Ctrl+B=2, ...)
def ctrlD : UInt32 := 4   -- Ctrl+D (page down)
def ctrlU : UInt32 := 21  -- Ctrl+U (page up)

-- | Colors (xterm-256 palette for TB_OUTPUT_256)
-- 0-7: standard ANSI, 8-15: bright, 16-231: cube, 232-255: grayscale
def default : UInt32 := 0x0000  -- termbox TB_DEFAULT (uses terminal default color)
def black   : UInt32 := 16      -- pure black (from 6x6x6 cube, since 0=TB_DEFAULT)
def red     : UInt32 := 1       -- ANSI red
def green   : UInt32 := 2       -- ANSI green
def yellow  : UInt32 := 3       -- ANSI yellow/olive
def blue    : UInt32 := 4       -- ANSI blue
def magenta : UInt32 := 5       -- ANSI magenta
def cyan    : UInt32 := 6       -- ANSI cyan
def white   : UInt32 := 7       -- ANSI white (light gray)
-- Bright colors (8-15)
def brBlack   : UInt32 := 8     -- bright black (dark gray)
def brRed     : UInt32 := 9
def brGreen   : UInt32 := 10
def brYellow  : UInt32 := 11
def brBlue    : UInt32 := 12
def brMagenta : UInt32 := 13
def brCyan    : UInt32 := 14    -- light cyan
def brWhite   : UInt32 := 15    -- bright white
-- Extended palette
def slate     : UInt32 := 60    -- muted blue-gray
def sky       : UInt32 := 237   -- dark gray (subtle highlight)
def mint      : UInt32 := 158   -- soft mint green
def peach     : UInt32 := 223   -- soft peach
def lavender  : UInt32 := 183   -- soft purple
def gray234   : UInt32 := 234   -- very dark gray
def gray240   : UInt32 := 240   -- medium gray
def gray252   : UInt32 := 252   -- very light gray

-- | Attributes (OR with color)
def underline : UInt32 := 0x02000000

-- | Terminal event from poll
structure Event where
  type : UInt8
  mod  : UInt8
  key  : UInt16
  ch   : UInt32
  w    : UInt32  -- resize width
  h    : UInt32  -- resize height
  deriving Repr

-- FFI declarations
@[extern "lean_isatty_stdin"]
opaque isattyStdin : IO Bool

@[extern "lean_reopen_tty"]
opaque reopenTty : IO Bool

@[extern "lean_tb_init"]
opaque init : IO Int32

@[extern "lean_tb_shutdown"]
opaque shutdown : IO Unit

@[extern "lean_tb_width"]
opaque width : IO UInt32

@[extern "lean_tb_height"]
opaque height : IO UInt32

@[extern "lean_tb_clear"]
opaque clear : IO Unit

@[extern "lean_tb_present"]
opaque present : IO Unit

@[extern "lean_tb_poll_event"]
opaque pollEvent : IO Event

-- | Capture screen via tmux capture-pane
def bufferStr : IO String := do
  let out ← IO.Process.output { cmd := "tmux", args := #["capture-pane", "-p"] }
  pure out.stdout

-- | Batch print with padding (C FFI - fast)
@[extern "lean_tb_print_pad"]
opaque printPadC : UInt32 → UInt32 → UInt32 → UInt32 → UInt32 → @& String → UInt8 → IO Unit

-- | Unified table render (C reads Column directly, computes widths if needed)
-- allCols, names, fmts, inWidths, colIdxs, nTotalRows, nKeys, colOff, r0, r1, curRow, curCol
-- moveDir, selCols, selRows, styles, precAdj, widthAdj
-- fmts: format chars for type indicators (empty = use Column tag)
-- moveDir: -1 = moved left, 0 = none, 1 = moved right (for tooltip direction)
-- precAdj: precision adjustment for float display, widthAdj: column width offset
-- Returns computed widths (Array Nat)
@[extern "lean_render_table"]
opaque renderTable : @& Array Column → @& Array String → @& Array Char → @& Array Nat
                   → @& Array Nat → UInt64 → UInt64 → UInt64 → UInt64 → UInt64 → UInt64 → UInt64
                   → Int64 → @& Array Nat → @& Array Nat → @& Array UInt32
                   → Int64 → Int64 → IO (Array Nat)

-- | Print string at position (for backwards compat)
def print (x y : UInt32) (fg bg : UInt32) (s : String) : IO Unit :=
  printPadC x y s.length.toUInt32 fg bg s 0

end Term
