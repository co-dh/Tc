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

-- | Colors (termbox2 indexed: TB_DEFAULT=0, TB_BLACK=1, ..., TB_WHITE=8)
def default : UInt32 := 0x0000
def black   : UInt32 := 0x0001
def red     : UInt32 := 0x0002
def green   : UInt32 := 0x0003
def yellow  : UInt32 := 0x0004
def blue    : UInt32 := 0x0005
def magenta : UInt32 := 0x0006
def cyan    : UInt32 := 0x0007
def white   : UInt32 := 0x0008

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

@[extern "lean_tb_set_cell"]
opaque setCell : UInt32 → UInt32 → UInt32 → UInt32 → UInt32 → IO Unit

@[extern "lean_tb_poll_event"]
opaque pollEvent : IO Event

@[extern "lean_tb_buffer_str"]
opaque bufferStr : IO String

-- | Batch print with padding (C FFI - fast)
@[extern "lean_tb_print_pad"]
opaque printPadC : UInt32 → UInt32 → UInt32 → UInt32 → UInt32 → @& String → UInt8 → IO Unit

-- | Batch render column (x, w, y0, fgs, bgs, strs, rights)
@[extern "lean_tb_render_col"]
opaque renderCol : UInt32 → UInt32 → UInt32 → @& Array UInt32 → @& Array UInt32
                 → @& Array String → @& Array UInt8 → IO Unit

-- | Unified table render (C reads Column directly, computes widths if needed)
-- allCols, names, fmts, inWidths, colIdxs, nTotalRows, nKeys, colOff, r0, r1, curRow, curCol, selCols, selRows, styles
-- fmts: format chars for type indicators (empty = use Column tag)
-- Returns computed widths (Array Nat)
@[extern "lean_render_table"]
opaque renderTable : @& Array Column → @& Array String → @& Array Char → @& Array Nat
                   → @& Array Nat → UInt64 → UInt64 → UInt64 → UInt64 → UInt64 → UInt64 → UInt64
                   → @& Array Nat → @& Array Nat → @& Array UInt32 → IO (Array Nat)

-- | Print string left-aligned, truncated/padded to width
def printPad (x y w : UInt32) (fg bg : UInt32) (s : String) : IO Unit :=
  printPadC x y w fg bg s 0

-- | Print string right-aligned, truncated/padded to width
def printPadR (x y w : UInt32) (fg bg : UInt32) (s : String) : IO Unit :=
  printPadC x y w fg bg s 1

-- | Print string at position (for backwards compat)
def print (x y : UInt32) (fg bg : UInt32) (s : String) : IO Unit :=
  printPadC x y s.length.toUInt32 fg bg s 0

end Term
