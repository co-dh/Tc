/-
  termbox2 FFI bindings for TUI rendering
-/
import Tc.Util

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
def keyBackspace  : UInt16 := 0x08
def keyBackspace2 : UInt16 := 0x7f

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
-- | Parse color string: "default" | ANSI name | "rgbRGB" (cube) | "gray0"-"gray23"
-- ANSI names: black red green yellow blue magenta cyan white + br* variants
-- rgbRGB: xterm 6×6×6 cube, R/G/B ∈ 0-5, index = 16 + 36R + 6G + B
-- grayN: xterm grayscale ramp, N ∈ 0-23, index = 232 + N
def parseColor (s : String) : UInt32 :=
  match s with
  | "default"   => 0
  | "black"     => 16  -- 0 = TB_DEFAULT, so use cube black
  | "red"       => 1  | "green"   => 2  | "yellow"  => 3
  | "blue"      => 4  | "magenta" => 5  | "cyan"    => 6  | "white" => 7
  | "brBlack"   => 8  | "brRed"   => 9  | "brGreen" => 10 | "brYellow" => 11
  | "brBlue"    => 12 | "brMagenta" => 13 | "brCyan" => 14 | "brWhite" => 15
  | _ =>
    if s.startsWith "rgb" && s.length == 6 then
      match (s.drop 3).toString.toList with
      | [rc, gc, bc] =>
        let r := rc.toNat - '0'.toNat
        let g := gc.toNat - '0'.toNat
        let b := bc.toNat - '0'.toNat
        if r ≤ 5 && g ≤ 5 && b ≤ 5 then (16 + 36 * r + 6 * g + b).toUInt32
        else 0
      | _ => 0
    else if s.startsWith "gray" then
      match (s.drop 4).toString.toNat? with
      | some n => if n ≤ 23 then (232 + n).toUInt32 else 0
      | none => 0
    else 0

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

@[extern "lean_tb_inited"]
opaque inited : IO Bool

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

-- | Read termbox internal cell buffer as string (rows separated by newlines)
@[extern "lean_tb_buffer_str"]
opaque bufferStr : IO String

-- | Batch print with padding (C FFI - fast)
@[extern "lean_tb_print_pad"]
opaque printPadC : UInt32 → UInt32 → UInt32 → UInt32 → UInt32 → @& String → UInt8 → IO Unit

-- | Unified table render (C reads Column directly, computes widths if needed)
-- allCols, names, fmts, inWidths, colIdxs, nTotalRows, nKeys, colOff, r0, r1, curRow, curCol
-- moveDir, selCols, selRows, styles, prec, widthAdj
-- fmts: format chars for type indicators (empty = use Column tag)
-- moveDir: -1 = moved left, 0 = none, 1 = moved right (for tooltip direction)
-- prec: float decimal count (0-17), widthAdj: column width offset
-- Returns computed widths (Array Nat)
@[extern "lean_render_table"]
opaque renderTable : @& Array Column → @& Array String → @& Array Char → @& Array Nat
                   → @& Array Nat → UInt64 → UInt64 → UInt64 → UInt64 → UInt64 → UInt64 → UInt64
                   → Int64 → @& Array Nat → @& Array Nat → @& Array Nat → @& Array UInt32
                   → Int64 → Int64 → UInt8 → @& Array String → IO (Array Nat)

-- | Print string at position (for backwards compat)
def print (x y : UInt32) (fg bg : UInt32) (s : String) : IO Unit :=
  printPadC x y s.length.toUInt32 fg bg s 0

end Term
