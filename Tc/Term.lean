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

-- | xterm-256 color names: index → name. Names for ANSI 0-15 + black (16).
-- 0 = TB_DEFAULT (terminal default), 16 = pure black (since 0 is taken).
-- Unnamed slots are "". Themes use rgbRGB/grayN syntax for other colors.
def colorNames : Array String := Id.run do
  let mut a := Array.replicate 256 ""
  for (name, idx) in #[
    ("default", 0),
    ("red", 1), ("green", 2), ("yellow", 3), ("blue", 4),
    ("magenta", 5), ("cyan", 6), ("white", 7),
    ("brBlack", 8), ("brRed", 9), ("brGreen", 10), ("brYellow", 11),
    ("brBlue", 12), ("brMagenta", 13), ("brCyan", 14), ("brWhite", 15),
    ("black", 16)] do
    a := a.set! idx name
  return a

-- | Parse color string: named | "rgbRGB" (cube) | "gray0"-"gray23"
def parseColor (s : String) : UInt32 :=
  if let some i := colorNames.findIdx? (· == s) then i.toUInt32
  else if s.startsWith "rgb" && s.length == 6 then
    match (s.drop 3).toString.toList with
    | [rc, gc, bc] =>
      if rc.isDigit && gc.isDigit && bc.isDigit then
        let r := rc.toNat - '0'.toNat
        let g := gc.toNat - '0'.toNat
        let b := bc.toNat - '0'.toNat
        if r ≤ 5 && g ≤ 5 && b ≤ 5 then (16 + 36 * r + 6 * g + b).toUInt32
        else 0
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
