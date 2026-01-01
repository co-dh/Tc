/-
  Render logic for typeclass-based navigation
  ViewState holds scroll offsets (view concern, not navigation state)
-/
import Tc.Nav
import Tc.Term

open Tc

-- ViewState: scroll offsets (view concern, separate from NavState)
structure ViewState (n : Nat) where
  rowOff : Nat := 0      -- first visible row
  colOff : Fin n         -- first visible column

-- Default ViewState
def ViewState.default (h : n > 0) : ViewState n := ⟨0, ⟨0, h⟩⟩

/-! ## Style indices (match C STYLE_* defines) -/
def stCursor   : Nat := 0  -- cursor cell
def stSelRow   : Nat := 1  -- selected row
def stSelCur   : Nat := 2  -- selected col + cursor row
def stSelCol   : Nat := 3  -- selected col
def stCurRow   : Nat := 4  -- cursor row
def stCurCol   : Nat := 5  -- cursor col
def stDefault  : Nat := 6  -- default

-- Styles: fg, bg pairs for 7 states
def styles : Array UInt32 := #[
  Term.black, Term.white,     -- cursor
  Term.black, Term.green,     -- selected row
  Term.black, Term.magenta,   -- selected col + cursor row
  Term.magenta, Term.default, -- selected col
  Term.default, Term.default, -- cursor row
  Term.yellow, Term.default,  -- cursor col
  Term.default, Term.default  -- default
]

-- Get fg/bg pair from styles array
def styleFg (st : Array UInt32) (i : Nat) : UInt32 := st.getD (i * 2) Term.default
def styleBg (st : Array UInt32) (i : Nat) : UInt32 := st.getD (i * 2 + 1) Term.default

-- Determine cell style index based on cursor/selection state
def cellStyle (isCursor isSelRow isSelCol isCurRow isCurCol : Bool) : Nat :=
  if isCursor  then stCursor
  else if isSelRow then stSelRow
  else if isSelCol && isCurRow then stSelCur
  else if isSelCol then stSelCol
  else if isCurRow then stCurRow
  else if isCurCol then stCurCol
  else stDefault

-- Reserved lines: 1 header + 1 footer + 1 status
def reservedLines : Nat := 3

-- Column page size (fixed, since widths vary)
def colPageSize : Nat := 5

/-! ## Shared rendering functions -/

-- Column info: (origIdx, xPos, width)
abbrev ColInfo := Nat × UInt32 × UInt32

-- Render header/footer row at y with underline, returns separator x position
def renderHdrRow (names : Array String) (cols : Array ColInfo) (selCols : Array Nat)
    (curCol : Nat) (st : Array UInt32) (y : UInt32) : IO Unit := do
  for (idx, x, w) in cols do
    let name := names.getD idx ""
    let isCur := idx == curCol
    let isSel := selCols.contains idx
    let si := if isCur then stCursor else if isSel then stSelCol else stDefault
    let fg := styleFg st si ||| Term.underline
    let bg := styleBg st si
    Term.printPad x y w fg bg name

-- Render separator on a row
def renderSep (sepX : UInt32) (y : UInt32) (st : Array UInt32) : IO Unit :=
  if sepX > 0 then Term.setCell sepX y '|'.toNat.toUInt32 (styleFg st stDefault) (styleBg st stDefault)
  else pure ()

-- Render data cell (right-align if numeric)
def renderCell (x y w : UInt32) (fg bg : UInt32) (text : String) (isNum : Bool) : IO Unit :=
  if isNum then Term.printPadR x y w fg bg text else Term.printPad x y w fg bg text

-- RenderTable: tables that can render themselves
class RenderTable (α : Type) [ReadTable α] where
  render : {nRows nCols : Nat} → NavState nRows nCols α
         → (colOff rowStart rowEnd : Nat) → (styles : Array UInt32) → IO Unit

-- Render table to terminal, returns updated ViewState with adjusted offsets
def render {nRows nCols : Nat} {t : Type} [ReadTable t] [RenderTable t]
    (nav : NavState nRows nCols t) (view : ViewState nCols) (cumW : CumW nCols)
    : IO (ViewState nCols) := do
  Term.clear
  let h ← Term.height
  let w ← Term.width
  let visRows := h.toNat - reservedLines
  -- adjust offsets (need Fin for adjColOff, use curRow accessor for row)
  let rowOff := adjOff nav.curRow view.rowOff visRows
  let colOff := adjColOff ⟨nav.curColIdx, sorry⟩ view.colOff cumW w.toNat
  -- render via RenderTable
  RenderTable.render nav colOff.val rowOff (min nRows (rowOff + visRows)) styles
  -- status
  let status := s!"r{nav.curRow}/{nRows} c{nav.curColIdx}/{nCols} grp={nav.nKeys} sel={nav.selRows.size}"
  Term.print 0 (h - 1) Term.cyan Term.default status
  -- help
  let help := "hjkl:nav HJKL:pg g?:end t/T:sel !:grp q:q"
  Term.print (w - help.length.toUInt32) (h - 1) Term.yellow Term.default help
  Term.present
  pure ⟨rowOff, colOff⟩
