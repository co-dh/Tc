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

-- Reserved lines: 1 header + 1 status + 1 margin
def reservedLines : Nat := 3

-- Column page size (fixed, since widths vary)
def colPageSize : Nat := 5

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
  -- adjust offsets
  let rowOff := adjOff nav.row.cur view.rowOff visRows
  let colOff := adjColOff nav.col.cur view.colOff cumW w.toNat
  -- render via RenderTable
  RenderTable.render nav colOff.val rowOff (min nRows (rowOff + visRows)) styles
  -- status
  let status := s!"r{nav.row.cur}/{nRows} c{nav.col.cur.val}/{nCols} grp={nav.nKeys} sel={nav.selRows.size}"
  Term.print 0 (h - 1) Term.cyan Term.default status
  -- help
  let help := "hjkl:nav HJKL:pg g?:end t/T:sel !:grp q:q"
  Term.print (w - help.length.toUInt32) (h - 1) Term.yellow Term.default help
  Term.present
  pure ⟨rowOff, colOff⟩
