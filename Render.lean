/-
  Render logic for typeclass-based navigation
  ViewState holds scroll offsets (view concern, not navigation state)
-/
import Tc.Nav
import Tv.Backend
import Tv.Term

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

-- Render table to terminal, returns updated ViewState with adjusted offsets
def render {n : Nat} (st : SomeTable) (nav : NavState n)
    (view : ViewState n) (cumW : CumW n) : IO (ViewState n) := do
  Term.clear
  let h ← Term.height
  let w ← Term.width
  let visRows := h.toNat - reservedLines
  -- adjust row offset
  let rowOff := adjOff nav.row.cur view.rowOff visRows
  -- adjust col offset
  let colOff := adjColOff nav.col.cur view.colOff cumW w.toNat
  -- render
  let _ ← st.render nav.dispColIdxs nav.nKeys colOff.val
    rowOff (min nav.nRows (rowOff + visRows)) nav.row.cur nav.curColIdx
    nav.selColIdxs nav.selRows styles 50 20 3
  -- status
  let status := s!"r{nav.row.cur}/{nav.nRows} c{nav.col.cur.val}/{n} grp={nav.nKeys} sel={nav.selRows.size}"
  Term.print 0 (h - 1) Term.cyan Term.default status
  -- help
  let help := "hjkl:nav HJKL:pg g?:end t/T:sel !:grp q:q"
  Term.print (w - help.length.toUInt32) (h - 1) Term.yellow Term.default help
  Term.present
  pure ⟨rowOff, colOff⟩
