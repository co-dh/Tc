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

-- Render table to terminal, returns updated ViewState with adjusted offsets
def render {n : Nat} (st : SomeTable) (t : Nav n) (nav : NavState n t)
    (view : ViewState n) (cumW : CumW n) : IO (ViewState n) := do
  Term.clear
  let h ← Term.height
  let w ← Term.width
  let visRows := h.toNat - 2
  -- adjust row offset
  let rowOff := adjOff nav.row.cur view.rowOff visRows
  -- adjust col offset
  let colOff := adjColOff nav.col.cur view.colOff cumW w.toNat
  -- row range
  let r0 := rowOff
  let r1 := min t.nRows (r0 + visRows)
  -- col indices in display order
  let dispCols := dispOrder nav.group t.colNames
  let colIdxs := dispCols.filterMap fun name => t.colNames.findIdx? (· == name)
  let nKeys := nav.group.arr.size
  -- selected rows/cols
  let selRows := if nav.row.sels.inv then #[] else nav.row.sels.arr
  let selColIdxs := nav.col.sels.arr.filterMap fun name => t.colNames.findIdx? (· == name)
  -- cursor col in original order
  let curColName := colAt nav.group t.colNames nav.col.cur.val
  let curColIdx := curColName.bind (fun nm => t.colNames.findIdx? (· == nm)) |>.getD 0
  -- render
  let _ ← st.render colIdxs nKeys colOff.val
    r0 r1 nav.row.cur curColIdx
    selColIdxs selRows styles 50 20 3
  -- status
  let status := s!"r{nav.row.cur}/{t.nRows} c{nav.col.cur.val}/{n} grp={nKeys} sel={selRows.size}"
  Term.print 0 (h - 1) Term.cyan Term.default status
  -- help
  Term.print (w - 36) (h - 1) Term.yellow Term.default "hjkl:nav HJKL:pg g?:end stu:sel q:quit"
  Term.present
  pure ⟨rowOff, colOff⟩
