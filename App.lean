/-
  Test app for typeclass-based navigation (Tc/Basic.lean)
  Loads CSV via Backend (DuckDB+PRQL), renders with termbox2
-/
import Tc.Basic
import Tv.Backend
import Tv.Term
import Tv.Types

open Tc

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

-- Render table to terminal
def render (st : SomeTable) (t : Table) (nav : NavState t) : IO Unit := do
  Term.clear
  let h ← Term.height
  let w ← Term.width
  let visRows := h.toNat - 2
  -- row range
  let r0 := if nav.row.cur < visRows then 0 else nav.row.cur - visRows + 1
  let r1 := min t.nRows (r0 + visRows)
  -- col indices in display order
  let dispCols := dispOrder nav.group t.colNames
  let colIdxs := dispCols.filterMap fun name => t.colNames.findIdx? (· == name)
  let nKeys := nav.group.arr.size
  -- selected rows/cols
  let selRows := if nav.row.sels.inv then #[] else nav.row.sels.arr
  let selColIdxs := nav.col.sels.arr.filterMap fun name => t.colNames.findIdx? (· == name)
  -- cursor col in original order
  let curColName := colAt nav.group t.colNames nav.col.cur
  let curColIdx := curColName.bind (fun n => t.colNames.findIdx? (· == n)) |>.getD 0
  -- render
  let _ ← st.render colIdxs nKeys nav.col.off
    r0 r1 nav.row.cur curColIdx
    selColIdxs selRows styles 50 20 3
  -- status
  let status := s!"r{nav.row.cur}/{t.nRows} c{nav.col.cur}/{t.nCols} grp={nKeys} sel={selRows.size}"
  Term.print 0 (h - 1) Term.cyan Term.default status
  -- help
  Term.print (w - 30) (h - 1) Term.yellow Term.default "hjkl:nav spc:sel !:grp q:quit"
  Term.present

-- Map key to command
-- r=row.cur, c=col.cur, R=row.sels, C=col.sels, G=col.group
-- +=down/add, -=up/del, </>=page, 0/$=home/end/clear/all, ^=toggle, ~=invert
def keyToCmd (ev : Term.Event) : Option String :=
  if ev.type != Term.eventKey then none
  else if ev.ch != 0 then
    match Char.ofNat ev.ch.toNat with
    -- row cursor (r): j/k=move, g/G=home/end
    | 'j' => some "r+"  | 'k' => some "r-"
    | 'g' => some "r0"  | 'G' => some "r$"
    -- col cursor (c): h/l=move, 0/$=home/end, H/L=page
    | 'l' => some "c+"  | 'h' => some "c-"
    | '0' => some "c0"  | '$' => some "c$"
    | 'H' => some "c<"  | 'L' => some "c>"
    -- row selection (R): space=toggle, v/V=add/del, a=all, d=clear, ~=invert
    | ' ' => some "R^"
    | 'v' => some "R+"  | 'V' => some "R-"
    | 'a' => some "R$"  | 'd' => some "R0"
    | '~' => some "R~"
    -- col selection (C): s=toggle, S=all, D=clear, I=invert
    | 's' => some "C^"
    | 'S' => some "C$"  | 'x' => some "C0"
    | 'I' => some "C~"
    -- group (G): !=toggle, @=all, #=clear
    | '!' => some "G^"
    | '@' => some "G$"  | '#' => some "G0"
    | _ => none
  else
    -- non-printable keys
    if ev.key == Term.keyArrowDown  then some "r+"
    else if ev.key == Term.keyArrowUp    then some "r-"
    else if ev.key == Term.keyArrowRight then some "c+"
    else if ev.key == Term.keyArrowLeft  then some "c-"
    else if ev.key == Term.keyPageDown   then some "r>"
    else if ev.key == Term.keyPageUp     then some "r<"
    else if ev.key == Term.keyHome       then some "r0"
    else if ev.key == Term.keyEnd        then some "r$"
    else none

-- Main loop
partial def mainLoop (st : SomeTable) (t : Table) (nav : NavState t) : IO Unit := do
  render st t nav
  let ev ← Term.pollEvent
  if ev.type == Term.eventKey then
    if ev.ch == 'q'.toNat.toUInt32 || ev.key == Term.keyEsc then return
  match keyToCmd ev with
  | some cmd => mainLoop st t (dispatch cmd t nav)
  | none => mainLoop st t nav

-- Entry point
def main (args : List String) : IO Unit := do
  let path := args.head? |>.getD "data.csv"
  -- init
  let ok ← Backend.init
  if !ok then IO.eprintln "Backend init failed"; return
  let _ ← Term.init
  -- load via PRQL (DuckDB reads CSV/parquet directly)
  let prql := s!"from `{path}` | take 100000"
  let some st ← Backend.query (Backend.mkLimited prql 100000)
    | Term.shutdown; Backend.shutdown; IO.eprintln "Query failed"; return
  -- build Table
  let t : Table := ⟨st.nRows, st.colNames⟩
  let nav : NavState t := {}
  -- run
  mainLoop st t nav
  -- cleanup
  Term.shutdown
  Backend.shutdown
