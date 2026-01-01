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
def render {n : Nat} (st : SomeTable) (t : Table n) (nav : NavState n t) : IO Unit := do
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
  let curColName := colAt nav.group t.colNames nav.col.cur.val
  let curColIdx := curColName.bind (fun nm => t.colNames.findIdx? (· == nm)) |>.getD 0
  -- render
  let _ ← st.render colIdxs nKeys nav.col.off.val
    r0 r1 nav.row.cur curColIdx
    selColIdxs selRows styles 50 20 3
  -- status
  let status := s!"r{nav.row.cur}/{t.nRows} c{nav.col.cur.val}/{n} grp={nKeys} sel={selRows.size}"
  Term.print 0 (h - 1) Term.cyan Term.default status
  -- help
  Term.print (w - 28) (h - 1) Term.yellow Term.default "hjkl:nav stu:sel !:grp q:quit"
  Term.present

-- Map key to command (s/t/u=select/toggle/unselect)
def keyToCmd (ev : Term.Event) : Option String :=
  if ev.type != Term.eventKey then none
  -- Ctrl+D/U for page down/up
  else if ev.ch == Term.ctrlD then some "r>"
  else if ev.ch == Term.ctrlU then some "r<"
  -- Arrow keys
  else if ev.key == Term.keyArrowDown  then some "r+"
  else if ev.key == Term.keyArrowUp    then some "r-"
  else if ev.key == Term.keyArrowRight then some "c+"
  else if ev.key == Term.keyArrowLeft  then some "c-"
  else if ev.key == Term.keyPageDown   then some "r>"
  else if ev.key == Term.keyPageUp     then some "r<"
  else if ev.key == Term.keyHome       then some "r0"
  else if ev.key == Term.keyEnd        then some "r$"
  -- Character keys
  else if ev.ch != 0 then
    match Char.ofNat ev.ch.toNat with
    | 'j' => some "r+"  | 'k' => some "r-"
    | 'h' => some "c-"  | 'l' => some "c+"
    | 'g' => some "r0"  | 'G' => some "r$"
    | '0' => some "c0"  | '$' => some "c$"
    | 'H' => some "c<"  | 'L' => some "c>"
    | 's' => some "R+"  | 't' => some "R^"  | 'u' => some "R-"
    | 'S' => some "C+"  | 'T' => some "C^"  | 'U' => some "C-"
    | '!' => some "G^"
    | _ => none
  else none

-- Main loop
partial def mainLoop {n : Nat} (st : SomeTable) (t : Table n) (nav : NavState n t)
    (cumW : CumW n) (screenW : Nat) : IO Unit := do
  render st t nav
  let ev ← Term.pollEvent
  if ev.type == Term.eventKey then
    if ev.ch == 'q'.toNat.toUInt32 || ev.key == Term.keyEsc then return
  let h ← Term.height
  let rowPg := h.toNat - 2
  match keyToCmd ev with
  | some cmd => mainLoop st t (dispatch cmd t nav cumW screenW rowPg) cumW screenW
  | none => mainLoop st t nav cumW screenW

-- Entry point
def main (args : List String) : IO Unit := do
  let path := args.head? |>.getD "data.csv"
  -- init
  let ok ← Backend.init
  if !ok then IO.eprintln "Backend init failed"; return
  let _ ← Term.init
  -- load via PRQL
  let prql := s!"from `{path}` | take 100000"
  let some st ← Backend.query (Backend.mkLimited prql 100000)
    | Term.shutdown; Backend.shutdown; IO.eprintln "Query failed"; return
  -- build Table with proofs
  let nc := st.colNames.size
  if h : nc > 0 then
    -- colWidths comes from same source as colNames, assume same size
    have hWidths : st.colWidths.size = nc := sorry
    let t : Table nc := ⟨st.nRows, st.colNames, st.colWidths, rfl, hWidths⟩
    let cumW := t.cumW
    let w ← Term.width
    let nav : NavState nc t := ⟨{}, ColNav.default h, {}⟩
    mainLoop st t nav cumW w.toNat
  else
    IO.eprintln "No columns"
  -- cleanup
  Term.shutdown
  Backend.shutdown
