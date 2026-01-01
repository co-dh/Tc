/-
  Test app for typeclass-based navigation (Tc/Nav.lean)
  Loads CSV via Backend (DuckDB+PRQL), renders with termbox2
-/
import Tc.Nav
import Tc.Render
import Tc.Key
import Tv.Backend
import Tv.Term
import Tv.Types

open Tc

-- Main loop with g-prefix state
partial def mainLoop {n : Nat} (st : SomeTable) (t : Nav n) (nav : NavState n t)
    (view : ViewState n) (cumW : CumW n) (gPrefix : Bool := false) : IO Unit := do
  let view' ← render st t nav view cumW
  let ev ← Term.pollEvent
  if ev.type == Term.eventKey then
    if ev.ch == 'q'.toNat.toUInt32 || ev.key == Term.keyEsc then return
    -- Check for g prefix
    if ev.ch == 'g'.toNat.toUInt32 && !gPrefix then
      mainLoop st t nav view' cumW true
      return
  match keyToCmd ev gPrefix with
  | some cmd => mainLoop st t (dispatch cmd t nav) view' cumW
  | none => mainLoop st t nav view' cumW

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
  -- build Nav with proofs
  let nc := st.colNames.size
  if h : nc > 0 then
    let t : Nav nc := ⟨st.nRows, st.colNames, rfl⟩
    -- colWidths for rendering (same size as colNames from SomeTable)
    have hWidths : st.colWidths.size = nc := sorry
    let cumW : CumW nc := hWidths ▸ mkCumW st.colWidths
    let nav : NavState nc t := ⟨{}, ColNav.default h, {}⟩
    let view : ViewState nc := ViewState.default h
    mainLoop st t nav view cumW
  else
    IO.eprintln "No columns"
  -- cleanup
  Term.shutdown
  Backend.shutdown
