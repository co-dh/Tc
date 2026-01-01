/-
  Test app for typeclass-based navigation
  Loads CSV via Backend (DuckDB+PRQL), renders with termbox2
-/
import Tc.Data.ADBC
import Tc.Key
import Tv.Backend
import Tv.Term

open Tc

-- Main loop with g-prefix state
partial def mainLoop {nRows nCols : Nat} {t : Type} [ReadTable t] [RenderTable t]
    (nav : NavState nRows nCols t) (view : ViewState nCols) (cumW : CumW nCols)
    (gPrefix : Bool := false) : IO Unit := do
  let view' ← render nav view cumW
  let ev ← Term.pollEvent
  -- page sizes: half screen
  let h ← Term.height
  let rowPg := (h.toNat - reservedLines) / 2
  let colPg := colPageSize
  if ev.type == Term.eventKey then
    if ev.ch == 'q'.toNat.toUInt32 || ev.key == Term.keyEsc then return
    -- Check for g prefix
    if ev.ch == 'g'.toNat.toUInt32 && !gPrefix then
      mainLoop nav view' cumW true
      return
  match keyToCmd ev gPrefix with
  | some cmd => mainLoop (dispatch cmd nav rowPg colPg) view' cumW
  | none => mainLoop nav view' cumW

-- Entry point
def main (args : List String) : IO Unit := do
  let path := args.head? |>.getD "data.csv"
  -- init
  let ok ← Backend.init
  if !ok then IO.eprintln "Backend init failed"; return
  let _ ← Term.init
  -- load via PRQL
  let prql := s!"from `{path}` | take 100000"
  let some tbl ← Backend.query (Backend.mkLimited prql 100000)
    | Term.shutdown; Backend.shutdown; IO.eprintln "Query failed"; return
  -- build NavState using ReadTable
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then
      -- colWidths for rendering
      have hWidths : (ReadTable.colWidths tbl).size = nCols := sorry
      let cumW : CumW nCols := hWidths ▸ mkCumW (ReadTable.colWidths tbl)
      let nav : NavState nRows nCols SomeTable :=
        ⟨tbl, rfl, rfl, NavAxis.default hr, NavAxis.default hc, #[]⟩
      let view : ViewState nCols := ViewState.default hc
      mainLoop nav view cumW
    else
      IO.eprintln "No rows"
  else
    IO.eprintln "No columns"
  -- cleanup
  Term.shutdown
  Backend.shutdown
