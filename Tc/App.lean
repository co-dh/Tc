/-
  Test app for typeclass-based navigation
  CSV: pure Lean MemTable, other: Backend (DuckDB+PRQL)
-/
import Tc.Data.ADBC.Table
import Tc.Data.MemTable
import Tc.Key
import Tc.Data.ADBC.Backend
import Tc.Term

open Tc

-- Main loop with g-prefix state
partial def mainLoop {nRows nCols : Nat} {t : Type} [ReadTable t] [RenderTable t]
    (nav : NavState nRows nCols t) (view : ViewState nCols) (cumW : CumW nCols)
    (gPrefix : Bool := false) : IO Unit := do
  let view' ← render nav view cumW
  let ev ← Term.pollEvent
  let h ← Term.height
  let rowPg := (h.toNat - reservedLines) / 2
  let colPg := colPageSize
  if ev.type == Term.eventKey then
    if ev.ch == 'q'.toNat.toUInt32 || ev.key == Term.keyEsc then return
    if ev.ch == 'g'.toNat.toUInt32 && !gPrefix then
      mainLoop nav view' cumW true
      return
  match keyToCmd ev gPrefix with
  | some cmd => mainLoop (nav.dispatch cmd rowPg colPg) view' cumW
  | none => mainLoop nav view' cumW

-- Run viewer with any ReadTable/RenderTable
def runViewer {t : Type} [ReadTable t] [RenderTable t] (tbl : t) : IO Unit := do
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then
      have hWidths : (ReadTable.colWidths tbl).size = nCols := sorry
      let cumW : CumW nCols := hWidths ▸ mkCumW (ReadTable.colWidths tbl)
      let nav := NavState.new tbl rfl rfl hr hc
      mainLoop nav (ViewState.default hc) cumW
    else IO.eprintln "No rows"
  else IO.eprintln "No columns"

-- Entry point
def main (args : List String) : IO Unit := do
  let path := args.head? |>.getD "data.csv"
  let _ ← Term.init
  -- CSV: use MemTable, otherwise: use Backend
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .error e => Term.shutdown; IO.eprintln s!"CSV parse error: {e}"
    | .ok tbl  => runViewer tbl; Term.shutdown
  else
    let ok ← Backend.init
    if !ok then Term.shutdown; IO.eprintln "Backend init failed"; return
    let prql := s!"from `{path}` | take 100000"
    match ← Backend.query (Backend.mkLimited prql 100000) with
    | none     => Term.shutdown; Backend.shutdown; IO.eprintln "Query failed"
    | some tbl => runViewer tbl; Term.shutdown; Backend.shutdown
