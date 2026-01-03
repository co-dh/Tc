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

-- Loop result: quit or modify table
inductive LoopAction (t : Type) where
  | quit
  | del (curCol : Nat) (grp : Array String) (tbl : t)

-- Main loop with g-prefix state
partial def mainLoop {nRows nCols : Nat} {t : Type} [ModifyTable t] [RenderTable t]
    (nav : NavState nRows nCols t) (view : ViewState) (gPrefix : Bool := false)
    : IO (LoopAction t) := do
  let view' ← render nav view
  let ev ← Term.pollEvent
  let h ← Term.height
  let rowPg := (h.toNat - reservedLines) / 2
  let colPg := colPageSize
  if ev.type == Term.eventKey then
    if ev.ch == 'q'.toNat.toUInt32 || ev.key == Term.keyEsc then return .quit
    if ev.ch == 'g'.toNat.toUInt32 && !gPrefix then return ← mainLoop nav view' true
  match keyToCmd ev gPrefix with
  | some .del =>
    let (tbl', grp') := ModifyTable.del nav.tbl nav.curColIdx nav.selColIdxs nav.group
    return .del nav.curDispCol grp' tbl'
  | some cmd => mainLoop (nav.dispatch cmd rowPg colPg) view'
  | none => mainLoop nav view'

-- Read-only loop (no delete support)
partial def loopRO {nRows nCols : Nat} {t : Type} [ReadTable t] [RenderTable t]
    (nav : NavState nRows nCols t) (view : ViewState) (gPrefix : Bool := false)
    : IO Unit := do
  let view' ← render nav view
  let ev ← Term.pollEvent
  let h ← Term.height
  let rowPg := (h.toNat - reservedLines) / 2
  let colPg := colPageSize
  if ev.type == Term.eventKey then
    if ev.ch == 'q'.toNat.toUInt32 || ev.key == Term.keyEsc then return
    if ev.ch == 'g'.toNat.toUInt32 && !gPrefix then return ← loopRO nav view' true
  match keyToCmd ev gPrefix with
  | some cmd => loopRO (nav.dispatch cmd rowPg colPg) view'
  | none => loopRO nav view'

-- Run viewer (read-only, no delete support)
def runViewer {t : Type} [ReadTable t] [RenderTable t] (tbl : t) : IO Unit := do
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then
      let nav := NavState.new tbl rfl rfl hr hc
      loopRO nav ViewState.default
    else IO.eprintln "No rows"
  else IO.eprintln "No columns"

-- Run viewer with delete support (ModifyTable)
partial def runViewerMod {t : Type} [ModifyTable t] [RenderTable t]
    (tbl : t) (initCol : Nat := 0) (grp : Array String := #[]) : IO Unit := do
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then
      let nav := NavState.newAt tbl rfl rfl hr hc initCol grp
      match ← mainLoop nav ViewState.default with
      | .quit => pure ()
      | .del col grp' tbl' => runViewerMod tbl' col grp'
    else IO.eprintln "No rows"
  else IO.eprintln "No columns"

-- Entry point
def main (args : List String) : IO Unit := do
  let path := args.head? |>.getD "data.csv"
  let _ ← Term.init
  -- CSV: use MemTable (with delete support), otherwise: use Backend (read-only)
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .error e => Term.shutdown; IO.eprintln s!"CSV parse error: {e}"
    | .ok tbl  => runViewerMod tbl; Term.shutdown
  else
    let ok ← Backend.init
    if !ok then Term.shutdown; IO.eprintln "Backend init failed"; return
    let prql := s!"from `{path}` | take 100000"
    match ← Backend.query (Backend.mkLimited prql 100000) with
    | none     => Term.shutdown; Backend.shutdown; IO.eprintln "Query failed"
    | some tbl => runViewer tbl; Term.shutdown; Backend.shutdown
