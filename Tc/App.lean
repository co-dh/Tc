/-
  App with ViewStack support
  CSV: pure Lean MemTable, other: Backend (DuckDB+PRQL)
-/
import Tc.Data.ADBC.Table
import Tc.Data.MemTable
import Tc.Key
import Tc.Data.ADBC.Backend
import Tc.Render
import Tc.Term
import Tc.ViewStack

open Tc

-- | Main loop with ViewStack
partial def mainLoop (stk : ViewStack) (vs : ViewState) (gPrefix : Bool := false) : IO Unit := do
  let vs' ← stk.cur.doRender vs
  renderTabLine stk.tabNames 0  -- current is index 0
  Term.present
  let ev ← Term.pollEvent
  let h ← Term.height
  let rowPg := (h.toNat - reservedLines) / 2
  let colPg := colPageSize
  -- g prefix
  if ev.type == Term.eventKey && ev.ch == 'g'.toNat.toUInt32 && !gPrefix then
    return ← mainLoop stk vs' true
  -- dispatch Cmd to ViewStack
  match evToCmd ev gPrefix with
  | some cmd => match stk.exec cmd rowPg colPg with
    | some stk' => let vs'' := if cmd matches .stk .dec | .col .del | .colSel _ then ViewState.default else vs'
                   mainLoop stk' vs''
    | none => return  -- quit or table empty
  | none => mainLoop stk vs'

-- | Parse -c <cmd> from args
def parseArgs (args : List String) : String × String :=
  match args with
  | "-c" :: cmd :: rest => (rest.head?.getD "data.csv", cmd)
  | path :: "-c" :: cmd :: _ => (path, cmd)
  | path :: _ => (path, "")
  | [] => ("data.csv", "")

-- | Entry point
def main (args : List String) : IO Unit := do
  let (path, _cmdStr) := parseArgs args
  let _ ← Term.init
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .error e => Term.shutdown; IO.eprintln s!"CSV parse error: {e}"
    | .ok tbl => match View.fromTbl tbl path with
      | some v => mainLoop ⟨v, #[]⟩ ViewState.default; Term.shutdown
      | none => Term.shutdown; IO.eprintln "Empty table"
  else
    let ok ← Backend.init
    if !ok then Term.shutdown; IO.eprintln "Backend init failed"; return
    match ← AdbcTable.fromFile path with
    | none => Term.shutdown; Backend.shutdown; IO.eprintln "Query failed"
    | some tbl => match View.fromTbl tbl path with
      | some v => mainLoop ⟨v, #[]⟩ ViewState.default; Term.shutdown; Backend.shutdown
      | none => Term.shutdown; Backend.shutdown; IO.eprintln "Empty table"
