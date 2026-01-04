/-
  App with ViewStack support
  CSV: pure Lean MemTable, other: ADBC (DuckDB+PRQL)
-/
import Tc.Data.ADBC.Meta
import Tc.Data.Mem.Meta
import Tc.Key
import Tc.Render
import Tc.Term
import Tc.ViewStack

open Tc

-- | Main loop with ViewStack
partial def mainLoop (stk : ViewStack) (vs : ViewState) (verbPfx : Option Verb := none) : IO Unit := do
  let vs' ← stk.cur.doRender vs
  renderTabLine stk.tabNames 0  -- current is index 0
  -- info overlay (toggle with I)
  if vs'.showInfo then
    let h ← Term.height; let w ← Term.width
    infoOverlay h.toNat w.toNat
  Term.present
  let ev ← Term.pollEvent
  let h ← Term.height
  let rowPg := (h.toNat - reservedLines) / 2
  let colPg := colPageSize
  -- I key: toggle info overlay
  if ev.type == Term.eventKey && ev.ch == 'I'.toNat.toUInt32 then
    return ← mainLoop stk { vs' with showInfo := !vs'.showInfo }
  -- +/- prefix for hor/ver/prec/width commands
  if ev.type == Term.eventKey && verbPfx.isNone then
    if ev.ch == '+'.toNat.toUInt32 then return ← mainLoop stk vs' (some .inc)
    if ev.ch == '-'.toNat.toUInt32 then return ← mainLoop stk vs' (some .dec)
  -- dispatch Cmd to ViewStack
  match evToCmd ev verbPfx with
  | some cmd => match stk.exec cmd rowPg colPg with
    | some stk' =>
      let reset := cmd matches .stk .dec | .colSel .del | .colSel _ | .col .search | .row .search | .col .filter | .row .filter
      mainLoop stk' (if reset then ViewState.default else vs')
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
    let ok ← AdbcTable.init
    if !ok then Term.shutdown; IO.eprintln "Backend init failed"; return
    match ← AdbcTable.fromFile path with
    | none => Term.shutdown; AdbcTable.shutdown; IO.eprintln "Query failed"
    | some tbl => match View.fromTbl tbl path with
      | some v => mainLoop ⟨v, #[]⟩ ViewState.default; Term.shutdown; AdbcTable.shutdown
      | none => Term.shutdown; AdbcTable.shutdown; IO.eprintln "Empty table"
