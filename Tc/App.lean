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

-- | Result of del/sort: new table + cursor info
structure RebuildInfo where
  t : Type
  instR : ReadTable t
  instM : ModifyTable t
  instV : RenderTable t
  tbl : t
  col : Nat
  row : Nat
  grp : Array String

-- | Execute Cmd on View, returns (updated View, rebuildInfo if table changed)
def execCmd (v : View) (cmd : Cmd) (rowPg colPg : Nat) : View × Option RebuildInfo :=
  match cmd with
  | .col .del =>
    let tbl' := @ModifyTable.del v.t v.instM v.tbl v.curColIdx v.selColIdxs v.getGroup
    (v, some ⟨v.t, v.instR, v.instM, v.instV, tbl'.1, v.curDispCol, 0, tbl'.2⟩)
  | .colSel .sortAsc =>
    let grpIdxs := v.getGroup.filterMap v.colNames.idxOf?
    let tbl' := @ModifyTable.sort v.t v.instM v.tbl v.curColIdx grpIdxs true
    (v, some ⟨v.t, v.instR, v.instM, v.instV, tbl', v.curColIdx, v.curRow, v.getGroup⟩)
  | .colSel .sortDesc =>
    let grpIdxs := v.getGroup.filterMap v.colNames.idxOf?
    let tbl' := @ModifyTable.sort v.t v.instM v.tbl v.curColIdx grpIdxs false
    (v, some ⟨v.t, v.instR, v.instM, v.instV, tbl', v.curColIdx, v.curRow, v.getGroup⟩)
  | _ => (v.dispatch cmd rowPg colPg, none)

-- | Create View from table + path
def mkView {τ : Type} [ReadTable τ] [ModifyTable τ] [RenderTable τ]
    (tbl : τ) (path : String) (col : Nat := 0) (grp : Array String := #[]) (row : Nat := 0)
    : Option View := do
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then
      some (View.new (NavState.newAt tbl rfl rfl hr hc col grp row) path)
    else none
  else none

-- | Rebuild View from RebuildInfo
def rebuildView (path : String) (info : RebuildInfo) : Option View :=
  @mkView info.t info.instR info.instM info.instV info.tbl path info.col info.grp info.row

-- | Main loop with ViewStack
partial def mainLoop (stk : ViewStack) (vs : ViewState) (gPrefix : Bool := false) : IO Unit := do
  let vs' ← stk.cur.doRender vs
  let ev ← Term.pollEvent
  let h ← Term.height
  let rowPg := (h.toNat - reservedLines) / 2
  let colPg := colPageSize
  -- check special keys
  if ev.type == Term.eventKey then
    let c := Char.ofNat ev.ch.toNat
    -- q = pop/quit
    if c == 'q' || ev.key == Term.keyEsc then
      match stk.pop with
      | none => return
      | some stk' => return ← mainLoop stk' ViewState.default
    -- S = swap
    if c == 'S' then return ← mainLoop stk.swap vs'
    -- g prefix
    if c == 'g' && !gPrefix then return ← mainLoop stk vs' true
  -- regular Cmd
  match evToCmd ev gPrefix with
  | some cmd =>
    let (v', info) := execCmd stk.cur cmd rowPg colPg
    match info with
    | some ri =>
      match rebuildView stk.cur.path ri with
      | some v'' => mainLoop (stk.setCur v'') ViewState.default
      | none => return  -- table empty after del
    | none => mainLoop (stk.setCur v') vs'
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
    | .ok tbl =>
      match mkView tbl path with
      | some v => mainLoop ⟨v, #[]⟩ ViewState.default; Term.shutdown
      | none => Term.shutdown; IO.eprintln "Empty table"
  else
    let ok ← Backend.init
    if !ok then Term.shutdown; IO.eprintln "Backend init failed"; return
    match ← AdbcTable.fromFile path with
    | none => Term.shutdown; Backend.shutdown; IO.eprintln "Query failed"
    | some tbl =>
      match mkView tbl path with
      | some v => mainLoop ⟨v, #[]⟩ ViewState.default; Term.shutdown; Backend.shutdown
      | none => Term.shutdown; Backend.shutdown; IO.eprintln "Empty table"
