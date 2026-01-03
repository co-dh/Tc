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
  | sort (curRow : Nat) (curCol : Nat) (tbl : t)  -- sorted table, keep cursor

-- Execute single Cmd, return new nav or LoopAction for del/sort
def execCmd {nRows nCols : Nat} {t : Type} [ModifyTable t]
    (cmd : Cmd) (nav : NavState nRows nCols t) (rowPg colPg : Nat)
    : Sum (NavState nRows nCols t) (LoopAction t) :=
  match cmd with
  | .col .del =>
    let (tbl', grp') := ModifyTable.del nav.tbl nav.curColIdx nav.selColIdxs nav.group
    .inr (.del nav.curDispCol grp' tbl')
  | .colSel .sortAsc =>
    let grpIdxs := nav.group.filterMap nav.colNames.idxOf?  -- group column indices
    .inr (.sort nav.curRow nav.curColIdx (ModifyTable.sort nav.tbl nav.curColIdx grpIdxs true))
  | .colSel .sortDesc =>
    let grpIdxs := nav.group.filterMap nav.colNames.idxOf?
    .inr (.sort nav.curRow nav.curColIdx (ModifyTable.sort nav.tbl nav.curColIdx grpIdxs false))
  | _ => .inl (nav.dispatch cmd rowPg colPg)

-- Main loop: Cmd-driven with g-prefix state
partial def mainLoop {nRows nCols : Nat} {t : Type} [ModifyTable t] [RenderTable t]
    (nav : NavState nRows nCols t) (view : ViewState) (gPrefix : Bool := false)
    : IO (LoopAction t) := do
  let view' ← render nav view
  let ev ← Term.pollEvent
  let h ← Term.height
  let rowPg := (h.toNat - reservedLines) / 2
  let colPg := colPageSize
  -- check special keys first
  match evToSpecial ev with
  | some .quit => return .quit
  | some .gPrefix => if !gPrefix then return ← mainLoop nav view' true else pure ()
  | none => pure ()
  -- then regular Cmd
  match evToCmd ev gPrefix with
  | some cmd => match execCmd cmd nav rowPg colPg with
    | .inl nav' => mainLoop nav' view'
    | .inr act  => return act
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
  match evToSpecial ev with
  | some .quit => return
  | some .gPrefix => if !gPrefix then return ← loopRO nav view' true else pure ()
  | none => pure ()
  match evToCmd ev gPrefix with
  | some cmd => loopRO (nav.dispatch cmd rowPg colPg) view'
  | none => loopRO nav view'

-- Play command string (space-separated), render after each
partial def playCmds {nRows nCols : Nat} {t : Type} [ModifyTable t] [RenderTable t]
    (cmds : Array Cmd) (i : Nat) (nav : NavState nRows nCols t) (view : ViewState)
    : IO (LoopAction t) := do
  let view' ← render nav view
  let th ← Term.height
  let rowPg := (th.toNat - reservedLines) / 2
  let colPg := colPageSize
  if hi : i < cmds.size then
    match execCmd cmds[i] nav rowPg colPg with
    | .inl nav' => playCmds cmds (i + 1) nav' view'
    | .inr act  => return act
  else mainLoop nav view'  -- done, switch to interactive

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

-- Run viewer with delete support, optionally play command string first
partial def runViewerMod {t : Type} [ModifyTable t] [RenderTable t]
    (tbl : t) (initCol : Nat := 0) (grp : Array String := #[])
    (cmdStr : String := "") (initRow : Nat := 0) : IO Unit := do
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then
      let nav := NavState.newAt tbl rfl rfl hr hc initCol grp initRow
      let cmds := Cmd.parseMany cmdStr
      let act ← if cmds.isEmpty then mainLoop nav ViewState.default
                else playCmds cmds 0 nav ViewState.default
      match act with
      | .quit => pure ()
      | .del col grp' tbl' => runViewerMod tbl' col grp' ""
      | .sort row col tbl' => runViewerMod tbl' col grp "" row  -- keep cursor
    else IO.eprintln "No rows"
  else IO.eprintln "No columns"

-- Parse -c <cmd> from args, return (path, cmdStr)
def parseArgs (args : List String) : String × String :=
  match args with
  | "-c" :: cmd :: rest => (rest.head?.getD "data.csv", cmd)
  | path :: "-c" :: cmd :: _ => (path, cmd)
  | path :: _ => (path, "")
  | [] => ("data.csv", "")

-- Entry point: tc [-c cmd] <file> or tc <file> [-c cmd]
def main (args : List String) : IO Unit := do
  let (path, cmdStr) := parseArgs args
  let _ ← Term.init
  -- CSV: use MemTable (with delete support), otherwise: use Backend (read-only)
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .error e => Term.shutdown; IO.eprintln s!"CSV parse error: {e}"
    | .ok tbl  => runViewerMod tbl 0 #[] cmdStr; Term.shutdown
  else
    let ok ← Backend.init
    if !ok then Term.shutdown; IO.eprintln "Backend init failed"; return
    let prql := s!"from `{path}` | take 100000"
    match ← Backend.query (Backend.mkLimited prql 100000) with
    | none     => Term.shutdown; Backend.shutdown; IO.eprintln "Query failed"
    | some stbl => runViewerMod (MemTable.ofAdbcTable stbl) 0 #[] cmdStr; Term.shutdown; Backend.shutdown
