/-
  App Core: Minimal CSV-only table viewer
  No ADBC, no Kdb, no folder view - just MemTable from stdin/csv
-/
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Text
import Tc.Fzf
import Tc.Key
import Tc.Render
import Tc.Term
import Tc.Theme
import Tc.Table.Mem
import Tc.Backend.Core

open Tc

-- | Simple View for core (no folder support)
structure CoreView where
  nRows : Nat
  nCols : Nat
  nav : NavState nRows nCols Table
  path : String
  vkind : ViewKind := .tbl
  precAdj : Int := 0
  widthAdj : Int := 0
  widths : Array Nat := #[]

namespace CoreView

def new {nr nc : Nat} (nav : NavState nr nc Table) (path : String) : CoreView :=
  ⟨nr, nc, nav, path, .tbl, 0, 0, #[]⟩

def tabName (v : CoreView) : String :=
  v.path.splitOn "/" |>.getLast? |>.getD v.path

def doRender (v : CoreView) (vs : ViewState) (styles : Array UInt32) : IO (ViewState × CoreView) := do
  let (vs', widths) ← render v.nav vs v.widths styles v.precAdj v.widthAdj
  pure (vs', { v with widths })

def fromTbl (tbl : Table) (path : String) : Option CoreView := do
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then some (CoreView.new (NavState.newAt tbl rfl rfl hr hc 0 #[] 0) path)
    else none
  else none

-- | Load from file (CSV only)
def fromFile (path : String) : IO (Option CoreView) := do
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .error e => IO.eprintln s!"CSV error: {e}"; pure none
    | .ok tbl => pure (fromTbl (.mem tbl) path)
  else
    IO.eprintln s!"Core build only supports .csv files, not: {path}"
    pure none

private def verbDelta (verb : Verb) : Int := if verb == .inc then 1 else -1

-- | Execute navigation command
def exec (v : CoreView) (cmd : Cmd) (rowPg : Nat) : IO (Option CoreView) := do
  let n := v.nav; let names := ReadTable.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  match cmd with
  | .prec verb  => pure (some { v with precAdj := v.precAdj + verbDelta verb })
  | .width verb => pure (some { v with widthAdj := v.widthAdj + verbDelta verb })
  | .colSel .del =>
    let (tbl', grp') ← ModifyTable.del n.tbl curCol (n.col.sels.filterMap names.idxOf?) n.grp
    pure (fromTbl tbl' v.path)
  | .colSel .inc | .colSel .dec =>
    let tbl' ← ModifyTable.sort n.tbl curCol (n.grp.filterMap names.idxOf?) (cmd == .colSel .inc)
    pure (fromTbl tbl' v.path)
  | _ => pure <| match NavState.exec cmd n rowPg colPageSize with
    | some nav' => some { v with nav := nav' }
    | none => none

end CoreView

-- | Simple AppState for core
structure CoreAppState where
  view : CoreView
  vs : ViewState := .default
  theme : Theme.State

-- | Main loop
partial def mainLoop (a : CoreAppState) (testMode : Bool) (keys : Array Char) : IO CoreAppState := do
  -- Render
  let (vs', v') ← a.view.doRender a.vs a.theme.styles
  let a := { a with view := v', vs := vs' }
  renderTabLine #[a.view.tabName] 0
  Term.present

  -- Exit in test mode when keys exhausted
  if testMode && keys.isEmpty then IO.print (← Term.bufferStr); return a

  -- Get input
  let (ev, keys') ← nextEvent keys
  if isKey ev 'Q' || isKey ev 'q' then return a

  -- Map event to cmd
  let some cmd := evToCmd ev a.view.vkind | mainLoop a testMode keys'
  let rowPg := ((← Term.height).toNat - reservedLines) / 2

  -- Execute
  match ← a.view.exec cmd rowPg with
  | some v' => mainLoop { a with view := v' } testMode keys'
  | none => mainLoop a testMode keys'

-- | Output table as text
def outputTable (v : CoreView) : IO Unit := do
  IO.println (← v.nav.tbl.toText)

-- | Parse args
def parseArgs (args : List String) : Option String × Array Char × Bool :=
  let toKeys s := (parseKeys s).toList.toArray
  let (path, rest) := match args with
    | "-c" :: t => (none, t)
    | p :: "-c" :: t => (some p, t)
    | p :: _ => (some p, [])
    | [] => (none, [])
  match rest with
  | k :: _ => (path, toKeys k, true)
  | [] => (path, #[], false)

-- | Entry point
def main (args : List String) : IO Unit := do
  let (path?, keys, testMode) := parseArgs args
  let envTest := (← IO.getEnv "TC_TEST_MODE").isSome
  Fzf.setTestMode (testMode || envTest)
  let pipeMode ← if testMode then pure false else (! ·) <$> Term.isattyStdin
  let theme ← Theme.State.init

  -- No backend init needed for core (Backend.Core.init is no-op)
  let _ ← Backend.init

  try
    if pipeMode && path?.isNone then
      -- stdin mode
      match ← MemTable.fromStdin with
      | .error e => IO.eprintln s!"Parse error: {e}"
      | .ok tbl =>
        if let some v := CoreView.fromTbl (.mem tbl) "stdin" then
          if pipeMode then let _ ← Term.reopenTty
          let _ ← Term.init
          let a' ← mainLoop ⟨v, .default, theme⟩ testMode keys
          Term.shutdown
          outputTable a'.view
    else if let some path := path? then
      if path.endsWith ".txt" then
        -- Space-separated text
        match MemTable.fromText (← IO.FS.readFile path) with
        | .error e => IO.eprintln s!"Parse error: {e}"
        | .ok tbl =>
          if let some v := CoreView.fromTbl (.mem tbl) path then
            if pipeMode then let _ ← Term.reopenTty
            let _ ← Term.init
            let _ ← mainLoop ⟨v, .default, theme⟩ testMode keys
            Term.shutdown
      else
        -- CSV or unsupported
        match ← CoreView.fromFile path with
        | some v =>
          if pipeMode then let _ ← Term.reopenTty
          let _ ← Term.init
          let _ ← mainLoop ⟨v, .default, theme⟩ testMode keys
          Term.shutdown
        | none => pure ()
    else
      IO.eprintln "Usage: tc-core <file.csv>"
      IO.eprintln "Core build only supports .csv and .txt files"
  finally
    Backend.shutdown
