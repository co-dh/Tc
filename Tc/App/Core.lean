/-
  App Core: CSV-only table viewer with full UI features
  No ADBC, no Kdb, no folder view - just MemTable from stdin/csv
  Uses ViewStack with Meta/Freq/Search support
-/
import Tc.Fzf
import Tc.Key
import Tc.Render
import Tc.Term
import Tc.Theme
import Tc.UI.Info.Core
import Tc.View.Core
import Tc.Backend.Core

open Tc

-- | App state
structure AppState where
  stk : ViewStack
  vs : ViewState := .default
  theme : Theme.State

namespace AppState

-- | Current view
def cur (a : AppState) : View := a.stk.cur

-- | Update current view in stack
def setCur (a : AppState) (v : View) : AppState :=
  { a with stk := a.stk.setCur v }

-- | Tab names for display
def tabNames (a : AppState) : Array String := a.stk.tabNames

end AppState

-- | Execute stack effect (meta push, freq push, etc.)
def runStackEffect (stk : ViewStack) (eff : Effect) : IO (Option ViewStack) := do
  let v := stk.cur; let n := v.nav
  match eff with
  | .none => pure (some stk)
  | .queryMeta => Meta.push stk
  | .queryFreq _ _ => Freq.push stk
  | .queryDel curCol sels grp =>
    let (tbl', grp') ← ModifyTable.del n.tbl curCol sels grp
    match View.fromTbl tbl' v.path n.col.cur.val grp' 0 with
    | some v' => pure (some (stk.setCur { v' with precAdj := v.precAdj, widthAdj := v.widthAdj }))
    | none => pure none
  | .querySort curCol grpIdxs asc =>
    let tbl' ← ModifyTable.sort n.tbl curCol grpIdxs asc
    match View.fromTbl tbl' v.path curCol n.grp n.row.cur.val with
    | some v' => pure (some (stk.setCur { v' with precAdj := v.precAdj, widthAdj := v.widthAdj }))
    | none => pure (some stk)
  | .queryFilter expr =>
    match ← QueryTable.filter n.tbl expr with
    | some tbl' =>
      match View.fromTbl tbl' v.path with
      | some v' => pure (some (stk.setCur { v' with precAdj := v.precAdj, widthAdj := v.widthAdj }))
      | none => pure (some stk)
    | none => pure (some stk)
  | _ => pure (some stk)  -- other effects not used in core

-- | Dispatch command to appropriate handler
def dispatch (a : AppState) (cmd : Cmd) (rowPg : Nat) : IO (Option AppState) := do
  let stk := a.stk; let v := stk.cur
  -- Try meta commands
  if let some stk' ← Meta.exec stk cmd then
    return some { a with stk := stk' }
  -- Try freq commands
  if let some stk' ← Freq.exec stk cmd then
    return some { a with stk := stk' }
  -- Try search/filter commands
  if let some stk' ← Filter.exec stk cmd then
    return some { a with stk := stk' }
  -- Stack operations
  match cmd with
  | .stk .dec =>  -- pop
    match stk.pop with
    | some stk' => pure (some { a with stk := stk' })
    | none => pure none  -- quit on empty stack
  | .stk .ent => pure (some { a with stk := stk.swap })  -- swap
  | .stk .dup => pure (some { a with stk := stk.dup })
  | .info _ => pure (some a)  -- info handled in main loop
  | _ =>
    -- View update (navigation, prec, width, colSel)
    match View.update v cmd rowPg with
    | some (v', eff) =>
      match ← runStackEffect (stk.setCur v') eff with
      | some stk' => pure (some { a with stk := stk' })
      | none => pure none
    | none => pure (some a)

-- | Main loop
partial def mainLoop (a : AppState) (testMode : Bool) (keys : Array Char) : IO AppState := do
  -- Render
  let (vs', v') ← a.cur.doRender a.vs a.theme.styles
  let a := a.setCur v' |> fun x => { x with vs := vs' }
  renderTabLine a.tabNames 0
  Term.present

  -- Exit in test mode when keys exhausted
  if testMode && keys.isEmpty then IO.print (← Term.bufferStr); return a

  -- Get input
  let (ev, keys') ← nextEvent keys
  if isKey ev 'Q' then return a

  -- Info overlay
  if isKey ev 'I' then
    Tc.Info.render a.cur.vkind
    Term.present
    let _ ← nextEvent #[]
    return ← mainLoop a testMode keys'

  -- Map event to cmd
  let some cmd := evToCmd ev a.cur.vkind | mainLoop a testMode keys'
  let rowPg := ((← Term.height).toNat - reservedLines) / 2

  -- Execute
  match ← dispatch a cmd rowPg with
  | some a' => mainLoop a' testMode keys'
  | none => return a  -- quit

-- | Output table as text
def outputTable (v : View) : IO Unit := do
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

-- | Create initial ViewStack from View
def mkStack (v : View) : ViewStack := ⟨#[v], by simp⟩

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
        if let some v := View.fromTbl (.mem tbl) "stdin" then
          if pipeMode then let _ ← Term.reopenTty
          let _ ← Term.init
          let a' ← mainLoop ⟨mkStack v, .default, theme⟩ testMode keys
          Term.shutdown
          outputTable a'.cur
    else if let some path := path? then
      if path.endsWith ".txt" then
        -- Space-separated text
        match MemTable.fromText (← IO.FS.readFile path) with
        | .error e => IO.eprintln s!"Parse error: {e}"
        | .ok tbl =>
          if let some v := View.fromTbl (.mem tbl) path then
            if pipeMode then let _ ← Term.reopenTty
            let _ ← Term.init
            let _ ← mainLoop ⟨mkStack v, .default, theme⟩ testMode keys
            Term.shutdown
      else if path.endsWith ".csv" then
        -- CSV
        match ← View.fromFile path with
        | some v =>
          if pipeMode then let _ ← Term.reopenTty
          let _ ← Term.init
          let _ ← mainLoop ⟨mkStack v, .default, theme⟩ testMode keys
          Term.shutdown
        | none => pure ()
      else
        IO.eprintln s!"Core build only supports .csv and .txt files, not: {path}"
    else
      IO.eprintln "Usage: tc-core <file.csv>"
      IO.eprintln "Core build only supports .csv and .txt files"
  finally
    Backend.shutdown
