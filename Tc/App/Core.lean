/-
  App/Core: Core build entry point (MemTable only, no ADBC/Kdb)
  Supports CSV files and stdin only.
-/
import Tc.Table.Mem  -- core build: mem only
import Tc.Data.Mem.Text
import Tc.Fzf
import Tc.Key
import Tc.Render
import Tc.Runner
import Tc.Term
import Tc.Theme
import Tc.UI.Info
import Tc.Dispatch

open Tc

-- | Run effect on AppState, returns updated state
partial def runEffect (a : AppState Table) (eff : Effect) : IO (AppState Table) := do
  match eff with
  | .none => pure a
  | .quit => pure a  -- handled by caller
  | .fzfCmd =>
    match ← Fzf.cmdMode a.stk.cur.vkind with
    | some cmd =>
      match a.update cmd with
      | some (a', eff') => if eff'.isNone then pure a' else runEffect a' eff'
      | none => pure a
    | none => pure a
  | .themeLoad delta =>
    let t' ← a.theme.runEffect delta
    pure { a with theme := t' }
  | _ =>
    let stk' ← Runner.runStackEffect a.stk eff
    pure { a with stk := stk', vs := if eff.isNone then a.vs else .default }

-- | Main loop
partial def mainLoop (a : AppState Table) (testMode : Bool) (keys : Array Char) : IO (AppState Table) := do
  let (vs', v') ← a.stk.cur.doRender a.vs a.theme.styles
  let a := { a with stk := a.stk.setCur v', vs := vs' }
  renderTabLine a.stk.tabNames 0
  if a.info.vis then UI.Info.render (← Term.height).toNat (← Term.width).toNat a.stk.cur.vkind
  Term.present
  if testMode && keys.isEmpty then IO.print (← Term.bufferStr); return a
  let (ev, keys') ← nextEvent keys
  if isKey ev 'Q' then return a
  if isKey ev ' ' then
    let a' ← runEffect a .fzfCmd
    mainLoop a' testMode keys'
  else
    let some cmd := evToCmd ev a.stk.cur.vkind | mainLoop a testMode keys'
    let some (a', eff) := a.update cmd | mainLoop a testMode keys'
    if eff == .quit then return a'
    let a'' ← if eff.isNone then pure a' else runEffect a' eff
    mainLoop a'' testMode keys'

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

-- | Output table as plain text
def outputTable (a : AppState Table) : IO Unit := do
  IO.println (← Table.toText a.stk.cur.nav.tbl)

-- | Run app with view
def runApp (v : View Table) (pipeMode testMode : Bool) (theme : Theme.State) (keys : Array Char) : IO (AppState Table) := do
  if pipeMode then let _ ← Term.reopenTty
  let _ ← Term.init
  let a : AppState Table := ⟨⟨#[v], by simp⟩, .default, theme, {}⟩
  let a' ← mainLoop a testMode keys
  Term.shutdown
  pure a'

-- | Run with MemTable result
def runMem (res : Except String MemTable) (name : String) (pipeMode testMode : Bool)
    (theme : Theme.State) (keys : Array Char) : IO (Option (AppState Table)) := do
  let tbl ← match res with
    | .error e => IO.eprintln s!"Parse error: {e}"; return none
    | .ok t => pure t
  let some v := View.fromTbl (Table.mem tbl) name | IO.eprintln "Empty table"; return none
  some <$> runApp v pipeMode testMode theme keys

-- | Entry point (core build: CSV only)
def main (args : List String) : IO Unit := do
  let (path?, keys, testMode) := parseArgs args
  let envTest := (← IO.getEnv "TC_TEST_MODE").isSome
  Fzf.setTestMode (testMode || envTest)
  let pipeMode ← if testMode then pure false else (! ·) <$> Term.isattyStdin
  let theme ← Theme.State.init
  let ok ← Backend.init  -- no-op for core
  if !ok then IO.eprintln "Backend init failed"; return
  if pipeMode && path?.isNone then
    if let some a ← runMem (← MemTable.fromStdin) "stdin" true testMode theme keys then outputTable a
    return
  let path := path?.getD ""
  try
    if path.isEmpty then  -- no file: show current directory as folder view
      match ← Folder.mkView (T := Table) "." 1 with
      | some v => let _ ← runApp v pipeMode testMode theme keys
      | none => IO.eprintln "Cannot list directory"
    else if path.endsWith ".txt" then  -- txt: parse as space-separated text
      let _ ← runMem (MemTable.fromText (← IO.FS.readFile path)) path pipeMode testMode theme keys
    else  -- use Table.fromFile (CSV only in core)
      match ← Table.fromFile path with
      | some tbl => match View.fromTbl tbl path with
        | some v => let _ ← runApp v pipeMode testMode theme keys
        | none => IO.eprintln "Cannot open file (empty table)"
      | none => pure ()  -- error already printed by Table.fromFile
  finally
    Backend.shutdown
