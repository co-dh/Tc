/-
  App/Common.lean: Common code for App entry points
-/
import Tc.Fzf
import Tc.Key
import Tc.Render
import Tc.Runner
import Tc.Term
import Tc.Theme
import Tc.UI.Info
import Tc.Dispatch
import Tc.Data.Mem.Text
import Tc.Folder
import Tc.View

open Tc

variable {T : Type} [TblOps T] [ModifyTable T] [MemConvert MemTable T]

-- | Run effect on AppState, returns updated state
partial def runEffect (a : AppState T) (eff : Effect) : IO (AppState T) := do
  match eff with
  | .none => pure a
  | .quit => pure a  -- handled by caller
  | .fzfCmd =>
    -- command mode: fzf object → fzf verb → execute resulting cmd
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
    -- delegate stack effects to Runner
    let stk' ← Runner.runStackEffect a.stk eff
    pure { a with stk := stk', vs := if eff.isNone then a.vs else .default }

-- | Main loop. Returns final AppState for pipe mode output
-- Uses pure update + effect runner pattern
partial def mainLoop (a : AppState T) (testMode : Bool) (keys : Array Char) : IO (AppState T) := do
  -- 1. Render (IO)
  let (vs', v') ← a.stk.cur.doRender a.vs a.theme.styles
  let a := { a with stk := a.stk.setCur v', vs := vs' }
  renderTabLine a.stk.tabNames 0
  if a.info.vis then UI.Info.render (← Term.height).toNat (← Term.width).toNat a.stk.cur.vkind
  Term.present

  -- 2. Exit in test mode when keys exhausted
  if testMode && keys.isEmpty then IO.print (← Term.bufferStr); return a

  -- 3. Get input (IO)
  let (ev, keys') ← nextEvent keys
  if isKey ev 'Q' then return a

  -- 4. Handle space → fzfCmd effect, else map event to cmd
  if isKey ev ' ' then
    let a' ← runEffect a .fzfCmd
    mainLoop a' testMode keys'
  else
    let some cmd := evToCmd ev a.stk.cur.vkind | mainLoop a testMode keys'
    -- 5. Pure update: returns (state', effect)
    let some (a', eff) := a.update cmd | mainLoop a testMode keys'
    -- 6. Check for quit effect
    if eff == .quit then return a'
    -- 7. Run effect (IO)
    let a'' ← if eff.isNone then pure a' else runEffect a' eff
    -- 8. Loop
    mainLoop a'' testMode keys'

-- | Parse args: path, optional -c for key replay (test mode)
def parseArgs (args : List String) : Option String × Array Char × Bool :=
  let toKeys s := (parseKeys s).toList.toArray
  let (path, rest) := match args with
    | "-c" :: t => (none, t)  -- no path, -c first
    | p :: "-c" :: t => (some p, t)  -- path then -c
    | p :: _ => (some p, [])
    | [] => (none, [])
  match rest with
  | k :: _ => (path, toKeys k, true)  -- test mode with keys
  | [] => (path, #[], false)

-- | Run app with view, returns final AppState
def runApp (v : View T) (pipeMode testMode : Bool) (theme : Theme.State) (keys : Array Char) : IO (AppState T) := do
  if pipeMode then let _ ← Term.reopenTty
  let _ ← Term.init
  let a : AppState T := ⟨⟨#[v], by simp⟩, .default, theme, {}⟩
  let a' ← mainLoop a testMode keys
  Term.shutdown
  pure a'

-- | Run with MemTable result, returns AppState if successful
def runMem (res : Except String MemTable) (name : String) (pipeMode testMode : Bool)
    (theme : Theme.State) (keys : Array Char) : IO (Option (AppState T)) := do
  let tbl ← match res with
    | .error e => IO.eprintln s!"Parse error: {e}"; return none
    | .ok t => pure t
  let some v := View.fromTbl (MemConvert.wrap tbl) name | IO.eprintln "Empty table"; return none
  some <$> runApp v pipeMode testMode theme keys
