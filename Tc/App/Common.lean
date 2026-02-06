-- App/Common: shared app loop, effect runner, arg parsing, generic main
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

-- run effect, recurse on fzfCmd
partial def runEffect (a : AppState T) (e : Effect) : IO (AppState T) := match e with
  | .none | .quit => pure a
  | .fzfCmd => do match ← Fzf.cmdMode a.stk.cur.vkind with
    | some c => match a.update c with
      | some (a', e') => if e'.isNone then pure a' else runEffect a' e'
      | none => pure a
    | none => pure a
  | .themeLoad d => do let th ← a.theme.runEffect d; pure { a with theme := th }
  | _ => do let s ← Runner.runStackEffect a.stk e; pure { a with stk := s, vs := if e.isNone then a.vs else .default }

-- main loop: render → input → update → effect → loop
partial def mainLoop (a : AppState T) (test : Bool) (ks : Array Char) : IO (AppState T) := do
  let (vs', v') ← a.stk.cur.doRender a.vs a.theme.styles
  let a := { a with stk := a.stk.setCur v', vs := vs' }
  renderTabLine a.stk.tabNames 0
  if a.info.vis then UI.Info.render (← Term.height).toNat (← Term.width).toNat a.stk.cur.vkind
  Term.present
  if test && ks.isEmpty then IO.print (← Term.bufferStr); return a
  let (ev, ks') ← nextEvent ks
  if isKey ev 'Q' then return a
  if isKey ev ' ' then mainLoop (← runEffect a .fzfCmd) test ks'
  else match evToCmd ev a.stk.cur.vkind with
    | none => mainLoop a test ks'
    | some c => match a.update c with
      | none => mainLoop a test ks'
      | some (a', e) => if e == .quit then pure a' else mainLoop (← if e.isNone then pure a' else runEffect a' e) test ks'

-- parse args: path?, -c keys?, test mode, +n (S3 no-sign-request)
def parseArgs (args : List String) : Option String × Array Char × Bool × Bool :=
  let noSign := args.any (· == "+n")
  let args := args.filter (· != "+n")
  let toK s := (parseKeys s).toList.toArray
  match args with
  | "-c" :: k :: _ => (none, toK k, true, noSign)
  | p :: "-c" :: k :: _ => (some p, toK k, true, noSign)
  | p :: _ => (some p, #[], false, noSign)
  | [] => (none, #[], false, noSign)

-- run app with view
def runApp (v : View T) (pipe test : Bool) (th : Theme.State) (ks : Array Char) : IO (AppState T) := do
  if pipe then let _ ← Term.reopenTty
  let _ ← Term.init
  let a' ← mainLoop ⟨⟨#[v], by simp⟩, .default, th, {}⟩ test ks
  Term.shutdown; pure a'

-- run from MemTable result
def runMem (r : Except String MemTable) (nm : String) (pipe test : Bool)
    (th : Theme.State) (ks : Array Char) : IO (Option (AppState T)) := do
  match r with
  | .error e => IO.eprintln s!"Parse error: {e}"; return none
  | .ok t => match View.fromTbl (MemConvert.wrap t) nm with
    | none => IO.eprintln "Empty table"; return none
    | some v => some <$> runApp v pipe test th ks

-- output table as plain text
def outputTable [TblOps T] (toText : T → IO String) (a : AppState T) : IO Unit := do
  IO.println (← toText a.stk.cur.nav.tbl)

-- generic main: works for any Table type with TblOps/ModifyTable/MemConvert
-- init/shutdown passed as params since Backend is defined per-variant
def appMain [TblOps T] [ModifyTable T] [MemConvert MemTable T]
    (toText : T → IO String) (init : IO Bool) (shutdown : IO Unit) (args : List String) : IO Unit := do
  let (path?, keys, testMode, noSign) := parseArgs args
  let envTest := (← IO.getEnv "TC_TEST_MODE").isSome
  Fzf.setTestMode (testMode || envTest)
  Folder.setS3NoSign noSign
  let pipeMode ← if testMode then pure false else (! ·) <$> Term.isattyStdin
  let theme ← Theme.State.init
  let ok ← try init catch e => IO.eprintln s!"Backend init error: {e}"; return
  if !ok then IO.eprintln "Backend init failed"; return
  if pipeMode && path?.isNone then
    if let some a ← runMem (T := T) (← MemTable.fromStdin) "stdin" true testMode theme keys then
      outputTable toText a
    return
  let path := path?.getD ""
  try
    if path.isEmpty then
      match ← Folder.mkView (T := T) "." 1 with
      | some v => let _ ← runApp (T := T) v pipeMode testMode theme keys
      | none => IO.eprintln "Cannot list directory"
    else if path.startsWith "s3://" then
      match ← Folder.mkView (T := T) path 1 with
      | some v => let _ ← runApp (T := T) v pipeMode testMode theme keys
      | none => IO.eprintln s!"Cannot browse S3 path: {path}"
    else if path.startsWith "kdb://" then
      match ← TblOps.fromUrl (α := T) path with
      | some tbl => match View.fromTbl tbl path with
        | some v => let _ ← runApp (T := T) v pipeMode testMode theme keys
        | none => IO.eprintln "Empty kdb table"
      | none => IO.eprintln "Cannot open kdb table"
    else if path.endsWith ".txt" then
      let _ ← runMem (T := T) (MemTable.fromText (← IO.FS.readFile path)) path pipeMode testMode theme keys
    else match ← TblOps.fromFile (α := T) path with
      | some tbl => match View.fromTbl tbl path with
        | some v => let _ ← runApp (T := T) v pipeMode testMode theme keys
        | none => IO.eprintln "Cannot open file (empty table)"
      | none => pure ()
  finally
    shutdown
