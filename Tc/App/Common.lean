-- App/Common: shared app loop, effect runner, arg parsing, generic main
import Tc.Fzf
import Tc.Key
import Tc.Render
import Tc.Runner
import Tc.Term
import Tc.Theme
import Tc.UI.Info
import Tc.Dispatch
import Tc.Data.Text
import Tc.Folder
import Tc.View

open Tc

-- run effect, recurse on fzfCmd
partial def runEffect (a : AppState) (e : Effect) : IO AppState := match e with
  | .none | .quit => pure a
  | .fzf .cmd => do match ← Fzf.cmdMode a.stk.cur.vkind with
    | some c => match a.update c with
      | some (a', e') => if e'.isNone then pure a' else runEffect a' e'
      | none => pure a
    | none => pure a
  | .themeLoad d => do let th ← a.theme.runEffect d; pure { a with theme := th }
  | _ => do let s ← Runner.runStackEffect a.stk e; pure { a with stk := s, vs := if e.isNone then a.vs else .default }

-- main loop: render → input → update → effect → loop
partial def mainLoop (a : AppState) (test : Bool) (ks : Array Char) : IO AppState := do
  let (vs', v') ← a.stk.cur.doRender a.vs a.theme.styles
  let a := { a with stk := a.stk.setCur v', vs := vs' }
  renderTabLine a.stk.tabNames 0
  if a.info.vis then UI.Info.render (← Term.height).toNat (← Term.width).toNat a.stk.cur.vkind
  Term.present
  if test && ks.isEmpty then IO.print (← Term.bufferStr); return a
  let (ev, ks') ← nextEvent ks
  if isKey ev 'Q' then return a
  if isKey ev ' ' then mainLoop (← runEffect a (.fzf .cmd)) test ks'
  else match evToCmd ev a.stk.cur.vkind with
    | none => mainLoop a test ks'
    | some c => match a.update c with
      | none => mainLoop a test ks'
      | some (a', e) =>
        if e == .quit then pure a'
        else let a'' ← if e.isNone then pure a' else runEffect a' e
             mainLoop a'' test ks'

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
def runApp (v : View Table) (pipe test : Bool) (th : Theme.State) (ks : Array Char) : IO AppState := do
  if pipe then let _ ← Term.reopenTty
  let _ ← Term.init
  let a' ← mainLoop ⟨⟨v, []⟩, .default, th, {}⟩ test ks
  Term.shutdown; pure a'

-- run from TSV string result
def runTsv (r : Except String String) (nm : String) (pipe test : Bool)
    (th : Theme.State) (ks : Array Char) : IO (Option AppState) := do
  match r with
  | .error e => IO.eprintln s!"Parse error: {e}"; return none
  | .ok tsv =>
    match ← AdbcTable.fromTsv tsv with
    | none => IO.eprintln "Empty table"; return none
    | some adbc => match View.fromTbl (Table.adbc adbc) nm with
      | none => IO.eprintln "Empty table"; return none
      | some v => some <$> runApp v pipe test th ks

-- output table as plain text
def outputTable (toText : Table → IO String) (a : AppState) : IO Unit := do
  IO.println (← toText a.stk.cur.nav.tbl)

-- main entry point: init backend, parse args, run app
def appMain (toText : Table → IO String) (init : IO Bool) (shutdown : IO Unit) (args : List String) : IO Unit := do
  let (path?, keys, testMode, noSign) := parseArgs args
  let envTest := (← IO.getEnv "TC_TEST_MODE").isSome
  Fzf.setTestMode (testMode || envTest)
  S3.setNoSign noSign
  let pipeMode ← if testMode then pure false else (! ·) <$> Term.isattyStdin
  let theme ← Theme.State.init
  let ok ← try init catch e => IO.eprintln s!"Backend init error: {e}"; return
  if !ok then IO.eprintln "Backend init failed"; return
  if pipeMode && path?.isNone then
    if let some a ← runTsv (← Tc.TextParse.fromStdin) "stdin" true testMode theme keys then
      outputTable toText a
    return
  let path := path?.getD ""
  try
    if path.isEmpty || path.startsWith "s3://" || path.startsWith "hf://" then
      let p := if path.isEmpty then "." else path
      match ← Folder.mkView p 1 with
      | some v => let _ ← runApp v pipeMode testMode theme keys
      | none => IO.eprintln s!"Cannot browse: {p}"
    else if path.startsWith "kdb://" then
      match ← TblOps.fromUrl (α := Table) path with
      | some tbl => match View.fromTbl tbl path with
        | some v => let _ ← runApp v pipeMode testMode theme keys
        | none => IO.eprintln "Empty kdb table"
      | none => IO.eprintln "Cannot open kdb table"
    else if path.endsWith ".txt" then
      let _ ← runTsv (Tc.TextParse.fromText (← IO.FS.readFile path)) path pipeMode testMode theme keys
    else match ← TblOps.fromFile (α := Table) path with
      | some tbl => match View.fromTbl tbl path with
        | some v => let _ ← runApp v pipeMode testMode theme keys
        | none => IO.eprintln "Cannot open file (empty table)"
      | none => pure ()
  finally
    shutdown
