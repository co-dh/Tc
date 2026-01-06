/-
  App with ViewStack support
  CSV: pure Lean MemTable, other: ADBC (DuckDB+PRQL)
-/
import Tc.Data.ADBC.Meta
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Text
import Tc.Fzf
import Tc.Key
import Tc.Render
import Tc.Term
import Tc.Theme
import Tc.UI.Info
import Tc.Dispatch

open Tc

-- | Main loop. Returns final AppState for pipe mode output
partial def mainLoop (a : AppState) (testMode : Bool) (keys : Array Char) : IO AppState := do
  let (vs', v') ← a.stk.cur.doRender a.vs a.theme.styles  -- v' has updated widths
  let a := { a with stk := a.stk.setCur v', vs := vs' }
  renderTabLine a.stk.tabNames 0
  if a.info.vis then UI.Info.render (← Term.height).toNat (← Term.width).toNat
  Term.present
  -- test mode: exit after render when keys exhausted
  if testMode && keys.isEmpty then IO.print (← Term.bufferStr); return a
  let (ev, keys') ← nextEvent keys
  -- Q=quit
  if isKey ev 'Q' then return a
  -- ,/. prefix: fzf menu for object selection
  let cmd? ← if isKey ev ',' then Fzf.prefixCmd .inc
             else if isKey ev '.' then Fzf.prefixCmd .dec
             else pure (evToCmd ev none)
  -- no cmd (unrecognized key): continue loop unchanged
  let some cmd := cmd? | mainLoop a testMode keys'
  -- exec returns none when table becomes empty (e.g. delete all cols): quit
  let some a' ← a.exec cmd | return a
  mainLoop a' testMode keys'

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

-- | Output table as plain text (for pipe mode)
def outputTable (a : AppState) : IO Unit := do
  IO.println (← a.stk.cur.nav.tbl.toText)

-- | Run app with view, returns final AppState
def runApp (v : View) (pipeMode testMode : Bool) (theme : Theme.State) (keys : Array Char) : IO AppState := do
  if pipeMode then let _ ← Term.reopenTty
  let _ ← Term.init
  let a : AppState := ⟨⟨#[v], by simp⟩, .default, theme, {}⟩
  let a' ← mainLoop a testMode keys
  Term.shutdown
  pure a'

-- | Run with MemTable result, returns AppState if successful
def runMem (res : Except String MemTable) (name : String) (pipeMode testMode : Bool)
    (theme : Theme.State) (keys : Array Char) : IO (Option AppState) := do
  let tbl ← match res with
    | .error e => IO.eprintln s!"Parse error: {e}"; return none
    | .ok t => pure t
  let some v := View.fromTbl (.mem tbl) name | IO.eprintln "Empty table"; return none
  some <$> runApp v pipeMode testMode theme keys


-- | Entry point
def main (args : List String) : IO Unit := do
  let (path?, keys, testMode) := parseArgs args
  Fzf.setTestMode testMode
  -- pipe mode: stdin is not tty AND not test mode (test mode uses -c for replay)
  let pipeMode ← if testMode then pure false else (! ·) <$> Term.isattyStdin
  let theme ← Theme.State.init
  -- stdin mode if piped
  if pipeMode && path?.isNone then
    if let some a ← runMem (← MemTable.fromStdin) "stdin" true testMode theme keys then outputTable a
    return
  -- init ADBC for parquet support (used by folder view and CLI)
  let ok ← AdbcTable.init
  if !ok then IO.eprintln "Backend init failed"; return
  let path := path?.getD ""
  try
    if path.isEmpty then  -- no file: show current directory as folder view
      match ← Folder.mkView "." 1 with
      | some v => let _ ← runApp v pipeMode testMode theme keys
      | none => IO.eprintln "Cannot list directory"
    else if path.endsWith ".txt" then  -- txt: parse as space-separated text
      let _ ← runMem (MemTable.fromText (← IO.FS.readFile path)) path pipeMode testMode theme keys
    else  -- csv/parquet: use View.fromFile
      match ← View.fromFile path with
      | some v => let _ ← runApp v pipeMode testMode theme keys
      | none => IO.eprintln "Cannot open file"
  finally
    AdbcTable.shutdown
