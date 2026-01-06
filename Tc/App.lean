/-
  App with ViewStack support
  CSV: pure Lean MemTable, other: ADBC (DuckDB+PRQL)
-/
import Tc.Data.ADBC.Meta
import Tc.Data.Mem.Meta
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
  -- +/- prefix: fzf menu for object selection
  let cmd? ← if isKey ev '+' then Fzf.prefixCmd .inc
             else if isKey ev '-' then Fzf.prefixCmd .dec
             else pure (evToCmd ev none)
  -- no cmd (unrecognized key): continue loop unchanged
  let some cmd := cmd? | mainLoop a testMode keys'
  -- exec returns none when table becomes empty (e.g. delete all cols): quit
  let some a' ← a.exec cmd | return a
  mainLoop a' testMode keys'

-- | Parse args: path, optional -c for key replay (test mode)
def parseArgs (args : List String) : Option String × Array Char × Bool :=
  let keys s := (parseKeys s).toList.toArray
  match args with
  | [p, "-c", k] | p :: "-c" :: k :: _ => (some p, keys k, true)  -- path + keys
  | ["-c", k] | "-c" :: k :: _ => (none, keys k, true)  -- keys only (stdin)
  | p :: _ => (some p, #[], false)
  | [] => (none, #[], false)

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
  match res with
  | .error e => IO.eprintln s!"Parse error: {e}"; pure none
  | .ok tbl => match View.fromTbl (.mem tbl) name with
    | some v => pure (some (← runApp v pipeMode testMode theme keys))
    | none => IO.eprintln "Empty table"; pure none

-- | Get ls -l output (skip "total" line via tail)
def lsDir (dir : String) : IO String := do
  let d := if dir.isEmpty then "." else dir
  let out ← IO.Process.output { cmd := "sh", args := #["-c", s!"ls -l {d} | tail -n +2"] }
  pure out.stdout

-- | Entry point
def main (args : List String) : IO Unit := do
  let (path?, keys, testMode) := parseArgs args
  Fzf.setTestMode testMode
  let pipeMode ← (! ·) <$> Term.isattyStdin
  let theme ← Theme.State.init
  -- stdin mode if piped
  if pipeMode && path?.isNone then
    if let some a ← runMem (← MemTable.fromStdin) "stdin" true testMode theme keys then outputTable a
    return
  let path := path?.getD ""
  if path.isEmpty then  -- no file: show current directory
    let _ ← runMem (MemTable.fromText (← lsDir ".")) "." pipeMode testMode theme keys
  else if path.endsWith ".csv" then
    let _ ← runMem (← MemTable.load path) path pipeMode testMode theme keys
  else if path.endsWith ".txt" then
    let _ ← runMem (MemTable.fromText (← IO.FS.readFile path)) path pipeMode testMode theme keys
  else
    let ok ← AdbcTable.init
    if !ok then IO.eprintln "Backend init failed"; return
    try
      match ← AdbcTable.fromFile path with
      | none => AdbcTable.shutdown; IO.eprintln "Query failed"
      | some tbl => match View.fromTbl (.adbc tbl) path with
        | some v => let _ ← runApp v pipeMode testMode theme keys; AdbcTable.shutdown
        | none => AdbcTable.shutdown; IO.eprintln "Empty table"
    catch e => AdbcTable.shutdown; IO.eprintln s!"Query error: {e}"
