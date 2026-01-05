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
import Tc.ViewStack

open Tc

-- | Loop context (immutable per iteration)
structure LoopCtx where
  styles   : Array UInt32
  themeIdx : Nat
  testMode : Bool

-- | Main loop with ViewStack. Returns final ViewStack for pipe mode output
partial def mainLoop (stk : ViewStack) (vs : ViewState) (ctx : LoopCtx) (keys : Array Char) : IO ViewStack := do
  let (vs', v') ← stk.cur.doRender vs ctx.styles  -- v' has updated widths
  let stk := stk.setCur v'
  renderTabLine stk.tabNames 0
  if vs'.showInfo then UI.Info.render (← Term.height).toNat (← Term.width).toNat
  Term.present
  -- test mode: exit after render when keys exhausted
  if ctx.testMode && keys.isEmpty then IO.print (← Term.bufferStr); return stk
  let (ev, keys') ← nextEvent keys
  let rowPg := ((← Term.height).toNat - reservedLines) / 2
  -- Q=quit
  if isKey ev 'Q' then return stk
  -- +/- prefix: fzf menu for object selection
  let cmd? ← if isKey ev '+' then Fzf.prefixCmd .inc
             else if isKey ev '-' then Fzf.prefixCmd .dec
             else pure (evToCmd ev none)
  -- dispatch command
  match cmd? with
  | some (.thm verb) =>
    let (sty, idx) ← Theme.doCycle ctx.themeIdx (if verb == .inc then 1 else -1)
    mainLoop stk vs' { ctx with styles := sty, themeIdx := idx } keys'
  | some (.info v) =>
    let vis := match v with | .inc => true | .dec => false | _ => !vs'.showInfo
    mainLoop stk { vs' with showInfo := vis } ctx keys'
  | some cmd => match ← stk.exec cmd rowPg colPageSize with
    | some stk' =>
      let reset := cmd matches .stk .dec | .colSel .del | .colSel _ | .metaV _ | .freq _ | .col .search | .row .search | .col .filter | .row .filter
      mainLoop stk' (if reset then ViewState.default else vs') ctx keys'
    | none => return stk
  | none => mainLoop stk vs' ctx keys'

-- | Parse args: path, optional -c for key replay (test mode)
def parseArgs (args : List String) : String × Array Char × Bool :=
  match args with
  | [path, "-c", keys] => (path, (parseKeys keys).toList.toArray, true)
  | path :: "-c" :: keys :: _ => (path, (parseKeys keys).toList.toArray, true)
  | ["-c", keys] => ("data.csv", (parseKeys keys).toList.toArray, true)  -- -c without path
  | "-c" :: keys :: _ => ("data.csv", (parseKeys keys).toList.toArray, true)
  | path :: _ => (path, #[], false)
  | [] => ("data.csv", #[], false)

-- | Output table as plain text (for pipe mode)
def outputTable (stk : ViewStack) : IO Unit := do
  match stk.cur.nav.tbl with
  | .mem tbl => IO.println (MemTable.toText tbl)
  | .adbc _ => pure ()  -- ADBC output not yet supported

-- | Run app with view, returns final ViewStack
def runApp (v : View) (pipeMode : Bool) (ctx : LoopCtx) (keys : Array Char) : IO ViewStack := do
  if pipeMode then let _ ← Term.reopenTty
  let _ ← Term.init
  let stk ← mainLoop ⟨#[v], by simp⟩ ViewState.default ctx keys
  Term.shutdown
  pure stk

-- | Run with MemTable result, returns ViewStack if successful
def runMem (res : Except String MemTable) (name : String) (pipeMode : Bool) (ctx : LoopCtx) (keys : Array Char) : IO (Option ViewStack) := do
  match res with
  | .error e => IO.eprintln s!"Parse error: {e}"; pure none
  | .ok tbl => match View.fromTbl (.mem tbl) name with
    | some v => pure (some (← runApp v pipeMode ctx keys))
    | none => IO.eprintln "Empty table"; pure none

-- | Entry point
def main (args : List String) : IO Unit := do
  let (path, keys, testMode) := parseArgs args
  Fzf.setTestMode testMode
  let pipeMode ← (! ·) <$> Term.isattyStdin
  -- detect terminal bg, load theme
  let dark ← Theme.isDark
  let variant := if dark then "dark" else "light"
  let styles ← Theme.load "theme.csv" "default" variant <|> pure Theme.defaultDark
  let ctx : LoopCtx := ⟨styles, Theme.themeIdx "default" variant, testMode⟩
  -- stdin mode if piped and no explicit path
  if pipeMode && path == "data.csv" then
    if let some stk ← runMem (← MemTable.fromStdin) "stdin" true ctx keys then outputTable stk
  else if path.endsWith ".csv" then
    let _ ← runMem (← MemTable.load path) path pipeMode ctx keys
  else if path.endsWith ".txt" then
    let _ ← runMem (MemTable.fromText (← IO.FS.readFile path)) path pipeMode ctx keys
  else
    let ok ← AdbcTable.init
    if !ok then IO.eprintln "Backend init failed"; return
    try
      match ← AdbcTable.fromFile path with
      | none => AdbcTable.shutdown; IO.eprintln "Query failed"
      | some tbl => match View.fromTbl (.adbc tbl) path with
        | some v => let _ ← runApp v pipeMode ctx keys; AdbcTable.shutdown
        | none => AdbcTable.shutdown; IO.eprintln "Empty table"
    catch e => AdbcTable.shutdown; IO.eprintln s!"Query error: {e}"
