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
import Tc.ViewStack

open Tc

-- | Parse key notation: <ret> → \r, <C-d> → Ctrl-D, etc.
def parseKeys (s : String) : String :=
  s.replace "<ret>" "\r"
   |>.replace "<esc>" "\x1b"
   |>.replace "<C-d>" "\x04"
   |>.replace "<C-u>" "\x15"
   |>.replace "<backslash>" "\\"
   |>.replace "<key>" "!"

-- | Convert char to synthetic Term.Event (matches termbox behavior)
def charToEvent (c : Char) : Term.Event :=
  let ch := c.toNat.toUInt32
  -- Ctrl chars: termbox reports key=ctrl_code, ch=0, mod=2
  if ch < 32 then ⟨Term.eventKey, 2, ch.toUInt16, 0, 0, 0⟩
  else ⟨Term.eventKey, 0, 0, ch, 0, 0⟩

-- | Main loop with ViewStack, keys = remaining replay keys, testMode = exit when keys exhausted
-- Returns final ViewStack for pipe mode output
partial def mainLoop (stk : ViewStack) (vs : ViewState) (styles : Array UInt32) (themeIdx : Nat) (keys : Array Char) (testMode : Bool := false) (verbPfx : Option Verb := none) : IO ViewStack := do
  let (vs', v') ← stk.cur.doRender vs styles  -- v' has updated widths
  let stk := stk.setCur v'             -- update stack with new widths
  renderTabLine stk.tabNames 0  -- current is index 0
  -- info overlay (toggle with I)
  if vs'.showInfo then
    let h ← Term.height; let w ← Term.width
    infoOverlay h.toNat w.toNat
  Term.present
  -- test mode: exit after keys consumed (check AFTER render)
  if testMode && keys.isEmpty then
    IO.print (← Term.bufferStr)
    return stk
  -- get event: from keys or poll
  let (ev, keys') ← if h : keys.size > 0 then
    pure (charToEvent keys[0], keys.extract 1 keys.size)
  else
    let e ← Term.pollEvent
    pure (e, #[])
  let h ← Term.height
  let rowPg := (h.toNat - reservedLines) / 2
  let colPg := colPageSize
  -- I key: toggle info overlay
  if ev.type == Term.eventKey && ev.ch == 'I'.toNat.toUInt32 then
    return ← mainLoop stk { vs' with showInfo := !vs'.showInfo } styles themeIdx keys' testMode
  -- +/- prefix: show fzf menu to select object, then dispatch
  if ev.type == Term.eventKey && verbPfx.isNone then
    let verb? := if ev.ch == '+'.toNat.toUInt32 then some Verb.inc
                 else if ev.ch == '-'.toNat.toUInt32 then some Verb.dec
                 else none
    if let some verb := verb? then
      match ← Fzf.prefixCmd verb with
      | some (.thm v) =>
        let delta := if v == .inc then 1 else -1
        let (theme, variant) := Theme.cycleTheme themeIdx delta
        let newIdx := Theme.themeIdx theme variant
        let newStyles ← Theme.load "theme.csv" theme variant <|> pure Theme.defaultDark
        return ← mainLoop stk vs' newStyles newIdx keys' testMode
      | some cmd => match ← stk.exec cmd rowPg colPg with
        | some stk' => return ← mainLoop stk' vs' styles themeIdx keys' testMode
        | none => return stk
      | none => return ← mainLoop stk vs' styles themeIdx keys' testMode
  -- dispatch Cmd to ViewStack
  match evToCmd ev verbPfx with
  | some (.thm verb) =>
    -- theme switch: cycle theme and reload styles
    let delta := if verb == .inc then 1 else -1
    let (theme, variant) := Theme.cycleTheme themeIdx delta
    let newIdx := Theme.themeIdx theme variant
    let newStyles ← Theme.load "theme.csv" theme variant <|> pure Theme.defaultDark
    mainLoop stk vs' newStyles newIdx keys' testMode
  | some cmd => match ← stk.exec cmd rowPg colPg with
    | some stk' =>
      -- reset ViewState when view changes (widths now in View, so ViewState just has scroll/lastCol)
      let reset := cmd matches .stk .dec | .colSel .del | .colSel _ | .metaV _ | .freq _ | .col .search | .row .search | .col .filter | .row .filter
      mainLoop stk' (if reset then ViewState.default else vs') styles themeIdx keys' testMode
    | none => return stk  -- quit or table empty
  | none => mainLoop stk vs' styles themeIdx keys' testMode

-- | Parse args: path, optional -c for key replay (test mode)
def parseArgs (args : List String) : String × Array Char × Bool :=
  match args with
  | [path, "-c", keys] => (path, (parseKeys keys).toList.toArray, true)
  | path :: "-c" :: keys :: _ => (path, (parseKeys keys).toList.toArray, true)
  | path :: _ => (path, #[], false)
  | [] => ("data.csv", #[], false)

-- | Output table as plain text (for pipe mode)
def outputTable (stk : ViewStack) : IO Unit := do
  let v := stk.cur
  match v.nav.tbl with
  | .mem tbl => IO.println (MemTable.toText tbl)
  | .adbc _ => pure ()  -- ADBC output not yet supported

-- | Entry point
def main (args : List String) : IO Unit := do
  let (path, keys, testMode) := parseArgs args
  Fzf.setTestMode testMode
  -- check if stdin is piped (not a tty)
  let pipeMode ← (! ·) <$> Term.isattyStdin
  -- detect terminal bg, load theme (fallback to default dark)
  let dark ← Theme.isDark
  let variant := if dark then "dark" else "light"
  let themeIdx := Theme.themeIdx "default" variant
  let styles ← Theme.load "theme.csv" "default" variant <|> pure Theme.defaultDark
  -- pipe mode: read from stdin, else from file
  if pipeMode then
    match ← MemTable.fromStdin with
    | .error e => IO.eprintln s!"Parse error: {e}"
    | .ok tbl => match View.fromTbl (.mem tbl) "stdin" with
      | some v =>
        let _ ← Term.reopenTty  -- reopen stdin from /dev/tty for termbox
        let _ ← Term.init
        let stk ← mainLoop ⟨#[v], by simp⟩ ViewState.default styles themeIdx keys testMode
        Term.shutdown
        outputTable stk
      | none => IO.eprintln "Empty table"
  else if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .error e => IO.eprintln s!"CSV parse error: {e}"
    | .ok tbl => match View.fromTbl (.mem tbl) path with
      | some v =>
        let _ ← Term.init
        let _ ← mainLoop ⟨#[v], by simp⟩ ViewState.default styles themeIdx keys testMode
        Term.shutdown
      | none => IO.eprintln "Empty table"
  else
    let ok ← AdbcTable.init
    if !ok then IO.eprintln "Backend init failed"; return
    match ← AdbcTable.fromFile path with
    | none => AdbcTable.shutdown; IO.eprintln "Query failed"
    | some tbl => match View.fromTbl (.adbc tbl) path with
      | some v =>
        let _ ← Term.init
        let _ ← mainLoop ⟨#[v], by simp⟩ ViewState.default styles themeIdx keys testMode
        Term.shutdown; AdbcTable.shutdown
      | none => AdbcTable.shutdown; IO.eprintln "Empty table"
