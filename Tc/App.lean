/-
  App with ViewStack support
  CSV: pure Lean MemTable, other: ADBC (DuckDB+PRQL)
-/
import Tc.Data.ADBC.Meta
import Tc.Data.Mem.Meta
import Tc.Key
import Tc.Render
import Tc.Term
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
partial def mainLoop (stk : ViewStack) (vs : ViewState) (keys : Array Char) (testMode : Bool := false) (verbPfx : Option Verb := none) : IO Unit := do
  let (vs', v') ← stk.cur.doRender vs  -- v' has updated widths
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
    return
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
    return ← mainLoop stk { vs' with showInfo := !vs'.showInfo } keys' testMode
  -- +/- prefix for hor/ver/prec/width commands
  if ev.type == Term.eventKey && verbPfx.isNone then
    if ev.ch == '+'.toNat.toUInt32 then return ← mainLoop stk vs' keys' testMode (some .inc)
    if ev.ch == '-'.toNat.toUInt32 then return ← mainLoop stk vs' keys' testMode (some .dec)
  -- dispatch Cmd to ViewStack
  match evToCmd ev verbPfx with
  | some cmd => match ← stk.exec cmd rowPg colPg with
    | some stk' =>
      -- reset ViewState when view changes (widths now in View, so ViewState just has scroll/lastCol)
      let reset := cmd matches .stk .dec | .colSel .del | .colSel _ | .info _ | .freq _ | .col .search | .row .search | .col .filter | .row .filter
      mainLoop stk' (if reset then ViewState.default else vs') keys' testMode
    | none => return  -- quit or table empty
  | none => mainLoop stk vs' keys' testMode

-- | Parse args: path, optional -c for key replay (test mode)
def parseArgs (args : List String) : String × Array Char × Bool :=
  match args with
  | [path, "-c", keys] => (path, (parseKeys keys).toList.toArray, true)
  | path :: "-c" :: keys :: _ => (path, (parseKeys keys).toList.toArray, true)
  | path :: _ => (path, #[], false)
  | [] => ("data.csv", #[], false)

-- | Entry point
def main (args : List String) : IO Unit := do
  let (path, keys, testMode) := parseArgs args
  let _ ← Term.init
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .error e => Term.shutdown; IO.eprintln s!"CSV parse error: {e}"
    | .ok tbl => match View.fromTbl (.mem tbl) path with
      | some v => mainLoop ⟨#[v], by simp⟩ ViewState.default keys testMode; Term.shutdown
      | none => Term.shutdown; IO.eprintln "Empty table"
  else
    let ok ← AdbcTable.init
    if !ok then Term.shutdown; IO.eprintln "Backend init failed"; return
    match ← AdbcTable.fromFile path with
    | none => Term.shutdown; AdbcTable.shutdown; IO.eprintln "Query failed"
    | some tbl => match View.fromTbl (.adbc tbl) path with
      | some v => mainLoop ⟨#[v], by simp⟩ ViewState.default keys testMode; Term.shutdown; AdbcTable.shutdown
      | none => Term.shutdown; AdbcTable.shutdown; IO.eprintln "Empty table"
