-- App/Common: app state, dispatch, main loop
import Tc.Filter
import Tc.Folder
import Tc.SourceConfig
import Tc.Meta
import Tc.Plot
import Tc.Transpose
import Tc.Join
import Tc.Key
import Tc.Runner
import Tc.Derive
import Tc.Split
import Tc.Theme
import Tc.UI.Info
import Tc.UI.Preview
import Tc.Data.Text
import Tc.Sparkline
import Tc.Session
import Tc.Diff
import Tc.StatusAgg
import Tc.Export
import Tc.Replay

open Tc

-- | App state: view stack + render state + theme + info + preview scroll
structure AppState where
  stk   : ViewStack AdbcTable
  vs    : ViewState
  theme : Theme.State
  info  : UI.Info.State
  prevScroll : Nat := 0
  heatMode : UInt8 := 0  -- 0=off, 1=numeric, 2=categorical, 3=both
  sparklines : Array String := #[]  -- per-column sparkline cache (empty = recompute)
  statusCache : String × String × String := ("", "", "")  -- (path, col, desc) — avoids per-frame DB query
  aggCache : StatusAgg.Cache := StatusAgg.Cache.empty

-- | Dispatch result: quit, unhandled, or new state
inductive Action where | quit | unhandled | ok (a : AppState)

-- | Handler function type: (state, cmdInfo, arg) → IO Action
-- arg is empty for non-arg commands, contains user input for arg commands.
abbrev HandlerFn := AppState → CmdConfig.CmdInfo → String → IO Action

-- | Unified handler map: handler name → HandlerFn.
-- One HashMap for all commands (pure, IO, arg). Lookup falls back to viewUp.
initialize handlerMap : IO.Ref (Std.HashMap String HandlerFn) ← IO.mkRef {}

namespace AppState

-- | Update stk, reset vs if ci.resetsVS
def withStk (a : AppState) (ci : CmdConfig.CmdInfo) (s' : ViewStack AdbcTable) : AppState :=
  { a with stk := s', vs := if ci.resetsVS then .default else a.vs }

-- | Try/catch wrapper for stack-level IO (resets vs+sparklines on success)
private def tryStk (a : AppState) (ci : CmdConfig.CmdInfo)
    (f : IO (Option (ViewStack AdbcTable))) : IO Action := do
  match ← f.toBaseIO with
  | .ok (some s') =>
    let r := a.withStk ci s'
    pure (.ok { r with vs := .default, sparklines := #[] })
  | .ok none => pure (.ok a)
  | .error e => Log.error e.toString; errorPopup e.toString; pure (.ok a)

-- | Execute a residual Effect from View.update/Freq.update inline
private def runViewEffect (a : AppState) (ci : CmdConfig.CmdInfo)
    (v' : View AdbcTable) (e : Effect) : IO Action := do
  let s := a.stk
  let a' := a.withStk ci (s.setCur v')
  match e with
  | .none => pure (.ok a')
  | .quit => pure .quit
  | .fetchMore =>
    match ← (TblOps.fetchMore s.tbl).toBaseIO with
    | .ok (some tbl') => match v'.rebuild tbl' (row := v'.nav.row.cur.val) with
      | some rv => pure (.ok { a' with stk := s.setCur rv, vs := .default, sparklines := #[] })
      | none => pure (.ok a')
    | .ok none => pure (.ok a')
    | .error err => Log.error err.toString; errorPopup err.toString; pure (.ok a')
  | .sort colIdx sels grp asc => tryStk a ci do
    let tbl' ← ModifyTable.sort s.tbl colIdx sels grp asc
    pure (v'.rebuild tbl' (col := colIdx) (row := v'.nav.row.cur.val) |>.map s.setCur)
  | .exclude cols => tryStk a ci do
    let tbl' ← AdbcTable.excludeCols s.tbl cols
    let grp' := v'.nav.grp.filter (!cols.contains ·)
    let hidden' := v'.nav.hidden.filter (!cols.contains ·)
    pure (v'.rebuild tbl' (grp := grp') (row := v'.nav.row.cur.val) |>.map fun rv =>
      s.setCur { rv with nav := { rv.nav with hidden := hidden' } })
  | .freq colNames => tryStk a ci do
    let some (adbc, totalGroups) ← AdbcTable.freqTable s.tbl colNames | return none
    match View.fromTbl adbc s.cur.path 0 colNames with
    | some fv => pure (some (s.push { fv with vkind := .freqV colNames totalGroups, disp := s!"freq {",".intercalate colNames.toList}" }))
    | none => pure none
  | .freqFilter cols row => tryStk a ci do
    match s.cur.vkind, s.pop with
    | .freqV _ _, some s' => do
      let expr ← Freq.filterExprIO s.tbl cols row
      match ← TblOps.filter s'.tbl expr with
      | some tbl' => match s'.cur.rebuild tbl' (row := 0) with
        | some rv => pure (some (s'.push rv))
        | none => pure none
      | none => pure none
    | _, _ => pure none

-- | View.update + runViewEffect fallback for nav/sort/exclude/freq handlers
private def viewUp (a : AppState) (ci : CmdConfig.CmdInfo) : IO Action := do
  let h := ci.handler
  -- freq handlers: try Freq.update first, then View.update
  if h.startsWith "freq." then
    match Freq.update a.stk h with
    | some (s', e) => return ← runViewEffect (a.withStk ci s') ci s'.cur e
    | none => pure ()
  match View.update a.stk.cur h 20 with
  | some (v', e) => runViewEffect a ci v' e
  | none => pure .unhandled

-- | (handler, value, isAbsolute) — decimal count 0-17
private def precTable : Array (String × Nat × Bool) := #[
  ("precDec", 1, false), ("precInc", 1, false), ("prec0", 0, true), ("precMax", 17, true)]

-- | Shared pure dispatch: scroll, prec, info, heat, nav-only View.update.
-- Both pureDispatch and dispatch delegate here to avoid duplicating logic.
private def sharedPure (a : AppState) (ci : CmdConfig.CmdInfo) : Option AppState :=
  let h := ci.handler
  if h == "scrollUp" then some { a with prevScroll := a.prevScroll - min a.prevScroll 5 }
  else if h == "scrollDn" then some { a with prevScroll := a.prevScroll + 5 }
  else if let some (_, v, abs) := precTable.find? (·.1 == h) then
    let cur := a.stk.cur.prec
    let p := if abs then v else if h == "precDec" then cur - min cur v else min 17 (cur + v)
    some { a with stk := a.stk.setCur { a.stk.cur with prec := p } }
  else if h == "infoTog" then a.info.update h |>.map fun i' => { a with info := i' }
  else if h.startsWith "heat." then
    some { a with heatMode := min 3 (h.back.toNat - '0'.toNat).toUInt8 }
  else
    -- Nav-only: try View.update, keep only .none effects
    View.update a.stk.cur h 20 |>.bind fun (v', e) =>
      if e.isNone then some (a.withStk ci (a.stk.setCur v')) else none

-- | Pure-only dispatch for preview polling (fzf cmd mode). No IO effects executed.
def pureDispatch (a : AppState) (ci : CmdConfig.CmdInfo) : Option AppState :=
  let h := ci.handler
  if h.startsWith "stk." then
    ViewStack.update a.stk h |>.bind fun (s', _) => some (a.withStk ci s')
  else sharedPure a ci

-- | Full dispatch: unified HashMap lookup with viewUp fallback.
-- Returns .quit to exit, .unhandled if command not recognized, .ok for everything else.
partial def dispatch (a : AppState) (ci : CmdConfig.CmdInfo) (arg : String := "") : IO Action := do
  match (← handlerMap.get).get? ci.handler with
  | some f => f a ci arg
  | none => viewUp a ci  -- default: View.update + runViewEffect

-- | fzf command menu with live socket polling for preview
private partial def runMenu (a : AppState) : IO Action := do
  let ref ← IO.mkRef a
  let poll : IO Unit := do
    match ← Socket.pollCmd with
    | some cmdStr =>
      Log.write "sock" s!"poll cmd={cmdStr}"
      let ci ← CmdConfig.handlerLookup cmdStr
      match (← ref.get).pureDispatch ci with
      | some a' =>
        let (vs', v') ← a'.stk.cur.doRender a'.vs a'.theme.styles a'.heatMode a'.sparklines
        ref.set { a' with stk := a'.stk.setCur v', vs := vs' }
        Term.present
      | none => pure ()
    | none => pure ()
  let handler? ← Fzf.cmdMode a.stk.cur.vkind poll
  let a' ← ref.get
  let _ ← Socket.pollCmd  -- drain stale preview command from fzf focus
  match handler? with
  | some h =>
    let ci ← CmdConfig.handlerLookup h
    a'.dispatch ci
  | none => pure (.ok a')

-- | Run a stack-level IO action with shared error handling and state reset
private def runStackIO (a : AppState) (f : IO (ViewStack AdbcTable)) : IO Action := do
  match ← f.toBaseIO with
  | .ok s' => pure (.ok { a with stk := s', vs := .default, sparklines := #[] })
  | .error e => Log.error e.toString; errorPopup e.toString; pure (.ok a)

end AppState

-- | Register all handlers into the unified handlerMap.
-- Groups: pure state, stack ops, IO domain, prefix domain, arg commands.
def initHandlers : IO Unit := do
  let mut m : Std.HashMap String HandlerFn := {}
  -- quit
  m := m.insert "quit" fun _ _ _ => pure .quit
  -- pure state: scroll, prec, heat, info
  for h in #["scrollUp", "scrollDn", "precDec", "precInc", "prec0", "precMax", "infoTog",
             "heat.0", "heat.1", "heat.2", "heat.3"] do
    m := m.insert h fun a ci _ =>
      match AppState.sharedPure a ci with
      | some a' => pure (.ok a')
      | none => pure .unhandled
  -- stack ops (may produce .quit on empty stack pop)
  for h in #["stk.pop", "stk.swap", "stk.dup"] do
    m := m.insert h fun a ci _ =>
      pure (match ViewStack.update a.stk h with
        | some (_, .quit) => .quit
        | some (s', _) => .ok (a.withStk ci s')
        | none => .unhandled)
  -- menu
  m := m.insert "menu" fun a _ _ => AppState.runMenu a
  -- transpose, diff
  m := m.insert "xpose" fun a ci _ => a.tryStk ci (Transpose.push a.stk)
  m := m.insert "diff" fun a ci _ =>
    if a.stk.cur.sameHide.isEmpty then a.tryStk ci (Diff.run a.stk)
    else pure (.ok { a with stk := a.stk.setCur (Diff.showSame a.stk.cur), vs := .default, sparklines := #[] })
  -- folder handlers (domain dispatch with viewUp fallback)
  for h in #["folder.push", "folder.enter", "folder.parent", "folder.del",
             "folder.depthDec", "folder.depthInc"] do
    m := m.insert h fun a ci _ => do
      if let some f := Folder.dispatch a.stk h then return ← a.tryStk ci f
      a.viewUp ci
  -- meta handlers (domain dispatch with viewUp fallback)
  for h in #["meta.push", "meta.selNull", "meta.selSingle", "meta.setKey"] do
    m := m.insert h fun a ci _ => do
      if let some f := Meta.dispatch a.stk h then return ← a.tryStk ci f
      a.viewUp ci
  -- plot handlers
  for h in #["plot.area", "plot.line", "plot.scatter", "plot.bar", "plot.box",
             "plot.step", "plot.hist", "plot.density", "plot.violin"] do
    m := m.insert h fun a ci _ => do
      if let some kind := Plot.kindOf? h then return ← a.tryStk ci (Plot.run a.stk kind)
      pure .unhandled
  -- filter handlers (non-arg: searchNext/searchPrev)
  for h in #["filter.searchNext", "filter.searchPrev"] do
    m := m.insert h fun a ci _ => do
      if let some f := Filter.dispatch a.stk h then return ← a.tryStk ci (some <$> f)
      a.viewUp ci
  -- freq handlers (dispatch via viewUp which tries Freq.update first)
  for h in #["freq.open", "freq.filter"] do
    m := m.insert h fun a ci _ => a.viewUp ci
  -- arg commands: handlers that use the arg parameter
  m := m.insert "split" fun a _ arg => a.runStackIO (if arg.isEmpty then Split.run a.stk else Split.runWith a.stk arg)
  m := m.insert "derive" fun a _ arg => a.runStackIO (if arg.isEmpty then Derive.run a.stk else Derive.runWith a.stk arg)
  m := m.insert "filter.rowFilter" fun a _ arg => a.runStackIO (if arg.isEmpty then ViewStack.rowFilter a.stk else ViewStack.filterWith a.stk arg)
  m := m.insert "filter.rowSearch" fun a _ arg => a.runStackIO (if arg.isEmpty then ViewStack.rowSearch a.stk else ViewStack.searchWith a.stk arg)
  m := m.insert "filter.colSearch" fun a _ arg => a.runStackIO (if arg.isEmpty then ViewStack.colSearch a.stk else ViewStack.colJumpWith a.stk arg)
  m := m.insert "export" fun a _ arg => a.runStackIO (if arg.isEmpty then do
      match ← Export.pickFmt with | some f => Export.run a.stk f | none => pure a.stk
    else Export.runWith a.stk arg)
  m := m.insert "sessSave" fun a _ arg => a.runStackIO (do Session.saveWith a.stk arg; pure a.stk)
  m := m.insert "sessLoad" fun a _ arg => a.runStackIO (do
    match ← Session.loadWith arg with | some stk' => pure stk' | none => pure a.stk)
  m := m.insert "join" fun a _ arg => a.runStackIO (do
    match ← Join.runWith a.stk arg with | some s' => pure s' | none => pure a.stk)
  handlerMap.set m

-- | Convert ViewKind to context string for config lookup
def viewCtxStr : ViewKind → String
  | .freqV _ _ => "freqV" | .colMeta => "colMeta" | .fld _ _ => "fld" | .tbl => "tbl"

-- | Dispatch a handler name string from socket (handler name, optionally with arg after space)
private partial def dispatchHandler (a : AppState) (cmdStr : String) : IO AppState := do
  Log.write "sock" s!"cmd={cmdStr}"
  -- Handler names may include arg after space: "filter.rowFilter Bid > 100"
  let (h, arg) := match cmdStr.splitOn " " with
    | [h] => (h, "")
    | h :: rest => (h, " ".intercalate rest)
    | [] => (cmdStr, "")
  let ci ← CmdConfig.handlerLookup h
  match ← a.dispatch ci arg with
  | .ok a' => pure { a' with prevScroll := 0 }
  | _ => pure a

-- main loop: render → input → dispatch → loop
partial def mainLoop (a : AppState) (test : Bool) (ks : Array Char) : IO AppState := do
  -- Lazy sparkline computation: recompute when cache is empty
  let a ← if a.sparklines.isEmpty then
    pure { a with sparklines := ← Sparkline.compute a.stk.tbl }
  else pure a
  let (vs', v') ← a.stk.cur.doRender a.vs a.theme.styles a.heatMode a.sparklines
  let a := { a with stk := a.stk.setCur v', vs := vs' }
  renderTabLine a.stk.tabNames 0 (Replay.opsStr a.stk.cur)
  -- Show column description on status line from DuckDB column comments (cached)
  let colName := a.stk.cur.nav.colNames.getD a.stk.cur.nav.curColIdx ""
  let (cachedPath, cachedCol, _) := a.statusCache
  let a ← if cachedPath == a.stk.cur.path && cachedCol == colName then pure a
    else do
      let desc ← AdbcTable.columnComment a.stk.cur.path colName
      pure { a with statusCache := (a.stk.cur.path, colName, desc) }
  let (_, _, desc) := a.statusCache
  if !desc.isEmpty then
    let ht ← Term.height; let w ← Term.width
    let label := colName ++ ": " ++ desc
    let maxLen := w.toNat * 2 / 3
    let label := if label.length > maxLen then (label.take maxLen).toString ++ "…" else label
    Term.print 0 (ht - 1) Term.cyan Term.default label
  -- Column aggregation stats (sum/avg/count) cached per column
  let aggCache ← StatusAgg.update a.aggCache a.stk.tbl a.stk.cur.path a.stk.cur.nav.curColIdx
  let a := { a with aggCache }
  if a.info.vis then UI.Info.render (← Term.height).toNat (← Term.width).toNat a.stk.cur.vkind
  -- Preview box for truncated cell text (skip in test mode)
  if !test then do
    let h ← Term.height; let w ← Term.width
    let nav := a.stk.cur.nav
    let curCol := nav.curColIdx
    let cellText ← TblOps.cellStr nav.tbl nav.row.cur.val curCol
    -- Show preview if cell text wider than column display width
    let colW := min (a.stk.cur.widths.getD nav.col.cur.val 10) 50
    if cellText.length + 2 > colW then
      UI.Preview.render h.toNat w.toNat cellText a.prevScroll
  Term.present
  if test && ks.isEmpty then IO.print (← Term.bufferStr); return a
  let (ev, ks') ← nextEvent ks
  -- Socket commands from external tools (handler names)
  let a ← match ← Socket.pollCmd with
    | some cmdStr => dispatchHandler a cmdStr
    | none => pure a
  -- \x16 = <wait> test key: sleep to let socket commands arrive
  if ev.type == 1 && ev.key == 0x16 then IO.sleep 50; return ← mainLoop a test ks'
  -- Empty event (socket wake-up with no key press) → re-render and loop
  if ev.type == 0 then return ← mainLoop a test ks'
  -- Resolve key → handler name via config (context-aware)
  let key := evToKey ev
  let vkStr := viewCtxStr a.stk.cur.vkind
  let handler? ← do
    match ← CmdConfig.keyLookup key vkStr with
    | some ci => pure (some ci.handler)
    | none => pure none
  match handler? with
  | some h =>
    -- Arg commands: collect user input first (in test mode, chars until \r), then dispatch
    let (arg, rest) ← if ← CmdConfig.isArgHandler h then
      pure (if test && ks'.any (· == '\r') then
        let idx := ks'.findIdx? (· == '\r') |>.getD ks'.size
        (String.ofList (ks'.extract 0 idx).toList, ks'.extract (idx + 1) ks'.size)
      else ("", ks'))
    else pure ("", ks')
    let ci ← CmdConfig.handlerLookup h
    match ← a.dispatch ci arg with
    | .quit => pure a
    | .unhandled => mainLoop a test rest
    | .ok a'' =>
      let a'' := if h == "scrollUp" || h == "scrollDn" then a'' else { a'' with prevScroll := 0 }
      mainLoop a'' test rest
  | none => mainLoop a test ks'

