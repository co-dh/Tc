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

-- | Reset view state caches (after data changes)
@[inline] def resetVS (a : AppState) : AppState := { a with vs := .default, sparklines := #[] }

-- | Update stk, reset vs if ci.resetsVS
def withStk (a : AppState) (ci : CmdConfig.CmdInfo) (s' : ViewStack AdbcTable) : AppState :=
  { a with stk := s', vs := if ci.resetsVS then .default else a.vs }

-- | Try/catch wrapper for stack-level IO (resets vs+sparklines on success)
private def tryStk (a : AppState) (ci : CmdConfig.CmdInfo)
    (f : IO (Option (ViewStack AdbcTable))) : IO Action := do
  match ← f.toBaseIO with
  | .ok (some s') =>
    pure (.ok (a.withStk ci s').resetVS)
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
      | some rv => pure (.ok { a' with stk := s.setCur rv }.resetVS)
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

-- | Pure-only dispatch for preview polling (fzf cmd mode). No IO effects, no HashMap.
-- Only called from runMenu poll loop — perf not critical.
def pureDispatch (a : AppState) (ci : CmdConfig.CmdInfo) : Option AppState :=
  let h := ci.handler
  if h.startsWith "stk." then
    ViewStack.update a.stk h |>.bind fun (s', _) => some (a.withStk ci s')
  else
    -- Nav-only: try View.update, keep only .none effects
    View.update a.stk.cur h 20 |>.bind fun (v', e) =>
      if e.isNone then some (a.withStk ci (a.stk.setCur v')) else none

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
  | .ok s' => pure (.ok { a with stk := s' }.resetVS)
  | .error e => Log.error e.toString; errorPopup e.toString; pure (.ok a)

end AppState

-- | Handler combinators — build HandlerFn from domain functions
-- set prec to absolute value
private def precSet (v : Nat) : HandlerFn := fun a _ _ =>
  pure (.ok { a with stk := a.stk.setCur { a.stk.cur with prec := v } })
-- adjust prec by delta, clamped to [0,17]
private def precAdj (delta : Int) : HandlerFn := fun a _ _ =>
  let cur := a.stk.cur.prec
  let p := (Int.ofNat cur + delta).toNat |> min 17
  pure (.ok { a with stk := a.stk.setCur { a.stk.cur with prec := p } })
-- domain dispatch with tryStk + viewUp fallback
private def domainH (d : ViewStack AdbcTable → String → Option (IO (Option (ViewStack AdbcTable)))) : HandlerFn :=
  fun a ci _ => do
    if let some f := d a.stk ci.handler then return ← a.tryStk ci f
    a.viewUp ci
-- domain dispatch returning non-optional (Filter style)
private def domainH' (d : ViewStack AdbcTable → String → Option (IO (ViewStack AdbcTable))) : HandlerFn :=
  fun a ci _ => do
    if let some f := d a.stk ci.handler then return ← a.tryStk ci (some <$> f)
    a.viewUp ci
-- stack op
private def stkH : HandlerFn := fun a ci _ =>
  pure (match ViewStack.update a.stk ci.handler with
    | some (_, .quit) => .quit | some (s', _) => .ok (a.withStk ci s') | none => .unhandled)
-- plot: handler name → PlotKind → run
private def plotH : HandlerFn := fun a ci _ => do
  if let some k := Plot.kindOf? ci.handler then return ← a.tryStk ci (Plot.run a.stk k)
  pure .unhandled
-- arg command: fzf version when empty, direct when arg given
private def argH (fzf : ViewStack AdbcTable → IO (ViewStack AdbcTable))
    (direct : ViewStack AdbcTable → String → IO (ViewStack AdbcTable)) : HandlerFn :=
  fun a _ arg => a.runStackIO (if arg.isEmpty then fzf a.stk else direct a.stk arg)

-- | Abbrev for Entry construction
private abbrev E := CmdConfig.Entry
-- | viewUp as HandlerFn (for freq/nav fallthrough)
private def vuH : HandlerFn := fun a ci _ => a.viewUp ci

-- | Single source of truth: command metadata + handler function.
-- none fn = nav/sort handler (falls through to viewUp in dispatch).
-- nav/sort handlers: no explicit fn, falls through to viewUp
private def nav (e : E) : E × Option HandlerFn := (e, none)
-- handlers with explicit fn
private def cmd (e : E) (f : HandlerFn) : E × Option HandlerFn := (e, some f)

private def commands : Array (E × Option HandlerFn) := #[
  -- row navigation                        -- ctx: r=row c=col g=groups s=sels a=arg S=stack
  nav { handler := "row.inc",      ctx := "r",  key := "j" },
  nav { handler := "row.dec",      ctx := "r",  key := "k" },
  nav { handler := "row.pgdn",    ctx := "r",  key := "<pgdn>" },
  nav { handler := "row.pgup",    ctx := "r",  key := "<pgup>" },
  nav { handler := "row.pgdn",    ctx := "r",  key := "<C-d>" },
  nav { handler := "row.pgup",    ctx := "r",  key := "<C-u>" },
  nav { handler := "row.top",     ctx := "r",  key := "<home>" },
  nav { handler := "row.bot",     ctx := "r",  key := "<end>" },
  nav { handler := "row.sel",     ctx := "r",  key := "T", label := "Select/deselect current row" },
  -- row search/filter
  cmd { handler := "row.search",    ctx := "ca", key := "/", label := "Search for value in current column", resetsVS := true }
    (argH ViewStack.rowSearch ViewStack.searchWith),
  cmd { handler := "row.filter",    ctx := "a",  key := "\\", label := "Filter rows by PRQL expression", resetsVS := true }
    (argH ViewStack.rowFilter ViewStack.filterWith),
  cmd { handler := "row.searchNext",ctx := "rc", key := "n", label := "Jump to next search match" } (domainH' Filter.dispatch),
  cmd { handler := "row.searchPrev",ctx := "rc", key := "N", label := "Jump to previous search match" } (domainH' Filter.dispatch),
  -- col navigation
  nav { handler := "col.inc",     ctx := "c",  key := "l" },
  nav { handler := "col.dec",     ctx := "c",  key := "h" },
  nav { handler := "col.first",   ctx := "c" },
  nav { handler := "col.last",    ctx := "c" },
  nav { handler := "col.grp",     ctx := "c",  key := "!", label := "Toggle group on current column" },
  nav { handler := "col.hide",    ctx := "c",  key := "H", label := "Hide/unhide current column" },
  nav { handler := "col.exclude", ctx := "c",  key := "x", label := "Delete column(s) from query", resetsVS := true },
  nav { handler := "col.shiftL",  ctx := "c",  key := "<S-left>", label := "Shift key column left" },
  nav { handler := "col.shiftR",  ctx := "c",  key := "<S-right>", label := "Shift key column right" },
  -- col sort
  nav { handler := "sort.asc",    ctx := "c",  key := "[", label := "Sort ascending", resetsVS := true },
  nav { handler := "sort.desc",   ctx := "c",  key := "]", label := "Sort descending", resetsVS := true },
  -- col arg commands
  cmd { handler := "col.split",   ctx := "ca", key := ":", label := "Split column by delimiter" } (argH Split.run Split.runWith),
  cmd { handler := "col.derive",  ctx := "a",  key := "=", label := "Derive new column (name = expr)" } (argH Derive.run Derive.runWith),
  cmd { handler := "col.search",  ctx := "a",  key := "g", label := "Jump to column by name", resetsVS := true }
    (argH ViewStack.colSearch ViewStack.colJumpWith),
  -- col plot
  cmd { handler := "plot.area",    ctx := "cg", label := "Area (g=x numeric, c=y numeric)" } plotH,
  cmd { handler := "plot.line",    ctx := "cg", label := "Line (g=x numeric, c=y numeric)" } plotH,
  cmd { handler := "plot.scatter", ctx := "cg", label := "Scatter (g=x numeric, c=y numeric)" } plotH,
  cmd { handler := "plot.bar",     ctx := "cg", label := "Bar (g=x categorical, c=y numeric)" } plotH,
  cmd { handler := "plot.box",     ctx := "cg", label := "Boxplot (g=x categorical, c=y numeric)" } plotH,
  cmd { handler := "plot.step",    ctx := "cg", label := "Step (g=x numeric, c=y numeric)" } plotH,
  cmd { handler := "plot.hist",    ctx := "c",  label := "Histogram (c=numeric column)" } plotH,
  cmd { handler := "plot.density", ctx := "c",  label := "Density (c=numeric column)" } plotH,
  cmd { handler := "plot.violin",  ctx := "cg", label := "Violin (g=x categorical, c=y numeric)" } plotH,
  -- stk: view stack operations
  cmd { handler := "tbl.menu",   key := " ", label := "Open command menu" } (fun a _ _ => AppState.runMenu a),
  cmd { handler := "stk.swap",   ctx := "S",  key := "S", label := "Swap top two views" } stkH,
  cmd { handler := "stk.pop",    key := "q", label := "Close current view", resetsVS := true } stkH,
  cmd { handler := "stk.dup",    label := "Duplicate current view" } stkH,
  cmd { handler := "tbl.quit" } (fun _ _ _ => pure .quit),
  cmd { handler := "tbl.xpose",  key := "X", label := "Transpose table (rows <-> columns)" }
    (fun a ci _ => a.tryStk ci (Transpose.push a.stk)),
  cmd { handler := "tbl.diff",   ctx := "S",  key := "d", label := "Diff top two views" }
    (fun a ci _ => if a.stk.cur.sameHide.isEmpty then a.tryStk ci (Diff.run a.stk)
    else pure (.ok { a with stk := a.stk.setCur (Diff.showSame a.stk.cur) }.resetVS)),
  -- info: precision, heatmap, scroll
  cmd { handler := "info.tog",   key := "I", label := "Toggle info overlay" }
    (fun a ci _ => pure (match a.info.update ci.handler with | some i' => .ok { a with info := i' } | none => .unhandled)),
  cmd { handler := "prec.dec",   label := "Decrease decimal precision" } (precAdj (-1)),
  cmd { handler := "prec.inc",   label := "Increase decimal precision" } (precAdj 1),
  cmd { handler := "prec.zero",  label := "Set precision to 0 decimals" } (precSet 0),
  cmd { handler := "prec.max",   label := "Set precision to max (17)" } (precSet 17),
  cmd { handler := "cell.up",    key := "{", label := "Scroll cell preview up" }
    (fun a _ _ => pure (.ok { a with prevScroll := a.prevScroll - min a.prevScroll 5 })),
  cmd { handler := "cell.dn",    key := "}", label := "Scroll cell preview down" }
    (fun a _ _ => pure (.ok { a with prevScroll := a.prevScroll + 5 })),
  cmd { handler := "heat.0",     label := "Heatmap: off" } (fun a _ _ => pure (.ok { a with heatMode := 0 })),
  cmd { handler := "heat.1",     label := "Heatmap: numeric columns" } (fun a _ _ => pure (.ok { a with heatMode := 1 })),
  cmd { handler := "heat.2",     label := "Heatmap: categorical columns" } (fun a _ _ => pure (.ok { a with heatMode := 2 })),
  cmd { handler := "heat.3",     label := "Heatmap: all columns" } (fun a _ _ => pure (.ok { a with heatMode := 3 })),
  -- metaV: column metadata view
  cmd { handler := "meta.push",      key := "M", label := "Open column metadata view", resetsVS := true } (domainH Meta.dispatch),
  cmd { handler := "meta.setKey",    ctx := "s",  key := "<ret>", label := "Set selected rows as key columns", resetsVS := true, viewCtx := "colMeta" } (domainH Meta.dispatch),
  cmd { handler := "meta.selNull",   key := "0", label := "Select columns with null values", resetsVS := true } (domainH Meta.dispatch),
  cmd { handler := "meta.selSingle", key := "1", label := "Select columns with single value", resetsVS := true } (domainH Meta.dispatch),
  -- freq: frequency table
  cmd { handler := "freq.open",   ctx := "cg", key := "F", label := "Open frequency view", resetsVS := true } vuH,
  cmd { handler := "freq.filter", ctx := "r",  key := "<ret>", label := "Filter parent table by current row", resetsVS := true, viewCtx := "freqV" } vuH,
  -- fld: folder/file browser
  cmd { handler := "folder.push",     ctx := "r", key := "D", label := "Browse folder", resetsVS := true } (domainH Folder.dispatch),
  cmd { handler := "folder.enter",    ctx := "r", key := "<ret>", label := "Open file or enter directory", resetsVS := true, viewCtx := "fld" } (domainH Folder.dispatch),
  cmd { handler := "folder.parent",   key := "<bs>", label := "Go to parent directory", resetsVS := true, viewCtx := "fld" } (domainH Folder.dispatch),
  cmd { handler := "folder.del",      ctx := "r", label := "Move to trash", resetsVS := true } (domainH Folder.dispatch),
  cmd { handler := "folder.depthDec", label := "Decrease folder depth", resetsVS := true } (domainH Folder.dispatch),
  cmd { handler := "folder.depthInc", label := "Increase folder depth", resetsVS := true } (domainH Folder.dispatch),
  -- arg-only commands
  cmd { handler := "tbl.export", ctx := "a",  key := "e", label := "Export table (csv/parquet/json/ndjson)" }
    (fun a _ arg => a.runStackIO (if arg.isEmpty then do
      match ← Export.pickFmt with | some f => Export.run a.stk f | none => pure a.stk
      else Export.runWith a.stk arg)),
  cmd { handler := "sess.save",  ctx := "a",  key := "W", label := "Save session" }
    (fun a _ arg => a.runStackIO (do Session.saveWith a.stk arg; pure a.stk)),
  cmd { handler := "sess.load",  ctx := "a",  label := "Load session" }
    (fun a _ arg => a.runStackIO (do
      match ← Session.loadWith arg with | some stk' => pure stk' | none => pure a.stk)),
  cmd { handler := "tbl.join",   ctx := "Sa", key := "J", label := "Join tables" }
    (fun a _ arg => a.runStackIO (do
      match ← Join.runWith a.stk arg with | some s' => pure s' | none => pure a.stk))
]

def initHandlers : IO Unit := do
  CmdConfig.init (commands.map (·.1))
  let mut m : Std.HashMap String HandlerFn := {}
  for (e, fn?) in commands do
    if let some f := fn? then m := m.insert e.handler f
  handlerMap.set m

-- | Alias for ViewKind.ctxStr (used in dispatch)
def viewCtxStr := ViewKind.ctxStr

-- | Dispatch a handler name string from socket (handler name, optionally with arg after space)
private partial def dispatchHandler (a : AppState) (cmdStr : String) : IO AppState := do
  Log.write "sock" s!"cmd={cmdStr}"
  -- Handler names may include arg after space: "row.filter Bid > 100"
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
  let colName := a.stk.cur.nav.curColName
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
      let a'' := if h == "cell.up" || h == "cell.dn" then a'' else { a'' with prevScroll := 0 }
      mainLoop a'' test rest
  | none => mainLoop a test ks'

