-- App/Common: app state, dispatch, main loop
import Tc.AppF
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
import Tc.Lens

open Tc

-- | App state: view stack + render state + theme + info + preview scroll.
-- `testMode` is the only "runtime config" that varies per invocation and is
-- needed deep in handlers; other former-globals read their env vars directly.
-- `cmdCache` is a process-lifetime immutable cache derived from the `commands`
-- array below; it lives in AppState (rather than a top-level def) because the
-- `commands` array references helpers that also need the cache, and Lean 4
-- can't forward-declare across that boundary. Built once in `main`.
structure AppState where
  stk      : ViewStack AdbcTable
  testMode : Bool                -- true when driven by the `-c "keys"` test harness
  cmdCache : CmdConfig.Cache     -- command dispatch cache (keyInfo/cmdInfo/argCmds/menu)
  vs       : ViewState
  theme    : Theme.State
  info     : UI.Info.State
  prevScroll : Nat := 0
  heatMode : UInt8 := 0  -- 0=off, 1=numeric, 2=categorical, 3=both
  sparklines : Array String := #[]  -- per-column sparkline cache (empty = recompute)
  statusCache : String × String × String := ("", "", "")  -- (path, col, desc) — avoids per-frame DB query
  aggCache : StatusAgg.Cache := StatusAgg.Cache.empty

-- | Field lenses for AppState — auto-generated, non-dependent fields only.
namespace AppState
gen_lenses AppState where stk, vs, theme, info, prevScroll, heatMode, sparklines, aggCache

-- | Composed lens pointing at the currently-focused view.
def curViewL : Lens' AppState (View AdbcTable) := stkL ∘ₗ ViewStack.hdL

-- | Composed lens pointing at the focused view's decimal precision (3 levels deep).
-- Used by precSet/precAdj to avoid triple-nested `{ a with stk := a.stk.setCur { ... } }`.
def curPrecL : Lens' AppState Nat := curViewL ∘ₗ View.precL
end AppState

-- | Dispatch result: quit, unhandled, or new state
inductive Action where | quit | unhandled | ok (a : AppState)

-- | Handler function type: (state, cmdInfo, arg) → IO Action
-- arg is empty for non-arg commands, contains user input for arg commands.
abbrev HandlerFn := AppState → CmdConfig.CmdInfo → String → IO Action

-- | Unified handler map: Cmd → HandlerFn. Set once by `initHandlers` at
-- boot, never mutated after. Lookup falls back to `viewUp`.
--
-- Why an `IO.Ref` instead of a pure top-level def? Ordering cycle:
--   `dispatch` (below)          needs the lookup
--   `runMenu`  (below)          calls `dispatch`
--   `commands` (further below)  captures `runMenu` in its closures
--   lookup table                is built from `commands`
-- A pure `def handlerTable := commands.foldl …` would forward-reference
-- `commands`, and wrapping the whole chain in a `mutual` block mixes
-- `partial def` with non-partial `def` in awkward ways. The IORef acts as
-- a one-shot forward declaration: `dispatch` reads it by name, `commands`
-- is defined after, and `initHandlers` ties the knot at startup.
initialize handlerMap : IO.Ref (Std.HashMap Cmd HandlerFn) ← IO.mkRef {}

namespace AppState

-- | Reset view state caches (after data changes)
@[inline] def resetVS (a : AppState) : AppState := { a with vs := .default, sparklines := #[] }

-- | Update stk, reset vs if ci.resetsVS
def withStk (a : AppState) (ci : CmdConfig.CmdInfo) (s' : ViewStack AdbcTable) : AppState :=
  { a with stk := s', vs := if ci.resetsVS then .default else a.vs }

-- | Log error, show popup, return unchanged state
private def errAction (a : AppState) (e : IO.Error) : IO Action := do
  let msg := e.toString; Log.error msg; errorPopup msg; pure (.ok a)

-- | Generic try/catch wrapper: run `f`, map success via `ok`, route errors
-- through `errAction` so they surface as a popup instead of crashing the loop.
-- Callers supply the success branch so the same plumbing serves both
-- stack-transforming handlers and arg-running handlers.
private def runTry (a : AppState) (f : IO α) (ok : α → Action) : IO Action := do
  match ← f.toBaseIO with
  | .ok x => pure (ok x)
  | .error e => errAction a e

-- | Try/catch wrapper for stack-level IO (resets vs+sparklines on success).
-- `none` means the handler declined to update (e.g. no-op filter cancel).
private def tryStk (a : AppState) (ci : CmdConfig.CmdInfo)
    (f : IO (Option (ViewStack AdbcTable))) : IO Action :=
  a.runTry f fun
    | some s' => .ok (a.withStk ci s').resetVS
    | none    => .ok a

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
    | .ok (some tbl') => match v'.rebuild tbl' (row := v'.nav.row.cur) with
      | some rv => pure (.ok { a' with stk := s.setCur rv }.resetVS)
      | none => pure (.ok a')
    | .ok none => pure (.ok a')
    | .error err => errAction a' err
  | .sort colIdx sels grp asc => tryStk a ci do
    let tbl' ← ModifyTable.sort s.tbl colIdx sels grp asc
    pure (v'.rebuild tbl' (col := colIdx) (row := v'.nav.row.cur) |>.map s.setCur)
  | .exclude cols => tryStk a ci do
    let tbl' ← AdbcTable.excludeCols s.tbl cols
    let grp' := v'.nav.grp.filter (!cols.contains ·)
    let hidden' := v'.nav.hidden.filter (!cols.contains ·)
    pure (v'.rebuild tbl' (grp := grp') (row := v'.nav.row.cur) |>.map fun rv =>
      (View.navL ∘ₗ NavState.hiddenL).set hidden' rv |> s.setCur)
  | .freq colNames => tryStk a ci do
    let some (adbc, totalGroups) ← AdbcTable.freqTable s.tbl colNames | return none
    let some fv := View.fromTbl adbc s.cur.path 0 colNames | return none
    let vkind := .freqV colNames totalGroups
    pure (some (s.push { fv with vkind, disp := s!"freq {colNames.joinWith ","}" }))
  | .freqFilter cols row => tryStk a ci do
    let .freqV _ _ := s.cur.vkind | return none
    let some s' := s.pop | return none
    let expr ← Freq.filterExprIO s.tbl cols row
    let some tbl' ← TblOps.filter s'.tbl expr | return none
    let some rv := s'.cur.rebuild tbl' (row := 0) | return none
    pure (some (s'.push rv))

-- | View.update + runViewEffect fallback for nav/sort/exclude/freq handlers
private def viewUp (a : AppState) (ci : CmdConfig.CmdInfo) : IO Action := do
  -- freq handlers: try Freq.update first, then View.update
  if ci.cmd matches .freqOpen | .freqFilter then
    match Freq.update a.stk ci.cmd with
    | some (s', e) => return ← runViewEffect (a.withStk ci s') ci s'.cur e
    | none => pure ()
  match View.update a.stk.cur ci.cmd 20 with
  | some (v', e) => runViewEffect a ci v' e
  | none => pure .unhandled

-- | Pure-only dispatch for preview polling (fzf cmd mode). No IO effects, no HashMap.
-- Only called from runMenu poll loop — perf not critical.
def pureDispatch (a : AppState) (ci : CmdConfig.CmdInfo) : Option AppState :=
  if ci.cmd matches .stkDup | .stkPop | .stkSwap then
    ViewStack.update a.stk ci.cmd |>.bind fun (s', _) => some (a.withStk ci s')
  else
    -- Nav-only: try View.update, keep only .none effects
    View.update a.stk.cur ci.cmd 20 |>.bind fun (v', e) =>
      if e.isNone then some (a.withStk ci (a.stk.setCur v')) else none

-- | Full dispatch: unified HashMap lookup with viewUp fallback.
-- Returns .quit to exit, .unhandled if command not recognized, .ok for everything else.
partial def dispatch (a : AppState) (ci : CmdConfig.CmdInfo) (arg : String := "") : IO Action := do
  match (← handlerMap.get).get? ci.cmd with
  | some f => f a ci arg
  | none => viewUp a ci  -- default: View.update + runViewEffect

-- | fzf command menu with live socket polling for preview
private partial def runMenu (a : AppState) : IO Action := do
  let ref ← IO.mkRef a
  let poll : IO Unit := do
    match ← Socket.pollCmd with
    | some cmdStr =>
      Log.write "sock" s!"poll cmd={cmdStr}"
      let some ci ← a.cmdCache.handlerLookup cmdStr | pure ()
      match (← ref.get).pureDispatch ci with
      | some a' =>
        let (vs', v') ← a'.stk.cur.doRender a'.vs a'.theme.styles a'.heatMode a'.sparklines
        ref.set { a' with stk := a'.stk.setCur v', vs := vs' }
        Term.present
      | none => pure ()
    | none => pure ()
  let handler? ← Fzf.cmdMode a.testMode a.cmdCache a.stk.cur.vkind poll
  let a' ← ref.get
  let _ ← Socket.pollCmd  -- drain stale preview command from fzf focus
  match handler? with
  | some h =>
    match ← a.cmdCache.handlerLookup h with
    | some ci => a'.dispatch ci
    | none => pure (.ok a')
  | none => pure (.ok a')

-- | Stack-level variant for arg handlers: no `CmdInfo`, so `resetsVS` is
-- applied unconditionally rather than conditionally via `withStk`.
private def runStackIO (a : AppState) (f : IO (ViewStack AdbcTable)) : IO Action :=
  a.runTry f fun s' => .ok { a with stk := s' }.resetVS

end AppState

-- | Handler combinators — build HandlerFn from domain functions
-- set prec to absolute value
private def precSet (v : Nat) : HandlerFn := fun a _ _ =>
  pure (AppState.curPrecL.set v a |> .ok)
-- adjust prec by delta, clamped to [0,17]
private def precAdj (delta : Int) : HandlerFn := fun a _ _ =>
  pure (AppState.curPrecL.modify (fun p => (Int.ofNat p + delta).toNat |> min 17) a |> .ok)
-- domain dispatch with tryStk + viewUp fallback (dispatcher takes testMode)
private def domainH (d : Bool → ViewStack AdbcTable → Cmd → Option (IO (Option (ViewStack AdbcTable)))) : HandlerFn :=
  fun a ci _ => do
    if let some f := d a.testMode a.stk ci.cmd then return ← a.tryStk ci f
    a.viewUp ci
-- domain dispatch returning non-optional (Filter style, dispatcher takes testMode)
private def domainH' (d : Bool → ViewStack AdbcTable → Cmd → Option (IO (ViewStack AdbcTable))) : HandlerFn :=
  fun a ci _ => do
    if let some f := d a.testMode a.stk ci.cmd then return ← a.tryStk ci (some <$> f)
    a.viewUp ci
-- stack op
private def stkH : HandlerFn := fun a ci _ =>
  pure (match ViewStack.update a.stk ci.cmd with
    | some (_, .quit) => .quit | some (s', _) => .ok (a.withStk ci s') | none => .unhandled)
-- plot: Cmd → PlotKind → run
private def plotH : HandlerFn := fun a ci _ => do
  if let some k := ci.cmd.plotKind? then return ← a.tryStk ci (Plot.run a.stk k)
  pure .unhandled
-- arg command: fzf version when empty, direct when arg given.
-- fzf variant takes testMode so it can be stubbed in -c test mode.
private def argH (fzf : Bool → ViewStack AdbcTable → IO (ViewStack AdbcTable))
    (direct : ViewStack AdbcTable → String → IO (ViewStack AdbcTable)) : HandlerFn :=
  fun a _ arg => a.runStackIO (if arg.isEmpty then fzf a.testMode a.stk else direct a.stk arg)

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
  nav { cmd := .rowInc,      ctx := "r",  key := "j" },
  nav { cmd := .rowDec,      ctx := "r",  key := "k" },
  nav { cmd := .rowPgdn,    ctx := "r",  key := "<pgdn>" },
  nav { cmd := .rowPgup,    ctx := "r",  key := "<pgup>" },
  nav { cmd := .rowPgdn,    ctx := "r",  key := "<C-d>" },
  nav { cmd := .rowPgup,    ctx := "r",  key := "<C-u>" },
  nav { cmd := .rowTop,     ctx := "r",  key := "<home>" },
  nav { cmd := .rowBot,     ctx := "r",  key := "<end>" },
  nav { cmd := .rowSel,     ctx := "r",  key := "T", label := "Select/deselect current row" },
  -- row search/filter
  cmd { cmd := .rowSearch,    ctx := "ca", key := "/", label := "Search for value in current column", resetsVS := true }
    (fun a _ arg => do
      if !arg.isEmpty then return ← a.runStackIO (ViewStack.searchWith a.stk arg)
      let ref ← IO.mkRef a
      let preview (stk' : ViewStack AdbcTable) : IO Unit := do
        let a' ← ref.get
        let (vs', v') ← stk'.cur.doRender a'.vs a'.theme.styles a'.heatMode a'.sparklines
        ref.set { a' with stk := stk'.setCur v', vs := vs' }
        renderTabLine stk'.tabNames 0 (Replay.opsStr stk'.cur)
        if a'.info.vis then UI.Info.render (← Term.height).toNat (← Term.width).toNat stk'.cur.vkind
        Term.present
      let stk' ← ViewStack.rowSearchLive a.testMode a.stk preview
      pure (.ok { (← ref.get) with stk := stk' }.resetVS)),
  cmd { cmd := .rowFilter,    ctx := "a",  key := "\\", label := "Filter rows by PRQL expression", resetsVS := true }
    (argH ViewStack.rowFilter ViewStack.filterWith),
  cmd { cmd := .rowSearchNext, ctx := "rc", key := "n", label := "Jump to next search match" } (domainH' Filter.dispatch),
  cmd { cmd := .rowSearchPrev, ctx := "rc", key := "N", label := "Jump to previous search match" } (domainH' Filter.dispatch),
  -- col navigation
  nav { cmd := .colInc,     ctx := "c",  key := "l" },
  nav { cmd := .colDec,     ctx := "c",  key := "h" },
  nav { cmd := .colFirst,   ctx := "c" },
  nav { cmd := .colLast,    ctx := "c" },
  nav { cmd := .colGrp,     ctx := "c",  key := "!", label := "Toggle group on current column" },
  nav { cmd := .colHide,    ctx := "c",  key := "H", label := "Hide/unhide current column" },
  nav { cmd := .colExclude, ctx := "c",  key := "x", label := "Delete column(s) from query", resetsVS := true },
  nav { cmd := .colShiftL,  ctx := "c",  key := "<S-left>", label := "Shift key column left" },
  nav { cmd := .colShiftR,  ctx := "c",  key := "<S-right>", label := "Shift key column right" },
  -- col sort
  nav { cmd := .sortAsc,    ctx := "c",  key := "[", label := "Sort ascending", resetsVS := true },
  nav { cmd := .sortDesc,   ctx := "c",  key := "]", label := "Sort descending", resetsVS := true },
  -- col arg commands
  cmd { cmd := .colSplit,   ctx := "ca", key := ":", label := "Split column by delimiter" } (argH Split.run Split.runWith),
  cmd { cmd := .colDerive,  ctx := "a",  key := "=", label := "Derive new column (name = expr)" } (argH Derive.run Derive.runWith),
  cmd { cmd := .colSearch,  ctx := "a",  key := "g", label := "Jump to column by name", resetsVS := true }
    (argH ViewStack.colSearch ViewStack.colJumpWith),
  -- col plot
  cmd { cmd := .plotArea,    ctx := "cg", label := "Area (g=x numeric, c=y numeric)" } plotH,
  cmd { cmd := .plotLine,    ctx := "cg", label := "Line (g=x numeric, c=y numeric)" } plotH,
  cmd { cmd := .plotScatter, ctx := "cg", label := "Scatter (g=x numeric, c=y numeric)" } plotH,
  cmd { cmd := .plotBar,     ctx := "cg", label := "Bar (g=x categorical, c=y numeric)" } plotH,
  cmd { cmd := .plotBox,     ctx := "cg", label := "Boxplot (g=x categorical, c=y numeric)" } plotH,
  cmd { cmd := .plotStep,    ctx := "cg", label := "Step (g=x numeric, c=y numeric)" } plotH,
  cmd { cmd := .plotHist,    ctx := "c",  label := "Histogram (c=numeric column)" } plotH,
  cmd { cmd := .plotDensity, ctx := "c",  label := "Density (c=numeric column)" } plotH,
  cmd { cmd := .plotViolin,  ctx := "cg", label := "Violin (g=x categorical, c=y numeric)" } plotH,
  -- stk: view stack operations
  cmd { cmd := .tblMenu,   key := " ", label := "Open command menu" } (fun a _ _ => AppState.runMenu a),
  cmd { cmd := .stkSwap,   ctx := "S",  key := "S", label := "Swap top two views" } stkH,
  cmd { cmd := .stkPop,    key := "q", label := "Close current view", resetsVS := true } stkH,
  cmd { cmd := .stkDup,    label := "Duplicate current view" } stkH,
  cmd { cmd := .tblQuit } (fun _ _ _ => pure .quit),
  cmd { cmd := .tblXpose,  key := "X", label := "Transpose table (rows <-> columns)" }
    (fun a ci _ => a.tryStk ci (Transpose.push a.stk)),
  cmd { cmd := .tblDiff,   ctx := "S",  key := "d", label := "Diff top two views" }
    (fun a ci _ => if a.stk.cur.sameHide.isEmpty then a.tryStk ci (Diff.run a.stk)
    else pure (.ok { a with stk := a.stk.setCur (Diff.showSame a.stk.cur) }.resetVS)),
  -- info: precision, heatmap, scroll
  cmd { cmd := .infoTog,   key := "I", label := "Toggle info overlay" }
    (fun a ci _ => pure (match a.info.update ci.cmd with | some i' => .ok { a with info := i' } | none => .unhandled)),
  cmd { cmd := .precDec,   label := "Decrease decimal precision" } (precAdj (-1)),
  cmd { cmd := .precInc,   label := "Increase decimal precision" } (precAdj 1),
  cmd { cmd := .precZero,  label := "Set precision to 0 decimals" } (precSet 0),
  cmd { cmd := .precMax,   label := "Set precision to max (17)" } (precSet 17),
  cmd { cmd := .cellUp,    key := "{", label := "Scroll cell preview up" }
    (fun a _ _ => pure (.ok { a with prevScroll := a.prevScroll - min a.prevScroll 5 })),
  cmd { cmd := .cellDn,    key := "}", label := "Scroll cell preview down" }
    (fun a _ _ => pure (.ok { a with prevScroll := a.prevScroll + 5 })),
  cmd { cmd := .heat0,     label := "Heatmap: off" } (fun a _ _ => pure (.ok { a with heatMode := 0 })),
  cmd { cmd := .heat1,     label := "Heatmap: numeric columns" } (fun a _ _ => pure (.ok { a with heatMode := 1 })),
  cmd { cmd := .heat2,     label := "Heatmap: categorical columns" } (fun a _ _ => pure (.ok { a with heatMode := 2 })),
  cmd { cmd := .heat3,     label := "Heatmap: all columns" } (fun a _ _ => pure (.ok { a with heatMode := 3 })),
  -- metaV: column metadata view
  cmd { cmd := .metaPush,      key := "M", label := "Open column metadata view", resetsVS := true } (domainH Meta.dispatch),
  cmd { cmd := .metaSetKey,    ctx := "s",  key := "<ret>", label := "Set selected rows as key columns", resetsVS := true, viewCtx := "colMeta" } (domainH Meta.dispatch),
  cmd { cmd := .metaSelNull,   key := "0", label := "Select columns with null values", resetsVS := true } (domainH Meta.dispatch),
  cmd { cmd := .metaSelSingle, key := "1", label := "Select columns with single value", resetsVS := true } (domainH Meta.dispatch),
  -- freq: frequency table
  cmd { cmd := .freqOpen,   ctx := "cg", key := "F", label := "Open frequency view", resetsVS := true } vuH,
  cmd { cmd := .freqFilter, ctx := "r",  key := "<ret>", label := "Filter parent table by current row", resetsVS := true, viewCtx := "freqV" } vuH,
  -- fld: folder/file browser
  cmd { cmd := .folderPush,     ctx := "r", key := "D", label := "Browse folder", resetsVS := true } (domainH Folder.dispatch),
  cmd { cmd := .folderEnter,    ctx := "r", key := "<ret>", label := "Open file or enter directory", resetsVS := true, viewCtx := "fld" } (domainH Folder.dispatch),
  cmd { cmd := .folderParent,   key := "<bs>", label := "Go to parent directory", resetsVS := true, viewCtx := "fld" } (domainH Folder.dispatch),
  cmd { cmd := .folderDel,      ctx := "r", label := "Move to trash", resetsVS := true } (domainH Folder.dispatch),
  cmd { cmd := .folderDepthDec, label := "Decrease folder depth", resetsVS := true } (domainH Folder.dispatch),
  cmd { cmd := .folderDepthInc, label := "Increase folder depth", resetsVS := true } (domainH Folder.dispatch),
  -- arg-only commands
  cmd { cmd := .tblExport, ctx := "a",  key := "e", label := "Export table (csv/parquet/json/ndjson)" }
    (fun a _ arg => a.runStackIO (if arg.isEmpty then do
      match ← Export.pickFmt a.testMode with | some f => Export.run a.stk f | none => pure a.stk
      else Export.runWith a.stk arg)),
  cmd { cmd := .sessSave,  ctx := "a",  key := "W", label := "Save session" }
    (fun a _ arg => a.runStackIO (do Session.saveWith a.stk arg; pure a.stk)),
  cmd { cmd := .sessLoad,  ctx := "a",  label := "Load session" }
    (fun a _ arg => a.runStackIO (do
      match ← Session.loadWith arg with | some stk' => pure stk' | none => pure a.stk)),
  cmd { cmd := .tblJoin,   ctx := "Sa", key := "J", label := "Join tables" }
    (fun a _ arg => a.runStackIO (do
      match ← Join.runWith a.stk arg with | some s' => pure s' | none => pure a.stk)),
  -- theme: fzf picker with live preview via socket
  cmd { cmd := .themeOpen, label := "Pick color theme" }
    (fun a _ _ => do
      let ref ← IO.mkRef a
      let render (styles : Array UInt32) : IO Unit := do
        Theme.stylesRef.set styles
        let a' ← ref.get
        let (vs', v') ← a'.stk.cur.doRender a'.vs styles a'.heatMode a'.sparklines
        ref.set { a' with stk := a'.stk.setCur v', vs := vs' }
        renderTabLine a'.stk.tabNames 0 (Replay.opsStr a'.stk.cur)
        if a'.info.vis then UI.Info.render (← Term.height).toNat (← Term.width).toNat a'.stk.cur.vkind
        Term.present
      match ← Theme.run a.testMode a.theme render with
      | some t => pure (.ok { (← ref.get) with theme := t }.resetVS)
      | none   => do
        Theme.stylesRef.set a.theme.styles
        pure (.ok { (← ref.get) with theme := a.theme }.resetVS)),
  cmd { cmd := .themePreview } (fun a _ _ => pure (.ok a))
]

-- | Build the immutable CmdConfig cache from the command table.
-- Called once by `main` to populate `AppState.cmdCache`.
def cmdCache : CmdConfig.Cache := CmdConfig.build (commands.map (·.1))

def initHandlers : IO Unit := do
  let mut m : Std.HashMap Cmd HandlerFn := {}
  for (e, fn?) in commands do
    if let some f := fn? then m := m.insert e.cmd f
  handlerMap.set m
  Log.write "init" s!"commands: {commands.size} entries"

-- | Alias for ViewKind.ctxStr (used in dispatch)
def viewCtxStr := ViewKind.ctxStr

-- | Dispatch a handler name string from socket (handler name, optionally with arg after space)
private partial def dispatchHandler (a : AppState) (cmdStr : String) : IO AppState := do
  Log.write "sock" s!"cmd={cmdStr}"
  -- Handler names may include arg after space: "row.filter Bid > 100"
  let (h, arg) := match cmdStr.splitOn " " with
    | [h] => (h, "")
    | h :: rest => (h, rest |> " ".intercalate)
    | [] => (cmdStr, "")
  let some ci ← a.cmdCache.handlerLookup h | pure a
  match ← a.dispatch ci arg with
  | .ok a' => pure { a' with prevScroll := 0 }
  | _ => pure a

-- Status line + per-frame caches; common to both interpreters.
private def renderBase (a : AppState) : IO AppState := do
  -- Lazy sparkline computation: recompute when cache is empty
  let a ← if a.sparklines.isEmpty then
    pure { a with sparklines := ← Sparkline.compute a.stk.tbl }
  else pure a
  let (vs', v') ← a.stk.cur.doRender a.vs a.theme.styles a.heatMode a.sparklines
  let a := { a with stk := a.stk.setCur v', vs := vs' }
  renderTabLine a.stk.tabNames 0 (Replay.opsStr a.stk.cur)
  -- Column description on status line from DuckDB column comments (cached by path+col)
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
    Term.print 0 (ht - 1) (Theme.styleFg a.theme.styles Theme.sStatus) (Theme.styleBg a.theme.styles Theme.sStatus) label
  let aggCache ← StatusAgg.update a.aggCache a.stk.tbl a.stk.cur.path a.stk.cur.nav.curColIdx
  let a := { a with aggCache }
  if a.info.vis then UI.Info.render (← Term.height).toNat (← Term.width).toNat a.stk.cur.vkind
  pure a

-- Full frame render. `showPreview` toggles the truncated-cell overlay; prod
-- shows it, test skips it (tests don't exercise the preview box).
private def renderFrame (showPreview : Bool) (a : AppState) : IO AppState := do
  let a ← renderBase a
  if showPreview then
    let h ← Term.height; let w ← Term.width
    let nav := a.stk.cur.nav
    let cellText ← TblOps.cellStr nav.tbl nav.row.cur nav.curColIdx
    let colW := min (a.stk.cur.widths.getD nav.col.cur 10) 50
    if cellText.length + 2 > colW then
      UI.Preview.render h.toNat w.toNat cellText a.prevScroll
  Term.present
  pure a

-- Loop program expressed over AppM. Free of any test/prod conditionals —
-- the interpreter decides render flavor, key source, and arg collection.
partial def loopProg (a : AppState) : AppM AppState AppState := do
  let a ← AppM.doRender a
  match ← AppM.poll with
  | none => pure a
  | some key =>
    -- Socket commands from external tools (handler names)
    let a ← match ← Socket.pollCmd with
      | some cmdStr => dispatchHandler a cmdStr
      | none => pure a
    -- <wait>: sleep so external socat → socket → pollCmd can land mid-run.
    -- Only used by socket-dispatch tests that race an out-of-process sender
    -- against the -c keystroke stream; normal synchronous tests don't need it.
    if key == "<wait>" then do IO.sleep 50; loopProg a
    else if key.isEmpty then loopProg a
    else
      let vkStr := viewCtxStr a.stk.cur.vkind
      match a.cmdCache.keyLookup key vkStr with
      | some ci =>
        let arg ← if a.cmdCache.isArgCmd ci.cmd then AppM.readArg' else pure ""
        match ← a.dispatch ci arg with
        | .quit => pure a
        | .unhandled => loopProg a
        | .ok a'' =>
          -- Reset cell-preview scroll offset after every command except the
          -- two that explicitly scroll the preview ({, }): moving to a new
          -- cell should start the overlay at the top, but actively scrolling
          -- within it must not reset mid-gesture. Handled here rather than
          -- in dispatch so the generic dispatcher stays agnostic of UI state.
          let a'' := if ci.cmd matches .cellUp | .cellDn then a'' else { a'' with prevScroll := 0 }
          loopProg a''
      | none => loopProg a

-- Production interpreter: real Term.pollEvent; arg commands open fzf from inside their handlers.
private def prodInterp : AppM.Interp AppState where
  render  := renderFrame (showPreview := true)
  nextKey := do let e ← Term.pollEvent; pure (some (evToKey e))
  readArg := pure ""

-- Test interpreter. Used by the `-c "keys"` test harness: tests spawn `tv`,
-- feed it a canned keystroke string, then read the rendered terminal buffer
-- from stdout to assert on what the UI would have shown.
--
-- The interpreter lives in an `IO.Ref (Array String)` — a mutable reference
-- holding the unconsumed suffix of the keystroke queue. Each call to
-- `nextKey`/`readArg` reads the current queue, decides what to return, and
-- writes the remaining tokens back. This is the only piece of test state;
-- rendering is real (writes to the termbox fake-buffer) and command handlers
-- run in real IO (real DuckDB queries, real file ops, etc.). The test/prod
-- difference is *just* where keystrokes come from and what happens when the
-- queue runs out — everything else is identical to production.
private def testInterp (ref : IO.Ref (Array String)) : AppM.Interp AppState where
  -- Tests don't exercise the truncated-cell preview overlay → skip it to
  -- keep the asserted buffer predictable across terminal sizes.
  render  := renderFrame (showPreview := false)
  -- Poll for the next keystroke. In production this blocks on the terminal;
  -- here we pop from the queue instead. Empty queue means "the test is
  -- finished": print the current termbox buffer so the parent `-c` process
  -- can capture it from stdout, then return `none` which tells `loopProg`
  -- to exit cleanly (see the `| none => pure a` branch above).
  nextKey := do
    let ks ← ref.get
    if ks.isEmpty then
      IO.print (← Term.bufferStr)
      pure none
    else
      -- `Key.nextKey` pops the first token; ks' is the rest of the queue.
      let (key, ks') ← nextKey ks
      ref.set ks'
      pure (some key)
  -- Arg commands (`/`, `\`, `=`, etc.) prompt the user for a string. In
  -- production the handler opens an fzf popup; in tests we splice the arg
  -- directly into the keystroke stream terminated by a `<ret>` token, e.g.
  -- `"/foo<ret>"` means "press `/`, then type `foo`, then Enter". Here we
  -- scan forward to the next `<ret>`, concatenate the tokens before it into
  -- the arg string, and drop everything up to and including the `<ret>` so
  -- the outer loop picks up after the Enter. If there is no `<ret>` in the
  -- queue we return "" (same as prod when the user cancels fzf) so the
  -- handler takes its "empty arg" branch.
  readArg := do
    let ks ← ref.get
    if ks.any (· == "<ret>") then
      let idx := ks.findIdx? (· == "<ret>") |>.getD ks.size
      ref.set (ks.extract (idx + 1) ks.size)
      pure ((ks.extract 0 idx).joinWith "")
    else
      pure ""

-- Main loop entry point: picks the interpreter and runs the free-monad program.
def mainLoop (a : AppState) (test : Bool) (ks : Array String) : IO AppState := do
  if test then
    let ref ← IO.mkRef ks
    (loopProg a).run (testInterp ref)
  else
    (loopProg a).run prodInterp

