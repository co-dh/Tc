-- App: app state, dispatch, main loop, entry point
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

-- | Handlers that take user input (fzf or typed arg).
-- Derived from config: handlers that appear in runArgCmd dispatch.
private def argHandlers : Array String :=
  #["split", "derive", "filter.rowSearch", "filter.rowFilter",
    "filter.colSearch", "export", "sessSave", "sessLoad", "join"]
private def isArgHandler (h : String) : Bool := argHandlers.contains h

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

-- | Shared pure dispatch: scroll, prec, info, heat, nav-only View.update.
-- Both pureDispatch and dispatch delegate here to avoid duplicating logic.
private def sharedPure (a : AppState) (ci : CmdConfig.CmdInfo) : Option AppState :=
  let h := ci.handler
  if h == "scrollUp" then some { a with prevScroll := a.prevScroll - min a.prevScroll 5 }
  else if h == "scrollDn" then some { a with prevScroll := a.prevScroll + 5 }
  else if h == "precDec" then some { a with stk := a.stk.setCur { a.stk.cur with precAdj := a.stk.cur.precAdj - 1 } }
  else if h == "precInc" then some { a with stk := a.stk.setCur { a.stk.cur with precAdj := a.stk.cur.precAdj + 1 } }
  else if h == "prec0" then some { a with stk := a.stk.setCur { a.stk.cur with precAdj := -4 } }
  else if h == "precMax" then some { a with stk := a.stk.setCur { a.stk.cur with precAdj := 13 } }
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

-- | Full dispatch: handles all commands, executes IO inline with error handling.
-- Returns .quit to exit, .unhandled if command not recognized, .ok for everything else.
partial def dispatch (a : AppState) (ci : CmdConfig.CmdInfo) : IO Action := do
  let h := ci.handler
  -- quit
  if h == "quit" then return .quit
  -- stk operations (may produce .quit on empty stack pop)
  if h.startsWith "stk." then
    return match ViewStack.update a.stk h with
    | some (_, .quit) => .quit
    | some (s', _) => .ok (a.withStk ci s')
    | none => .unhandled
  -- pure state updates (shared with pureDispatch)
  if let some a' := sharedPure a ci then return .ok a'
  -- menu (fzf command picker with live polling)
  if h == "menu" then return ← runMenu a
  -- IO domain dispatch: transpose, diff
  if h == "xpose" then return ← tryStk a ci (Transpose.push a.stk)
  if h == "diff" then
    if a.stk.cur.sameHide.isEmpty then return ← tryStk a ci (Diff.run a.stk)
    else return .ok { a with stk := a.stk.setCur (Diff.showSame a.stk.cur), vs := .default, sparklines := #[] }
  -- IO domain dispatch by prefix: folder, meta, plot, filter
  if h.startsWith "folder." then
    if let some f := Folder.dispatch a.stk h then return ← tryStk a ci f
    -- fallthrough to viewUp below
  else if h.startsWith "meta." then
    if let some f := Meta.dispatch a.stk h then return ← tryStk a ci f
  else if h.startsWith "plot." then
    if let some kind := Plot.kindOf? h then
      return ← tryStk a ci (Plot.run a.stk kind)
  else if h.startsWith "filter." then
    if let some f := Filter.dispatch a.stk h then
      return ← tryStk a ci (some <$> f)
  else if h.startsWith "freq." then pure ()  -- handled via Freq.update + viewUp below
  -- View.update: nav, sort, exclude, fetchMore + freq
  let viewUp := fun () => do
    match View.update a.stk.cur h 20 with
    | some (v', e) => runViewEffect a ci v' e
    | none => pure .unhandled
  if h.startsWith "freq." then
    match Freq.update a.stk h with
    | some (s', e) =>
      -- Execute freq effect inline
      return ← runViewEffect (a.withStk ci s') ci s'.cur e
    | none => return ← viewUp ()
  if h.startsWith "folder." || h.startsWith "meta." then
    -- Fallthrough from folder/meta when domain dispatch returned none
    return ← viewUp ()
  viewUp ()
where
  -- | fzf command menu with live socket polling for preview
  runMenu (a : AppState) : IO Action := do
    let ref ← IO.mkRef a
    let poll : IO Unit := do
      match ← Socket.pollCmd with
      | some cmdStr =>
        Log.write "sock" s!"poll cmd={cmdStr}"
        -- Socket sends handler names directly
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

end AppState

-- | Run a stack-level IO action with shared error handling and state reset
private partial def runStackIO (a : AppState) (f : IO (ViewStack AdbcTable)) : IO AppState := do
  match ← f.toBaseIO with
  | .ok s' => pure { a with stk := s', vs := .default, sparklines := #[] }
  | .error e => Log.error e.toString; errorPopup e.toString; pure a

-- | Run an arg command: dispatch by handler name (from config table)
private partial def runArgCmd (a : AppState) (handler : String) (arg : String) : IO AppState :=
  match handler with
  | "split"     => runStackIO a (if arg.isEmpty then Split.run a.stk else Split.runWith a.stk arg)
  | "derive"    => runStackIO a (if arg.isEmpty then Derive.run a.stk else Derive.runWith a.stk arg)
  | "filter.rowFilter" => runStackIO a (if arg.isEmpty then ViewStack.rowFilter a.stk else ViewStack.filterWith a.stk arg)
  | "filter.rowSearch" => runStackIO a (if arg.isEmpty then ViewStack.rowSearch a.stk else ViewStack.searchWith a.stk arg)
  | "filter.colSearch" => runStackIO a (if arg.isEmpty then ViewStack.colSearch a.stk else ViewStack.colJumpWith a.stk arg)
  | "export"    => runStackIO a (if arg.isEmpty then do
      match ← Export.pickFmt with | some f => Export.run a.stk f | none => pure a.stk
    else Export.runWith a.stk arg)
  | "sessSave"  => runStackIO a (do Session.saveWith a.stk arg; pure a.stk)
  | "sessLoad"  => runStackIO a (do
    match ← Session.loadWith arg with | some stk' => pure stk' | none => pure a.stk)
  | "join"      => runStackIO a (do
    match ← Join.runWith a.stk arg with | some s' => pure s' | none => pure a.stk)
  | _ => pure a

-- | Dispatch a handler name string from socket (handler name, optionally with arg after space)
private partial def dispatchHandler (a : AppState) (cmdStr : String) : IO AppState := do
  Log.write "sock" s!"cmd={cmdStr}"
  -- Handler names may include arg after space: "filter.rowFilter Bid > 100"
  let (h, arg) := match cmdStr.splitOn " " with
    | [h] => (h, "")
    | h :: rest => (h, " ".intercalate rest)
    | [] => (cmdStr, "")
  if isArgHandler h && !arg.isEmpty then runArgCmd a h arg
  else
    let ci ← CmdConfig.handlerLookup h
    match ← a.dispatch ci with
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
  -- Dispatch: handler name → dispatch → loop
  let runHandler (h : String) (rest : Array Char) : IO AppState := do
    let ci ← CmdConfig.handlerLookup h
    match ← a.dispatch ci with
    | .quit => pure a
    | .unhandled => mainLoop a test rest
    | .ok a'' =>
      let a'' := if h == "scrollUp" || h == "scrollDn" then a'' else { a'' with prevScroll := 0 }
      mainLoop a'' test rest
  -- 1. Resolve key → handler name (config first, then compile-time KeyMap, then special keys)
  let keyChar := evToChar ev
  let handler? ← do
    match ← CmdConfig.keyLookup keyChar with
    | some ci => pure (some ci.handler)
    | none => pure (evToHandler ev a.stk.cur.vkind)
  match handler? with
  | some h =>
    -- Arg commands: collect user input (in test mode, chars until \r)
    if isArgHandler h then
      let (arg, rest) := if test && ks'.any (· == '\r') then
        let idx := ks'.findIdx? (· == '\r') |>.getD ks'.size
        (String.ofList (ks'.extract 0 idx).toList, ks'.extract (idx + 1) ks'.size)
      else ("", ks')
      mainLoop (← runArgCmd a h arg) test rest
    else runHandler h ks'
  | none => mainLoop a test ks'

-- parsed CLI arguments
structure CliArgs where
  path    : Option String := none
  keys    : Array Char := #[]
  test    : Bool := false
  noSign  : Bool := false
  session : Option String := none   -- -s "name" session restore

-- extract flag with value, return (value?, remaining args)
private def extractFlag (flag : String) : List String → Option String × List String
  | f :: v :: rest => if f == flag then (some v, rest)
    else let (r, rest') := extractFlag flag (v :: rest); (r, f :: rest')
  | other => (none, other)

-- parse args: path?, -c keys?, test mode, +n, -s session
def parseArgs (args : List String) : CliArgs :=
  let noSign := args.any (· == "+n")
  let args := args.filter (· != "+n")
  let (session, args) := extractFlag "-s" args
  let toK s := (parseKeys s).toList.toArray
  match args with
  | "-c" :: k :: _ => { path := none, keys := toK k, test := true, noSign, session }
  | p :: "-c" :: k :: _ => { path := some p, keys := toK k, test := true, noSign, session }
  | p :: _ => { path := some p, noSign, session }
  | [] => { noSign, session }

-- | Init/shutdown socket + terminal around a mainLoop call
private def withTui (test : Bool) (f : IO α) : IO α := do
  let r ← Socket.bracket test f
  if !test then Term.shutdown
  pure r

-- run app with view
def runApp (v : View AdbcTable) (pipe test : Bool) (th : Theme.State) (ks : Array Char) : IO AppState := do
  if pipe then let _ ← Term.reopenTty
  let _ ← Term.init
  withTui test (mainLoop { stk := ⟨v, []⟩, vs := .default, theme := th, info := {} } test ks)

-- run from TSV string result
def runTsv (r : Except String String) (nm : String) (pipe test : Bool)
    (th : Theme.State) (ks : Array Char) : IO (Option AppState) := do
  match r with
  | .error e => IO.eprintln s!"Parse error: {e}"; return none
  | .ok tsv =>
    match ← AdbcTable.fromTsv tsv with
    | none => IO.eprintln "Empty table"; return none
    | some adbc => match View.fromTbl adbc nm with
      | none => IO.eprintln "Empty table"; return none
      | some v => some <$> runApp v pipe test th ks

-- output table as plain text
def outputTable (a : AppState) : IO Unit := do
  IO.println (← AdbcTable.toText a.stk.tbl)

-- main entry point: init backend, parse args, run app
def appMain (args : List String) : IO Unit := do
  let cli := parseArgs args
  let (path?, keys, testMode, noSign) := (cli.path, cli.keys, cli.test, cli.noSign)
  let envTest := (← IO.getEnv "TV_TEST_MODE").isSome
  Fzf.setTestMode (testMode || envTest)
  SourceConfig.setNoSign noSign
  let pipeMode ← if testMode then pure false else (! ·) <$> Term.isattyStdin
  let theme ← Theme.State.init
  Log.setLogPath (← Log.path)
  Log.write "init" s!"tmpdir={← Tc.tmpDir.get}"
  let err ← try AdbcTable.init catch e => IO.eprintln s!"Backend init error: {e}"; return
  if !err.isEmpty then IO.eprintln s!"Backend init failed: {err}"; return
  try CmdConfig.init catch e => Log.write "init" s!"cmdConfig: {e}"
  -- session restore: -s name
  if let some sessName := cli.session then
    try
      match ← Session.load sessName with
      | some stk =>
        let _ ← Term.init
        let _ ← withTui testMode (mainLoop { stk, vs := .default, theme, info := {} } testMode keys)
      | none => IO.eprintln s!"Session not found: {sessName}"
    finally AdbcTable.shutdown; Tc.cleanupTmp
    return
  if pipeMode && path?.isNone then
    if let some a ← runTsv (← Tc.TextParse.fromStdin) "stdin" true testMode theme keys then
      outputTable a
    return
  let path := path?.getD ""
  try
    let srcCfg ← SourceConfig.findSource path
    let isDir ← (path : System.FilePath).isDir
    if path.isEmpty || srcCfg.isSome || isDir then
      let p := if path.isEmpty then "." else path
      -- Config-driven direct entry (e.g. tv osquery://groups)
      if let some cfg := srcCfg then
        if !cfg.script.isEmpty && !cfg.pfx.isEmpty then
          let rest := (p.drop cfg.pfx.length).toString
          if !rest.isEmpty then
            match ← cfg.runEnter rest with
            | some adbc => match View.fromTbl adbc s!"{cfg.pfx}{rest}" with
              | some v => let _ ← runApp v pipeMode testMode theme keys
              | none => IO.eprintln s!"Empty: {p}"
            | none => IO.eprintln s!"Cannot open: {p}"
            return
      match ← Folder.mkView p 1 with
      | some v => let _ ← runApp v pipeMode testMode theme keys
      | none => IO.eprintln s!"Cannot browse: {p}"
    else if path.endsWith ".txt" then
      let _ ← runTsv (Tc.TextParse.fromText (← IO.FS.readFile path)) path pipeMode testMode theme keys
    else if path.endsWith ".gz" && !Folder.isDataFile path then
      -- Smart: try read_csv for unrecognized .gz (handles decompression natively)
      match ← Folder.tryReadCsv path with
      | some v => let _ ← runApp v pipeMode testMode theme keys
      | none => Folder.viewFile path
    else
      -- try/catch: DuckDB throws on unrecognized formats
      match ← try Folder.openFile path catch e => Log.write "open" s!"{e}"; pure none with
      | some v => let _ ← runApp v pipeMode testMode theme keys
      | none => Folder.viewFile path
  finally
    AdbcTable.shutdown
    Tc.cleanupTmp

def main (args : List String) : IO Unit := do
  try appMain args
  catch e => IO.eprintln s!"Error: {e}"
