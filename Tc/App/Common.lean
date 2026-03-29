-- App: app state, dispatch, effect runner, main loop, entry point
import Tc.Filter
import Tc.Folder
import Tc.SourceConfig
import Tc.TmpDir
import Tc.Meta
import Tc.Plot
import Tc.Transpose
import Tc.Join
import Tc.Fzf
import Tc.Key
import Tc.Render
import Tc.Runner
import Tc.Derive
import Tc.Split
import Tc.Term
import Tc.Theme
import Tc.UI.Info
import Tc.UI.Preview
import Tc.Data.Text
import Tc.View
import Tc.Sparkline
import Tc.Session
import Tc.Diff
import Tc.StatusAgg
import Tc.Replay
import Tc.Socket

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

namespace AppState

-- | Update stk, reset vs if ci.resetsVS
def withStk (a : AppState) (ci : CmdConfig.CmdInfo) (s' : ViewStack AdbcTable) : AppState :=
  { a with stk := s', vs := if ci.resetsVS then .default else a.vs }

private def liftStk (a : AppState) (ci : CmdConfig.CmdInfo) (r : Option (ViewStack AdbcTable × Effect)) : Option (AppState × Effect) :=
  r.map fun (s', eff) => (withStk a ci s', eff)

-- | Dispatch by handler enum. Domain modules receive handler, not (obj,verb).
def dispatch (a : AppState) (ci : CmdConfig.CmdInfo) : Option (AppState × Effect) :=
  let h := ci.handler
  let viewUp := fun () => View.update a.stk.cur h 20 |>.map fun (v', e) => (withStk a ci (a.stk.setCur v'), e)
  match h with
  -- top-level effects
  | .quit  => some (a, .quit)
  | .xpose => some (a, .transpose)
  | .diff  => some (a, .diff)
  | .menu  => some (a, .fzf .cmd)
  -- info panel
  | .scrollUp => some ({ a with prevScroll := a.prevScroll - min a.prevScroll 5 }, .none)
  | .scrollDn => some ({ a with prevScroll := a.prevScroll + 5 }, .none)
  | .precDec  => some ({ a with stk := a.stk.setCur { a.stk.cur with precAdj := a.stk.cur.precAdj - 1 } }, .none)
  | .precInc  => some ({ a with stk := a.stk.setCur { a.stk.cur with precAdj := a.stk.cur.precAdj + 1 } }, .none)
  | .prec0    => some ({ a with stk := a.stk.setCur { a.stk.cur with precAdj := -4 } }, .none)
  | .precMax  => some ({ a with stk := a.stk.setCur { a.stk.cur with precAdj := 13 } }, .none)
  | .infoTog  => some ({ a with info := { a.info with vis := !a.info.vis } }, .none)
  | .heat0 | .heat1 | .heat2 | .heat3 =>
    let digit := match h with | .heat0 => 0 | .heat1 => 1 | .heat2 => 2 | _ => 3
    some ({ a with heatMode := digit }, .none)
  -- domain dispatch
  | .stkDup | .stkPop | .stkSwap      => liftStk a ci (ViewStack.update a.stk h)
  | .folderPush | .folderEnter | .folderParent | .folderDel
  | .folderDepthDec | .folderDepthInc  => liftStk a ci (Folder.update a.stk h) <|> viewUp ()
  | .metaPush | .metaSetKey | .metaSelNull | .metaSelSingle
                                       => liftStk a ci (Meta.update a.stk h) <|> viewUp ()
  | .freqOpen | .freqFilter            => liftStk a ci (Freq.update a.stk h) <|> viewUp ()
  | .plotArea | .plotLine | .plotScatter | .plotBar | .plotBox
  | .plotStep | .plotHist | .plotDensity | .plotViolin
                                       => liftStk a ci (Plot.update a.stk h)
  | .colSearch | .rowSearch | .rowFilter | .searchNext | .searchPrev
                                       => liftStk a ci (Filter.update a.stk h) <|> viewUp ()
  -- nav + sort + everything else → viewUp
  | _ => viewUp ()

end AppState

-- run effect, recurse on fzfCmd; errors are logged and shown as popup, never crash
partial def runEffect (a : AppState) (e : Effect) : IO AppState := do
  match ← (runEffectCore a e |>.toBaseIO) with
  | .ok a' => pure a'
  | .error err => do
    let msg := err.toString
    Log.error msg
    errorPopup msg
    pure a
where
  runEffectCore (a : AppState) (e : Effect) : IO AppState := match e with
  | .none | .quit => pure a
  | .fzf .cmd => do
    -- Live dispatch: poll socket + apply previewable cmds while fzf popup is open
    let ref ← IO.mkRef a
    let poll : IO Unit := do
      match ← Socket.pollCmd with
      | some cmdStr =>
        Log.write "sock" s!"poll cmd={cmdStr}"
        match (Parse.parse? cmdStr : Option Cmd) with
        | some cmd =>
          let ci ← CmdConfig.lookup cmd.obj cmd.verb
          match (← ref.get).dispatch ci with
          | some (a', _) =>  -- Effect discarded: poll is preview-only (re-render suffices)
            let (vs', v') ← a'.stk.cur.doRender a'.vs a'.theme.styles a'.heatMode a'.sparklines
            ref.set { a' with stk := a'.stk.setCur v', vs := vs' }
            Term.present
          | none => pure ()
        | none => Log.write "sock" s!"parse failed: {cmdStr}"
      | none => pure ()
    let cmd ← Fzf.cmdMode a.stk.cur.vkind poll
    let a ← ref.get
    let _ ← Socket.pollCmd  -- drain stale preview command from fzf focus
    match cmd with
    | some c =>
      let ci ← CmdConfig.lookup c.obj c.verb
      match a.dispatch ci with
      | some (a', e') => if e'.isNone then pure a' else runEffect a' e'
      | none => pure a
    | none => pure a
  | .transpose => do
      match ← Transpose.push a.stk with
      | some s' => pure { a with stk := s', vs := .default, sparklines := #[] }
      | none => pure a
  | .diff => do
      if a.stk.cur.sameHide.isEmpty then
        match ← Diff.run a.stk with
        | some s' => pure { a with stk := s', vs := .default, sparklines := #[] }
        | none => pure a
      else
        pure { a with stk := a.stk.setCur (Diff.showSame a.stk.cur), vs := .default }
  | _ => do
    let s ← Runner.runStackEffect a.stk e
    let vs' := if e.isNone then a.vs else .default
    let sp := if e.isNone then a.sparklines else #[]
    pure { a with stk := s, vs := vs', sparklines := sp }

-- | Run a stack-level IO action with shared error handling and state reset
private partial def runStackIO (a : AppState) (f : IO (ViewStack AdbcTable)) : IO AppState := do
  match ← f.toBaseIO with
  | .ok s' => pure { a with stk := s', vs := .default, sparklines := #[] }
  | .error e => Log.error e.toString; errorPopup e.toString; pure a

-- | Run an arg command: dispatch by handler enum (from config table)
private partial def runArgCmd (a : AppState) (handler : Handler) (arg : String) : IO AppState :=
  match handler with
  | .split     => runStackIO a (if arg.isEmpty then Split.run a.stk else Split.runWith a.stk arg)
  | .derive    => runStackIO a (if arg.isEmpty then Derive.run a.stk else Derive.runWith a.stk arg)
  | .rowFilter => runStackIO a (if arg.isEmpty then ViewStack.rowFilter a.stk else ViewStack.filterWith a.stk arg)
  | .rowSearch => runStackIO a (if arg.isEmpty then ViewStack.rowSearch a.stk else ViewStack.searchWith a.stk arg)
  | .colSearch => runStackIO a (if arg.isEmpty then ViewStack.colSearch a.stk else ViewStack.colJumpWith a.stk arg)
  | .export_   => runStackIO a (if arg.isEmpty then do
      match ← Export.pickFmt with | some f => Export.run a.stk f | none => pure a.stk
    else Export.runWith a.stk arg)
  | .sessSave  => runStackIO a (do Session.saveWith a.stk arg; pure a.stk)
  | .sessLoad  => runStackIO a (do
    match ← Session.loadWith arg with | some stk' => pure stk' | none => pure a.stk)
  | .join      => runStackIO a (do
    match ← Join.runWith a.stk arg with | some s' => pure s' | none => pure a.stk)
  | _ => pure a

-- | Dispatch a command string: parse → lookup handler → dispatch
private partial def dispatchCmd (a : AppState) (cmdStr : String) : IO AppState := do
  Log.write "sock" s!"cmd={cmdStr}"
  match (Parse.parse? cmdStr : Option Cmd) with
  | some cmd =>
    let ci ← CmdConfig.lookup cmd.obj cmd.verb
    if !cmd.arg.isEmpty then runArgCmd a ci.handler cmd.arg
    else match a.dispatch ci with
      | some (a', e) =>
        let a'' ← if e.isNone then pure a' else runEffect a' e
        pure (if e.isNone then a'' else { a'' with sparklines := #[], prevScroll := 0 })
      | none => pure a
  | none => pure a

-- main loop: render → input → update → effect → loop
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
  -- Socket commands from external tools
  let a ← match ← Socket.pollCmd with
    | some cmdStr => dispatchCmd a cmdStr
    | none => pure a
  -- \x16 = <wait> test key: sleep to let socket commands arrive
  if ev.type == 1 && ev.key == 0x16 then IO.sleep 50; return ← mainLoop a test ks'
  -- Empty event (socket wake-up with no key press) → re-render and loop
  if ev.type == 0 then return ← mainLoop a test ks'
  -- Dispatch: Cmd → update → runEffect → loop
  let runCmd (cmd : Cmd) (rest : Array Char) : IO AppState := do
    let ci ← CmdConfig.lookup cmd.obj cmd.verb
    -- Verb→arg shortcuts: bypass Effect system, call runArgCmd directly
    if let some (argH, dflt) := ← CmdConfig.argShortcut cmd.obj cmd.verb then
      return ← mainLoop (← runArgCmd a argH dflt) test rest
    match a.dispatch ci with
    | none => mainLoop a test rest
    | some (a', e) =>
      if e == .quit then pure a'
      else let a'' ← if e.isNone then pure a' else runEffect a' e
           let a'' := if e.isNone then a'' else { a'' with sparklines := #[] }
           let a'' := if ci.handler == .scrollUp || ci.handler == .scrollDn then a'' else { a'' with prevScroll := 0 }
           mainLoop a'' test rest
  -- 1. Test mode: 2-char obj+verb codes (e.g. "s1"=stk.1, "c["=col.[)
  -- Only when the first char has no single-key mapping and no nav mapping
  let ch := Char.ofNat ev.ch.toNat
  if test && ks'.size > 0 && (lookup KeyMap.char ch).isNone && !"jklh".toList.contains ch then
    let code := s!"{ch}{ks'[0]!}"
    match (Parse.parse? code : Option Cmd) with
    | some cmd => if cmd.arg.isEmpty then return ← runCmd cmd (ks'.extract 1 ks'.size)
    | _ => pure ()
  -- 2. Argument commands: prefix char + payload
  if Cmd.isArgPfx ch then do
    let (arg, rest) := if test && ks'.any (· == '\r') then
      let idx := ks'.findIdx? (· == '\r') |>.getD ks'.size
      (String.ofList (ks'.extract 0 idx).toList, ks'.extract (idx + 1) ks'.size)
    else ("", ks')
    let argCmd := Cmd.fromArg ch arg
    let argCi ← CmdConfig.lookup argCmd.obj argCmd.verb
    mainLoop (← runArgCmd a argCi.handler argCmd.arg) test rest
  else
  -- 3. Single-key shortcuts — runtime from config, fallback to compile-time KeyMap.char
  let keyChar := evToChar ev
  let keyCmd ← do
    match ← CmdConfig.keyLookup keyChar with
    | some (o, v) => pure (some { obj := o, verb := v : Cmd })
    | none => pure (lookup KeyMap.char keyChar)
  match keyCmd with
  | some cmd => runCmd cmd ks'
  -- 4. Special terminal keys + nav (Enter, Backspace, Shift+Arrow, hjkl, PgUp/Dn)
  | none => match evToCmd ev a.stk.cur.vkind with
    | some cmd => runCmd cmd ks'
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
  try SourceConfig.attachDb catch e => Log.write "init" s!"attachDb: {e}"
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
