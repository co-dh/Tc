-- App: app state, dispatch, effect runner, main loop, entry point
import Tc.Filter
import Tc.Folder
import Tc.SourceConfig
import Tc.TmpDir
import Tc.Meta
import Tc.Plot
import Tc.Fzf
import Tc.Key
import Tc.Render
import Tc.Runner
import Tc.Term
import Tc.Theme
import Tc.UI.Info
import Tc.UI.Preview
import Tc.Data.Text
import Tc.View

open Tc

-- | App state: view stack + render state + theme + info + preview scroll
structure AppState where
  stk   : ViewStack AdbcTable
  vs    : ViewState
  theme : Theme.State
  info  : UI.Info.State
  prevScroll : Nat := 0
  statusCache : String × String × String := ("", "", "")  -- (path, col, desc) — avoids per-frame DB query

namespace AppState

-- | Commands that reset ViewState to default (clear scroll/cursor) because view content changes substantially
def resetsVS (cmd : Cmd) : Bool :=
  cmd matches .stk .dec | .colSel .inc | .colSel .dec | .metaV _ | .freq _ | .fld _
    | .col .ent | .rowSel .inc | .rowSel .dec

-- | Update stk, reset vs if needed
def withStk (a : AppState) (cmd : Cmd) (s' : ViewStack AdbcTable) : AppState :=
  { a with stk := s', vs := if resetsVS cmd then .default else a.vs }

-- | Lift stack update to AppState (Kleisli helper for <|> chain)
private def liftStk (a : AppState) (cmd : Cmd) (r : Option (ViewStack AdbcTable × Effect)) : Option (AppState × Effect) :=
  r.map fun (s', eff) => (withStk a cmd s', eff)

-- | Route by Cmd discriminant, fallback to view for nav/selection
def update (a : AppState) (cmd : Cmd) : Option (AppState × Effect) :=
  let viewUp := View.update a.stk.cur cmd 20 |>.map fun (v', e) => (withStk a cmd (a.stk.setCur v'), e)
  match cmd with
  | .thm _    => a.theme.update cmd |>.map fun (t', e) => ({ a with theme := t' }, e)
  | .info _   => a.info.update cmd |>.map fun (i', e) => ({ a with info := i' }, e)
  | .stk _    => liftStk a cmd (ViewStack.update a.stk cmd)
  | .fld _    => liftStk a cmd (Folder.update a.stk cmd) <|> viewUp
  | .metaV _  => liftStk a cmd (Meta.update a.stk cmd) <|> viewUp
  | .freq _   => liftStk a cmd (Freq.update a.stk cmd) <|> viewUp
  | .plot _   => liftStk a cmd (Plot.update a.stk cmd)
  | .col .ent | .rowSel _ => liftStk a cmd (Filter.update a.stk cmd) <|> viewUp
  | .grp .inc | .grp .dec => liftStk a cmd (Filter.update a.stk cmd)
  | _ => viewUp

instance : Update (AppState) where update := update

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
  | .fzf .cmd => do match ← Fzf.cmdMode a.stk.cur.vkind with
    | some c => match a.update c with
      | some (a', e') => if e'.isNone then pure a' else runEffect a' e'
      | none => pure a
    | none => pure a
  | .themeLoad d => do let th ← a.theme.runEffect d; pure { a with theme := th }
  | _ => do
    let s ← Runner.runStackEffect a.stk e
    pure { a with stk := s, vs := if e.isNone then a.vs else .default }

-- main loop: render → input → update → effect → loop
partial def mainLoop (a : AppState) (test : Bool) (ks : Array Char) : IO AppState := do
  let (vs', v') ← a.stk.cur.doRender a.vs a.theme.styles
  let a := { a with stk := a.stk.setCur v', vs := vs' }
  renderTabLine a.stk.tabNames 0
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
  if isKey ev 'Q' then return a
  if isKey ev ' ' then mainLoop (← runEffect a (.fzf .cmd)) test ks'
  else if isKey ev 'e' then do
    match ← Fzf.fzf #["--prompt=export format "] "csv\nparquet\njson" with
    | some raw => match ExportFmt.ofString? raw.trimAscii.toString with
      | some fmt => mainLoop (← runEffect a (.export fmt)) test ks'
      | none => mainLoop a test ks'
    | none => mainLoop a test ks'
  else if isKey ev '{' then mainLoop { a with prevScroll := a.prevScroll - min a.prevScroll 5 } test ks'
  else if isKey ev '}' then mainLoop { a with prevScroll := a.prevScroll + 5 } test ks'
  else match evToCmd ev a.stk.cur.vkind with
    | none => mainLoop a test ks'
    | some c => match a.update c with
      | none => mainLoop a test ks'
      | some (a', e) =>
        if e == .quit then pure a'
        else let a'' ← if e.isNone then pure a' else runEffect a' e
             mainLoop { a'' with prevScroll := 0 } test ks'

-- parsed CLI arguments
structure CliArgs where
  path   : Option String := none
  keys   : Array Char := #[]
  test   : Bool := false
  noSign : Bool := false
  prql   : Option String := none   -- -p "prql" script mode

-- extract -p flag and its argument, return (prql?, remaining args)
private def extractPrql : List String → Option String × List String
  | "-p" :: v :: rest => (some v, rest)
  | x :: rest => let (p, r) := extractPrql rest; (p, x :: r)
  | [] => (none, [])

-- parse args: path?, -c keys?, test mode, +n, -p prql
def parseArgs (args : List String) : CliArgs :=
  let noSign := args.any (· == "+n")
  let args := args.filter (· != "+n")
  let (prql, args) := extractPrql args
  let toK s := (parseKeys s).toList.toArray
  match args with
  | "-c" :: k :: _ => { path := none, keys := toK k, test := true, noSign, prql }
  | p :: "-c" :: k :: _ => { path := some p, keys := toK k, test := true, noSign, prql }
  | p :: _ => { path := some p, noSign, prql }
  | [] => { noSign, prql }

-- run app with view
def runApp (v : View AdbcTable) (pipe test : Bool) (th : Theme.State) (ks : Array Char) : IO AppState := do
  if pipe then let _ ← Term.reopenTty
  let _ ← Term.init
  let a' ← mainLoop ⟨⟨v, []⟩, .default, th, {}, 0, ("", "", "")⟩ test ks
  if !test then Term.shutdown
  pure a'

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

-- stem from file path: "/tmp/right.csv" → "right"
private def fileStem (path : String) : String :=
  let base := (path.splitOn "/").getLast?.getD path
  (base.splitOn ".").head?.getD base

-- replace backtick file paths with their stems and register as DuckDB views
-- needed because PRQL qualifies columns with the full backtick name, but DuckDB
-- replacement scan only aliases by stem — causing "table not found" in joins
private def resolveBacktickPaths (prql : String) : IO String := do
  let parts := prql.splitOn "`"
  let mut result := ""
  let mut i := 0
  for part in parts do
    if i % 2 == 1 && !part.isEmpty && (part.any (· == '/') || part.any (· == '.')) then
      let stem := fileStem part
      let _ ← Adbc.query s!"CREATE OR REPLACE VIEW \"{escSql stem}\" AS SELECT * FROM '{escSql part}'"
      result := result ++ stem
    else if i % 2 == 1 then
      result := result ++ s!"`{part}`"  -- preserve non-path backticks
    else
      result := result ++ part
    i := i + 1
  pure result

-- run PRQL script: register CLI file as view `x`, compile PRQL, execute, print TSV
-- If prql starts with "from " or "let " → use as-is; otherwise prepend "from x |"
def runScript (path : String) (prqlOps : String) : IO Unit := do
  let _ ← Adbc.query s!"CREATE OR REPLACE VIEW x AS SELECT * FROM '{escSql path}'"
  let prqlOps ← resolveBacktickPaths prqlOps
  let prql := if prqlOps.startsWith "from " || prqlOps.startsWith "let " then prqlOps
    else s!"from x | {prqlOps}"
  let some qr ← Prql.query prql | IO.eprintln "PRQL compilation failed"; return
  let tbl ← AdbcTable.ofQueryResult qr default 0
  IO.println (← AdbcTable.toText tbl)

-- main entry point: init backend, parse args, run app
def appMain (args : List String) : IO Unit := do
  let cli := parseArgs args
  let (path?, keys, testMode, noSign) := (cli.path, cli.keys, cli.test, cli.noSign)
  let envTest := (← IO.getEnv "TC_TEST_MODE").isSome
  Fzf.setTestMode (testMode || envTest)
  SourceConfig.setNoSign noSign
  let pipeMode ← if testMode then pure false else (! ·) <$> Term.isattyStdin
  let theme ← Theme.State.init
  try Term.loadExtColors (← IO.FS.readFile "ext_colors.csv")
  catch _ => pure ()  -- use C defaults if CSV not found
  Log.setLogPath Log.path
  Log.write "init" s!"tmpdir={← Tc.tmpDir.get}"
  let err ← try AdbcTable.init catch e => IO.eprintln s!"Backend init error: {e}"; return
  if !err.isEmpty then IO.eprintln s!"Backend init failed: {err}"; return
  try SourceConfig.attachDb catch e => Log.write "init" s!"attachDb: {e}"
  -- script mode: run PRQL against file and exit
  if let some prqlOps := cli.prql then
    let path := path?.getD ""
    if path.isEmpty then IO.eprintln "tc -p requires a file argument"; return
    try runScript path prqlOps
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
      -- Config-driven direct entry (e.g. tc osquery://groups)
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
    else
      match ← Folder.openFile path with
      | some v => let _ ← runApp v pipeMode testMode theme keys
      | none => IO.eprintln s!"Cannot open file: {path}"
  finally
    AdbcTable.shutdown
    Tc.cleanupTmp

def main (args : List String) : IO Unit := do
  try appMain args
  catch e => IO.eprintln s!"Error: {e}"
