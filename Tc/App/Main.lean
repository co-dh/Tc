-- App/Main: CLI parsing, app init, entry point
import Tc.App.Common

open Tc

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
  initHandlers
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
    else if path.endsWith ".gz" && !FileFormat.isDataFile path then
      -- Smart: try read_csv for unrecognized .gz (handles decompression natively)
      match ← FileFormat.tryReadCsv path with
      | some v => let _ ← runApp v pipeMode testMode theme keys
      | none => FileFormat.viewFile path
    else
      -- try/catch: DuckDB throws on unrecognized formats
      match ← try FileFormat.openFile path catch e => Log.write "open" s!"{e}"; pure none with
      | some v => let _ ← runApp v pipeMode testMode theme keys
      | none => FileFormat.viewFile path
  finally
    AdbcTable.shutdown
    Tc.cleanupTmp

def main (args : List String) : IO Unit := do
  try appMain args
  catch e => IO.eprintln s!"Error: {e}"
