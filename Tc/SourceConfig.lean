/-
  SourceConfig: config-driven file/folder handling for remote sources.
  Each source is a row in data/sources.duckdb (tc_sources table).

  Flow: CLI cmd → JSON → tmp file → list_sql → DuckDB temp table
  Or:   setup_cmd → setup_sql → list_sql directly (no CLI, e.g. HF root)
-/
import Tc.Data.ADBC.Table
import Tc.Error
import Tc.Render
import Tc.Remote
import Tc.TmpDir

namespace Tc.SourceConfig

-- | Config entry for a remote source
structure Config where
  pfx            : String      -- URI prefix: "s3://", "hf://", etc.
  minParts       : Nat         -- min URI parts before parent returns none
  listCmd        : String      -- shell cmd template → stdout JSON. Empty = run listSql directly.
  listSql        : String      -- SQL to transform JSON (with {src}) or query directly (no {src})
  downloadCmd    : String      -- shell cmd template to download a file
  needsDownload  : Bool        -- true: download before DuckDB read. false: DuckDB reads URI
  dirSuffix      : Bool        -- true: append "/" when joining child dir paths
  parentFallback : String      -- fallback parent when at minParts root. Empty = none.
  setupCmd       : String      -- shell cmd to run before first listing. Empty = skip.
  setupSql       : String      -- SQL to run before first listing (e.g. ATTACH). Empty = skip.
  grp            : String      -- default group column name. Empty = none.
  enterUrl       : String      -- URL template for entering file rows. Empty = default.

-- | Global flag: use --no-sign-request for S3 (set via +n arg)
initialize noSign : IO.Ref Bool ← IO.mkRef false

def setNoSign (b : Bool) : IO Unit := noSign.set b
def getNoSign : IO Bool := noSign.get

-- | Get S3 extra args string
def s3Extra : IO String := do
  if ← getNoSign then pure "--no-sign-request" else pure ""

-- | Split path into components after stripping prefix
def pathParts (pfx path : String) : Array String :=
  let rest := (path.drop pfx.length).toString
  let rest := if rest.endsWith "/" then (rest.take (rest.length - 1)).toString else rest
  if rest.isEmpty then #[] else (rest.splitOn "/").toArray

-- | Expand template placeholders: {path}, {name}, {tmp}, {extra}, {1}, {2}, {2+}, etc.
def expand (tmpl : String) (vars : Array (String × String)) : String :=
  vars.foldl (fun s (k, v) => s.replace s!"\{{k}}" v) tmpl

-- | Build template variables from a config and path
def mkVars (cfg : Config) (path tmp name extra : String) : Array (String × String) :=
  let parts := pathParts cfg.pfx path
  let baseVars := #[("path", path), ("tmp", tmp), ("name", name), ("extra", extra)]
  let numbered := (List.range 9).map fun i =>
    (s!"{i + 1}", parts.getD i "")
  let plus := (List.range 9).map fun i =>
    (s!"{i + 1}+", "/".intercalate (parts.toList.drop i))
  baseVars ++ numbered.toArray ++ plus.toArray

/-! ## Config DB -/

-- | Attach data/sources.duckdb as schema "src". Called once after AdbcTable.init.
def attachDb : IO Unit := do
  let exe ← IO.appPath
  let exeDir := exe.parent.getD "."
  let candidates := #[
    s!"{exeDir}/data/sources.duckdb",
    s!"{exeDir}/../data/sources.duckdb",
    s!"{exeDir}/../../data/sources.duckdb",
    "data/sources.duckdb"
  ]
  for c in candidates do
    if ← (c : System.FilePath).pathExists then
      let _ ← Adbc.query s!"ATTACH '{c}' AS src (READ_ONLY)"
      Log.write "init" s!"sources: {c}"
      return
  Log.write "init" "sources.duckdb not found"

-- | A typed row accessor: bundles (qr, row, colMap) so column access
-- takes only a name. The row/col swap bug is impossible by construction —
-- there's no second UInt64 argument to confuse with the row index.
structure QRow where
  private mk ::
  qr     : Adbc.QueryResult
  row    : UInt64
  colIdx : String → UInt64

namespace QRow

def ofRow (qr : Adbc.QueryResult) (row : Nat) : IO QRow := do
  let nc ← Adbc.ncols qr
  let mut m : Array (String × UInt64) := #[]
  for i in [:nc.toNat] do
    m := m.push (← Adbc.colName qr i.toUInt64, i.toUInt64)
  pure ⟨qr, row.toUInt64, fun name => (m.find? (·.1 == name) |>.map (·.2)).getD 0⟩

def str (r : QRow) (col : String) : IO String := Adbc.cellStr r.qr r.row (r.colIdx col)
def int (r : QRow) (col : String) : IO Int   := Adbc.cellInt r.qr r.row (r.colIdx col)
def bool (r : QRow) (col : String) : IO Bool  := (· != 0) <$> r.int col

end QRow

-- | Parse a Config from a query result row
private def configFromRow (qr : Adbc.QueryResult) (row : Nat) : IO Config := do
  let r ← QRow.ofRow qr row
  pure {
    pfx            := ← r.str "pfx"
    minParts       := (← r.int "min_parts").toNat
    listCmd        := ← r.str "list_cmd"
    listSql        := ← r.str "list_sql"
    downloadCmd    := ← r.str "download_cmd"
    needsDownload  := ← r.bool "needs_download"
    dirSuffix      := ← r.bool "dir_suffix"
    parentFallback := ← r.str "parent_fallback"
    setupCmd       := ← r.str "setup_cmd"
    setupSql       := ← r.str "setup_sql"
    grp            := ← r.str "grp"
    enterUrl       := ← r.str "enter_url"
  }

-- | Find config for a path by prefix match (longest prefix wins)
def findSource (path : String) : IO (Option Config) := do
  try
    let qr ← Adbc.query s!"SELECT * FROM src.tc_sources WHERE '{path.replace "'" "''"}' LIKE pfx || '%' ORDER BY length(pfx) DESC LIMIT 1"
    let n ← Adbc.nrows qr
    if n == 0 then return none
    some <$> configFromRow qr 0
  catch _ => return none

/-! ## Generic Operations -/

-- | Parent path navigation using Remote.parent with config's minParts
def Config.parent (cfg : Config) (path : String) : Option String :=
  match Remote.parent path cfg.minParts with
  | some p => some p
  | none => if cfg.parentFallback.isEmpty then none else some cfg.parentFallback

-- | Track which prefixes have completed setup
initialize setupDone : IO.Ref (Array String) ← IO.mkRef #[]

-- | Run one-time setup for a config (setupCmd + setupSql), idempotent
private def runSetup (cfg : Config) : IO Unit := do
  let done ← setupDone.get
  if done.contains cfg.pfx then return
  let homeDir := (← IO.getEnv "HOME").getD "/tmp"
  if !cfg.setupCmd.isEmpty then
    Log.write "src" s!"setup cmd: {cfg.setupCmd}"
    let cmd := expand cfg.setupCmd #[("home", homeDir)]
    let _ ← IO.Process.output { cmd := "sh", args := #["-c", cmd] }
  if !cfg.setupSql.isEmpty then
    let sql := expand cfg.setupSql #[("home", homeDir)]
    Log.write "src" s!"setup sql: {sql}"
    let _ ← Adbc.query sql
  setupDone.modify (·.push cfg.pfx)

-- | Run listing command, ingest into DuckDB temp table, return AdbcTable
def Config.runList (cfg : Config) (path : String) : IO (Option AdbcTable) := do
  Log.write "src" s!"runList: pfx={cfg.pfx} path={path} listCmd={cfg.listCmd}"
  statusMsg s!"Loading {path} ..."
  runSetup cfg
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tbl := s!"tc_src_{n}"
  if cfg.listCmd.isEmpty then
    -- Direct SQL mode: run listSql against existing tables (e.g. HF root)
    let sql := s!"CREATE TEMP TABLE {tbl} AS {cfg.listSql}"
    let _ ← Adbc.query sql
    let q : Prql.Query := { base := s!"from {tbl}" }
    AdbcTable.requery q (← AdbcTable.queryCount q)
  else
    -- CLI mode: run command, save JSON, transform via listSql
    let p := if path.endsWith "/" then path else s!"{path}/"
    let tmpDir ← Tc.tmpPath "src"
    let _ ← Log.run "src" "mkdir" #["-p", tmpDir]
    let name := path.splitOn "/" |>.filter (·.length > 0) |>.getLast? |>.getD "file"
    let extra ← if cfg.pfx == "s3://" then s3Extra else pure ""
    let vars := mkVars cfg p tmpDir name extra
    let cmd := expand cfg.listCmd vars
    Log.write "src" s!"list: {cmd}"
    let out ← IO.Process.output { cmd := "sh", args := #["-c", cmd] }
    if out.exitCode != 0 then
      let errMsg := out.stderr.trimAscii.toString
      Log.write "src" s!"list failed (exit {out.exitCode}): {errMsg}"
      errorPopup s!"List failed: {errMsg}"
      return none
    let json := out.stdout
    if json.trimAscii.toString.isEmpty then return none
    let tmpFile ← Tc.tmpPath "src-list.json"
    IO.FS.writeFile tmpFile json
    let listSql := expand cfg.listSql #[("src", tmpFile)]
    -- Only add ".." parent row if list_sql produces standard folder columns (name,size,date,type)
    let parentSql := if (cfg.parent path |>.isSome) && ((cfg.listSql.splitOn "as type").length > 1)
      then s!" UNION ALL SELECT '..' as name, 0 as size, '' as date, 'dir' as type"
      else ""
    let sql := s!"CREATE TEMP TABLE {tbl} AS {listSql}{parentSql}"
    let _ ← Adbc.query sql
    try IO.FS.removeFile tmpFile catch _ => pure ()
    let q : Prql.Query := { base := s!"from {tbl}" }
    AdbcTable.requery q (← AdbcTable.queryCount q)

-- | Download a remote file to local temp path
def Config.runDownload (cfg : Config) (path : String) : IO String := do
  statusMsg s!"Downloading {path} ..."
  let tmpDir ← Tc.tmpPath "src"
  let _ ← Log.run "src" "mkdir" #["-p", tmpDir]
  let name := path.splitOn "/" |>.filter (·.length > 0) |>.getLast? |>.getD "file"
  let extra ← if cfg.pfx == "s3://" then s3Extra else pure ""
  let vars := mkVars cfg path tmpDir name extra
  let cmd := expand cfg.downloadCmd vars
  Log.write "src" s!"download: {cmd}"
  let _ ← IO.Process.output { cmd := "sh", args := #["-c", cmd] }
  pure s!"{tmpDir}/{name}"

-- | Resolve data file path: download if needed, or return URI for DuckDB
def Config.resolve (cfg : Config) (path : String) : IO String := do
  if cfg.needsDownload then cfg.runDownload path
  else pure path

end Tc.SourceConfig
