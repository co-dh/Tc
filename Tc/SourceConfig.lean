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

-- | Config entry for a source
structure Config where
  pfx            : String      -- URI prefix: "s3://", "hf://", etc. Empty = catch-all.
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
  script         : String      -- shell cmd template for enter: stdout = JSON rows. Empty = none.
  attach         : Bool        -- true: enter uses fromDuckDBTable (for attached databases)
  duckdbExt      : String      -- DuckDB extension to auto INSTALL/LOAD (e.g. "postgres"). Empty = none.
  attachType     : String      -- ATTACH TYPE clause (e.g. "POSTGRES"). Empty = native DuckDB.

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
-- For empty values, also removes a preceding "/" to avoid trailing slashes
-- (e.g., "tree/main/{3+}" with empty {3+} becomes "tree/main" not "tree/main/").
def expand (tmpl : String) (vars : Array (String × String)) : String :=
  vars.foldl (fun s (k, v) =>
    if v.isEmpty then s.replace s!"/\{{k}}" "" |>.replace s!"\{{k}}" ""
    else s.replace s!"\{{k}}" v) tmpl

-- | Build template variables from a config and path
def mkVars (cfg : Config) (path tmp name extra : String) : Array (String × String) :=
  let parts := pathParts cfg.pfx path
  let dsn := (path.drop cfg.pfx.length).toString  -- path with prefix stripped, no splitting
  let baseVars := #[("path", path), ("tmp", tmp), ("name", name), ("extra", extra), ("dsn", dsn)]
  let numbered := (List.range 9).map fun i =>
    (s!"{i + 1}", parts.getD i "")
  let plus := (List.range 9).map fun i =>
    (s!"{i + 1}+", "/".intercalate (parts.toList.drop i))
  baseVars ++ numbered.toArray ++ plus.toArray

-- | Reject shell metacharacters in user-supplied values before template expansion.
-- Blocklist approach: reject only dangerous chars, allow everything else (unicode, #, etc.)
private def hasShellMeta (s : String) : Bool :=
  s.any fun c => c == '$' || c == '`' || c == ';' || c == '&' || c == '|'
    || c == '(' || c == ')' || c == '!' || c == '{' || c == '}' || c == '<'
    || c == '>' || c == '\\' || c == '"' || c == '\'' || c == '\n'

private def validateShellSafe (s : String) (label : String) : IO Unit := do
  if hasShellMeta s then
    Log.write "src" s!"rejected unsafe {label}: {s}"
    throw (IO.userError s!"Path contains shell metacharacters: {s}")

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
  Log.write "init" "sources.duckdb not found (remote source browsing disabled)"

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
    script         := ← r.str "script"
    attach         := ← r.bool "attach"
    duckdbExt      := ← r.str "duckdb_ext"
    attachType     := ← r.str "attach_type"
  }

-- | Find config for a path by prefix match (longest prefix wins)
def findSource (path : String) : IO (Option Config) := do
  try
    let qr ← Adbc.queryParam "SELECT * FROM src.tc_sources WHERE pfx != '' AND $1 LIKE pfx || '%' ORDER BY length(pfx) DESC LIMIT 1" path
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

-- | Track which prefixes/extensions have completed setup
initialize setupDone : IO.Ref (Array String) ← IO.mkRef #[]

-- | Track which DuckDB extensions have been installed/loaded
initialize extLoaded : IO.Ref (Array String) ← IO.mkRef #[]

-- | Install and load a DuckDB extension (idempotent)
def loadExt (ext : String) : IO Unit := do
  if ext.isEmpty then return
  let done ← extLoaded.get
  if done.contains ext then return
  Log.write "src" s!"loading ext: {ext}"
  let _ ← Adbc.query s!"INSTALL {ext}; LOAD {ext}"
  extLoaded.modify (·.push ext)

-- | Run one-time setup for a config (duckdbExt + setupCmd + setupSql), idempotent.
--   Tries setupSql first; if it fails (e.g. DB doesn't exist), runs setupCmd to create it.
def runSetup (cfg : Config) : IO Unit := do
  loadExt cfg.duckdbExt
  let done ← setupDone.get
  if done.contains cfg.pfx then return
  let homeDir := (← IO.getEnv "HOME").getD "/tmp"
  if !cfg.setupSql.isEmpty then
    let sql := expand cfg.setupSql #[("home", homeDir)]
    -- Try ATTACH first; if it succeeds, skip the expensive setupCmd
    match ← (Adbc.query sql |>.toBaseIO) with
    | .ok _ =>
      Log.write "src" s!"setup sql ok (skipped cmd): {sql}"
      setupDone.modify (·.push cfg.pfx)
      return
    | .error _ =>
      Log.write "src" s!"setup sql failed, running cmd first"
  if !cfg.setupCmd.isEmpty then
    Log.write "src" s!"setup cmd: {cfg.setupCmd}"
    let cmd := expand cfg.setupCmd #[("home", homeDir)]
    let _ ← IO.Process.output { cmd := "sh", args := #["-c", cmd] }
  if !cfg.setupSql.isEmpty then
    let sql := expand cfg.setupSql #[("home", homeDir)]
    Log.write "src" s!"setup sql: {sql}"
    let _ ← Adbc.query sql
  setupDone.modify (·.push cfg.pfx)

-- | Allocate a fresh temp table name
private def freshTbl : IO String := do
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  pure s!"tc_src_{n}"

-- | Build AdbcTable from a temp table name
private def fromTbl (tbl : String) : IO (Option AdbcTable) := do
  let q : Prql.Query := { base := s!"from {tbl}" }
  AdbcTable.requery q (← AdbcTable.queryCount q)

-- | Extract last path component as a filename
private def nameFromPath (path : String) : String :=
  path.splitOn "/" |>.filter (·.length > 0) |>.getLast? |>.getD "file"

-- | Build template vars for a config + path (shared by runList/runDownload).
-- Returns (vars, tmpDir) so callers don't need to search the array.
private def Config.cmdVars (cfg : Config) (path : String) : IO (Array (String × String) × String) := do
  validateShellSafe path "path"
  let tmpDir ← Tc.tmpPath "src"
  let _ ← Log.run "src" "mkdir" #["-p", tmpDir]
  let extra ← if cfg.pfx == "s3://" then s3Extra else pure ""
  pure (mkVars cfg path tmpDir (nameFromPath path) extra, tmpDir)

-- | Generate attach SQL from config fields (DRY: DETACH/ATTACH/SELECT pattern)
private def Config.attachSql (cfg : Config) (connStr : String) : String :=
  let typClause := if cfg.attachType.isEmpty then "" else s!"TYPE {cfg.attachType}, "
  s!"DETACH DATABASE IF EXISTS extdb;\nATTACH '{escSql connStr}' AS extdb ({typClause}READ_ONLY);\nSELECT table_name as name, estimated_size as size, column_count as columns FROM duckdb_tables() WHERE database_name = 'extdb' AND schema_name NOT IN ('information_schema', 'pg_catalog')"

-- | Run listing: CLI cmd → JSON → listSql, or direct SQL mode
def Config.runList (cfg : Config) (path : String) : IO (Option AdbcTable) := do
  Log.write "src" s!"runList: pfx={cfg.pfx} path={path} listCmd={cfg.listCmd}"
  statusMsg s!"Loading {path} ..."
  runSetup cfg
  let tbl ← freshTbl
  if cfg.listCmd.isEmpty then
    -- Auto-generate attach SQL if attach=true and no custom listSql
    let sql ← if cfg.attach && cfg.listSql.isEmpty then do
      let connStr := if cfg.pfx.isEmpty then path
        else (path.drop cfg.pfx.length).toString
      pure (cfg.attachSql connStr)
    else do
      let (vars, _) ← cfg.cmdVars path
      pure (expand cfg.listSql vars)
    -- Direct SQL mode: support multi-statement (split on ";\n")
    let stmts := sql.splitOn ";\n" |>.map (·.trimAscii.toString) |>.filter (·.length > 0)
    for stmt in stmts.dropLast do
      let _ ← Adbc.query stmt
    let selectSql := stmts.getLast?.getD sql
    let _ ← Adbc.query s!"CREATE TEMP TABLE {tbl} AS {selectSql}"
    fromTbl tbl
  else
    -- CLI mode: run command, save JSON, transform via listSql
    let p := if path.endsWith "/" then path else s!"{path}/"
    let (vars, _) ← cfg.cmdVars p
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
    let _ ← Adbc.query s!"CREATE TEMP TABLE {tbl} AS {listSql}"
    -- Auto-unnest: if result is 1 row with a struct[] column, expand it
    try
      let qr ← Adbc.query s!"SELECT column_name FROM duckdb_columns() WHERE table_name='{tbl}' AND data_type LIKE 'STRUCT%[]' LIMIT 1"
      let cnt ← Adbc.query s!"SELECT count(*)::INT FROM {tbl}"
      let col ← Adbc.cellStr qr 0 0
      let n ← Adbc.cellInt cnt 0 0
      if n == 1 && !col.isEmpty then
        let _ ← Adbc.query s!"CREATE OR REPLACE TEMP TABLE {tbl} AS SELECT unnest(\"{col}\", recursive:=true) FROM {tbl}"
    catch _ => pure ()
    -- Add ".." parent row if table has standard folder columns (name,size,date,type)
    if cfg.parent path |>.isSome then
      try let _ ← Adbc.query s!"INSERT INTO {tbl} SELECT '..' as name, 0 as size, '' as date, 'dir' as type"
      catch _ => pure ()
    try IO.FS.removeFile tmpFile catch _ => pure ()
    fromTbl tbl

-- | Download a remote file to local temp path
def Config.runDownload (cfg : Config) (path : String) : IO String := do
  statusMsg s!"Downloading {path} ..."
  let (vars, tmpDir) ← cfg.cmdVars path
  let cmd := expand cfg.downloadCmd vars
  Log.write "src" s!"download: {cmd}"
  let _ ← IO.Process.output { cmd := "sh", args := #["-c", cmd] }
  pure s!"{tmpDir}/{nameFromPath path}"

-- | Resolve data file path: download if needed, or return URI for DuckDB
def Config.resolve (cfg : Config) (path : String) : IO String := do
  if cfg.needsDownload then cfg.runDownload path
  else pure path

-- | Run enter: script cmd → JSON → DuckDB temp table, apply types from stub view
def Config.runEnter (cfg : Config) (name : String) : IO (Option AdbcTable) := do
  if cfg.script.isEmpty then return none
  validateShellSafe name "name"
  runSetup cfg
  let vars := mkVars cfg (cfg.pfx ++ name) "" name ""
  let cmd := expand cfg.script vars
  Log.write "src" s!"enter: {cmd}"
  let out ← IO.Process.output { cmd := "sh", args := #["-c", cmd] }
  if out.exitCode != 0 then
    Log.write "src" s!"enter failed: {out.stderr.trimAscii.toString}"
    return none
  let json := out.stdout
  if json.trimAscii.toString.isEmpty || json.trimAscii.toString == "[]" then return none
  let tbl ← freshTbl
  let tmpFile ← Tc.tmpPath s!"src-enter-{tbl}.json"
  IO.FS.writeFile tmpFile json
  let _ ← Adbc.query s!"CREATE TEMP TABLE {tbl} AS SELECT * FROM read_json_auto('{tmpFile}')"
  try IO.FS.removeFile tmpFile catch _ => pure ()
  -- Apply types from DuckDB stub view (e.g. osq.groups has typed columns)
  let typeApply : IO Unit := do
    let qr ← Adbc.queryParam "SELECT column_name, data_type FROM duckdb_columns() WHERE table_name = $1 AND data_type != 'VARCHAR'" name
    let nr ← Adbc.nrows qr
    for i in [:nr.toNat] do
      let colName ← Adbc.cellStr qr i.toUInt64 0
      let colType ← Adbc.cellStr qr i.toUInt64 1
      let alter := s!"ALTER TABLE {tbl} ALTER COLUMN \"{colName}\" TYPE {colType} USING TRY_CAST(\"{colName}\" AS {colType})"
      try let _ ← Adbc.query alter catch _ => pure ()
  try typeApply catch e => Log.write "src" s!"enter types: {e}"
  fromTbl tbl

end Tc.SourceConfig
