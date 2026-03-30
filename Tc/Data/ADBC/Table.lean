/-
  ADBC backend: AdbcTable with PRQL query support
  Includes ADBC FFI declarations (DuckDB via Arrow Database Connectivity).
-/
import Tc.Data.ADBC.Prql
import Tc.Util
import Tc.Render
import Tc.Term
import Tc.Types

namespace Adbc

opaque QueryResult : Type

-- | Returns "" on success, error message on failure
@[extern "lean_adbc_init"]
opaque init : IO String

@[extern "lean_adbc_shutdown"]
opaque shutdown : IO Unit

@[extern "lean_adbc_query"]
opaque query : @& String → IO QueryResult

@[extern "lean_adbc_query_param"]
opaque queryParam : @& String → @& String → IO QueryResult

@[extern "lean_qr_ncols"]
opaque ncols : @& QueryResult → IO UInt64

@[extern "lean_qr_nrows"]
opaque nrows : @& QueryResult → IO UInt64

@[extern "lean_qr_col_name"]
opaque colName : @& QueryResult → UInt64 → IO String

@[extern "lean_qr_col_fmt"]
opaque colFmt : @& QueryResult → UInt64 → IO String

@[extern "lean_qr_cell_str"]
opaque cellStr : @& QueryResult → UInt64 → UInt64 → IO String

@[extern "lean_qr_cell_int"]
opaque cellInt : @& QueryResult → UInt64 → UInt64 → IO Int

@[extern "lean_qr_cell_float"]
opaque cellFloat : @& QueryResult → UInt64 → UInt64 → IO Float

@[extern "lean_qr_col_type"]
opaque colType : @& QueryResult → UInt64 → IO String

end Adbc

-- | Compile PRQL and execute via Adbc. Logs the PRQL, returns none on compile failure.
def Prql.query (prql : String) : IO (Option Adbc.QueryResult) := do
  Log.write "prql" prql
  let some sql ← Prql.compile prql | return none
  some <$> Adbc.query sql

namespace Tc

-- | Default row limit for PRQL queries
def prqlLimit : Nat := 1000

-- | Trim whitespace and trailing semicolon from compiled SQL
def stripSemi (s : String) : String :=
  let t := s.trimAscii.toString
  if t.endsWith ";" then (t.take (t.length - 1)).toString else t

-- | Zero-copy table with PRQL query for re-query on modification
structure AdbcTable where
  qr        : Adbc.QueryResult  -- arrow data (opaque, C memory)
  colNames  : Array String      -- cached column names
  colFmts   : Array Char        -- cached format chars per column
  colTypes  : Array String      -- cached type names (via nanoarrow)
  nRows     : Nat               -- rows in current result (≤ prqlLimit)
  query     : Prql.Query        -- PRQL query (base + ops)
  totalRows : Nat               -- total rows in underlying data

-- | Counter for unique temp table names
initialize memTblCounter : IO.Ref Nat ← IO.mkRef 0

-- | Allocate a unique temp table name: tc_{label}_{n}
def nextTmpName (label : String) : IO String := do
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  pure s!"tc_{label}_{n}"

-- | Track loaded DuckDB extensions (idempotent install+load)
initialize extLoaded : IO.Ref (Array String) ← IO.mkRef #[]

def loadDuckExt (ext : String) : IO Unit := do
  if ext.isEmpty then return
  let done ← extLoaded.get
  if done.contains ext then return
  Log.write "ext" s!"loading: {ext}"
  let _ ← Adbc.query s!"INSTALL {ext}; LOAD {ext}"
  extLoaded.modify (·.push ext)

namespace AdbcTable

-- | Init ADBC backend (DuckDB), install+load httpfs for hf:// support.
--   Returns "" on success, error message on failure.
def init : IO String := do
  let err ← Adbc.init
  if err.isEmpty then
    try let _ ← Adbc.query "INSTALL httpfs; LOAD httpfs"
    catch e => Log.write "init" s!"httpfs extension: {e}"
  pure err

-- | Shutdown ADBC backend
def shutdown : IO Unit := Adbc.shutdown

-- | Build AdbcTable from QueryResult
def ofQueryResult (qr : Adbc.QueryResult) (query : Prql.Query := default) (total : Nat := 0) : IO AdbcTable := do
  let nc ← Adbc.ncols qr
  let nr ← Adbc.nrows qr
  let mut names : Array String := #[]
  let mut fmts : Array Char := #[]
  let mut types : Array String := #[]
  for i in [:nc.toNat] do
    let n ← Adbc.colName qr i.toUInt64
    names := names.push n
    let fmt ← Adbc.colFmt qr i.toUInt64
    fmts := fmts.push (if h : fmt.length > 0 then fmt.toList[0] else '?')
    let typ ← Adbc.colType qr i.toUInt64
    types := types.push typ
  pure ⟨qr, names, fmts, types, nr.toNat, query, total⟩

-- | Extract column slice [r0, r1) as typed Column
def getCol (t : AdbcTable) (col r0 r1 : Nat) : IO Column := do
  let typ := t.colTypes.getD col "?"
  match typ with
  | "int" =>
    let mut arr : Array Int64 := #[]
    for r in [r0:r1] do
      arr := arr.push (← Adbc.cellInt t.qr r.toUInt64 col.toUInt64).toInt64
    pure (.ints arr)
  | "float" | "decimal" =>
    let mut arr : Array Float := #[]
    for r in [r0:r1] do
      arr := arr.push (← Adbc.cellFloat t.qr r.toUInt64 col.toUInt64)
    pure (.floats arr)
  | _ =>
    let mut arr : Array String := #[]
    for r in [r0:r1] do
      arr := arr.push (← Adbc.cellStr t.qr r.toUInt64 col.toUInt64)
    pure (.strs arr)

-- | Query total row count using cnt function
def queryCount (query : Prql.Query) : IO Nat := do
  let some qr ← Prql.query s!"{query.render} | cnt" | return 0
  let nr ← Adbc.nrows qr
  if nr.toNat > 0 then
    let v ← Adbc.cellInt qr 0 0
    return v.toNat
  return 0

-- | Execute PRQL query and return new AdbcTable (preserves totalRows if provided)
def requery (query : Prql.Query) (total : Nat := 0) : IO (Option AdbcTable) := do
  let some qr ← Prql.query s!"{query.render} | take {prqlLimit}" | return none
  some <$> ofQueryResult qr query total

-- | Build AdbcTable from an existing temp table name
def fromTmpTbl (tblName : String) : IO (Option AdbcTable) := do
  let q : Prql.Query := { base := s!"from {tblName}" }
  requery q (← queryCount q)

-- | Sanitize path to valid SQL identifier (alphanumeric + underscore)
private def remoteTblName (path : String) : String :=
  let s := path.toList.map (fun c => if c.isAlphanum then c else '_') |> String.ofList
  "tc_" ++ s

-- | Create from file path (queries total count).
--   Remote URLs (hf://) are materialized into a DuckDB temp table first.
def fromFile (path : String) : IO (Option AdbcTable) := do
  let query ← if path.startsWith "hf://" then do
    let tbl := remoteTblName path
    let _ ← Adbc.query s!"CREATE OR REPLACE TEMP TABLE \"{tbl}\" AS (SELECT * FROM '{escSql path}')"
    pure { base := s!"from {tbl}" : Prql.Query }
  else
    pure { base := s!"from `{path}`" : Prql.Query }
  let total ← queryCount query
  requery query total

-- | Attach a .duckdb file and list its tables as TSV (for folder-like view)
def listDuckDBTables (path : String) : IO (Option AdbcTable) := do
  let _ ← Adbc.query s!"ATTACH '{escSql path}' AS extdb (READ_ONLY)"
  let some qr ← Prql.query Prql.ducktabs | return none
  let total ← Adbc.nrows qr
  if total.toNat == 0 then return none
  some <$> ofQueryResult qr { base := Prql.ducktabs } total.toNat

-- | Get primary key columns for a table in the attached extdb
def duckDBPrimaryKeys (table : String) : IO (Array String) := do
  try
    let some qr ← Prql.query s!"from dcons | prim_keys '{escSql table}'" | return #[]
    let nr ← Adbc.nrows qr
    let mut keys : Array String := #[]
    for i in [:nr.toNat] do
      keys := keys.push (← Adbc.cellStr qr i.toUInt64 0)
    pure keys
  catch _ => pure #[]

-- | Open a table/view from an attached .duckdb file or schema-qualified name
def fromDuckDBTable (table : String) : IO (Option (AdbcTable × Array String)) := do
  let keys ← duckDBPrimaryKeys table
  let qualName := if table.contains "." then table else s!"extdb.{table}"
  let query : Prql.Query := { base := s!"from {qualName}" }
  let total ← queryCount query
  (·.map (·, keys)) <$> requery query total

-- | Sort: append sort op and re-query (all columns use given direction)
def sortBy (t : AdbcTable) (idxs : Array Nat) (asc : Bool) : IO AdbcTable := do
  let sortCols := idxs.map fun idx => (t.colNames.getD idx "", asc)
  let newQuery := t.query.pipe (.sort sortCols)
  match ← requery newQuery t.totalRows with
  | some t' => pure t'
  | none => pure t

-- | Hide columns: append select op and re-query
def hideCols (t : AdbcTable) (hideIdxs : Array Nat) : IO AdbcTable := do
  match ← requery (t.query.pipe (.sel (keepCols t.colNames.size hideIdxs t.colNames))) t.totalRows with
  | some t' => pure t' | none => pure t

-- | Exclude columns: append EXCLUDE op and re-query (DuckDB SELECT * EXCLUDE)
def excludeCols (t : AdbcTable) (cols : Array String) : IO AdbcTable := do
  match ← requery (t.query.pipe (.exclude cols)) t.totalRows with
  | some t' => pure t' | none => pure t

-- | Fetch more rows (increase limit by prqlLimit)
def fetchMore (t : AdbcTable) : IO (Option AdbcTable) := do
  if t.nRows >= t.totalRows then return none
  let limit := t.nRows + prqlLimit
  let some qr ← Prql.query s!"{t.query.render} | take {limit}" | return none
  some <$> ofQueryResult qr t.query t.totalRows

-- | Export plot data to tmpdir/plot.dat via DuckDB COPY (downsample in SQL)
-- truncLen: SUBSTRING length for time truncation; step: every-Nth-row for non-time
def plotExport (t : AdbcTable) (xName yName : String) (catName? : Option String) (xIsTime : Bool) (step : Nat) (truncLen : Nat)
    : IO (Option (Array String)) := do
  let q := Prql.quote
  -- time-like: use PRQL ds_trunc; non-time: hand-write SQL (PRQL miscompiles ROW_NUMBER + select)
  let sql' ← do
    let prql := if xIsTime then
        match catName? with
        | some cn => s!"{t.query.render} | ds_trunc_cat {q xName} {q yName} {q cn} {truncLen}"
        | none    => s!"{t.query.render} | ds_trunc {q xName} {q yName} {truncLen}"
      else
        let selCols := match catName? with
          | some cn => s!"{q xName}, {q yName}, {q cn}"
          | none    => s!"{q xName}, {q yName}"
        let dsFn := match catName? with
          | some cn => s!"ds_nth_cat {q yName} {q cn} {truncLen}"
          | none    => s!"ds_nth {q yName} {truncLen}"
        s!"{t.query.render} | {dsFn} | select \{{selCols}}"
    Log.write "prql" prql
    let some sql ← Prql.compile prql | return none
    pure (stripSemi sql)
  let datPath ← Tc.tmpPath "plot.dat"
  let copySql := s!"COPY ({sql'}) TO '{datPath}' (FORMAT CSV, DELIMITER '\\t', HEADER false)"
  Log.write "plot-sql" copySql
  try
    let _ ← Adbc.query copySql
  catch e =>
    let msg := s!"COPY failed: {e.toString}"
    Log.write "plot" msg
    throw (IO.userError msg)
  -- get distinct categories if needed
  match catName? with
  | some cn =>
    let some catQr ← Prql.query s!"{t.query.render} | uniq {q cn}" | return some #[]
    let nr ← Adbc.nrows catQr
    let mut cats : Array String := #[]
    for i in [:nr.toNat] do
      cats := cats.push (← Adbc.cellStr catQr i.toUInt64 0)
    return some cats
  | none => return some #[]

-- | Ingest content via DuckDB reader into a temp table
private def fromIngest (content label reader : String) : IO (Option AdbcTable) := do
  if content.isEmpty || content.trimAscii.toString == "[]" then return none
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tmp ← Tc.tmpPath s!"{label}-{n}.{label}"
  IO.FS.writeFile tmp content
  let tbl := s!"tc_{label}_{n}"
  try
    let _ ← Adbc.query s!"CREATE TEMP TABLE {tbl} AS SELECT * FROM {reader}('{tmp}')"
    Tc.tryRemoveFile tmp
    let q : Prql.Query := { base := s!"from {tbl}" }
    requery q (← queryCount q)
  catch e =>
    Tc.tryRemoveFile tmp
    Log.write label s!"error: {e.toString}"
    return none

def fromTsv (content : String) := fromIngest content "tsv" "read_csv_auto"
def fromJson (content : String) := fromIngest content "json" "read_json_auto"

-- | Create from file path with optional setup SQL and reader function.
--   setupSql: run before reading (e.g. "INSTALL arrow; LOAD arrow")
--   reader: DuckDB reader function (e.g. "read_arrow"). Empty = auto-detect via backtick.
def fromFileWith (path : String) (reader duckdbExt : String) : IO (Option AdbcTable) := do
  loadDuckExt duckdbExt
  if reader.isEmpty then
    fromFile path
  else
    -- Materialize via reader function into temp table (PRQL can't parse function calls in `from`)
    let tbl := remoteTblName path
    let _ ← Adbc.query s!"CREATE OR REPLACE TEMP TABLE \"{tbl}\" AS (SELECT * FROM {reader}('{escSql path}'))"
    let query : Prql.Query := { base := s!"from {tbl}" }
    let total ← queryCount query
    requery query total

end AdbcTable

-- NOTE: TblOps/ModifyTable instances for AdbcTable are in ADBC/Ops.lean

namespace AdbcTable

-- | Create freq table entirely in SQL (no round-trip to Lean)
def freqTable (t : AdbcTable) (colNames : Array String) : IO (Option (AdbcTable × Nat)) := do
  if colNames.isEmpty then return none
  let cols := colNames.map Prql.quote |>.toList |> ", ".intercalate
  -- total distinct groups
  let totalGroups ← do
    let some qr ← Prql.query s!"{t.query.render} | cntdist \{{cols}}" | pure 0
    let v ← Adbc.cellStr qr 0 0
    pure (v.toNat?.getD 0)
  -- freq table: uses freq PRQL function which computes Cnt, Pct, Bar in SQL
  let tblName ← nextTmpName "freq"
  let prql := s!"{t.query.render} | freq \{{cols}} | take 1000"
  Log.write "prql" prql
  let some sql ← Prql.compile prql | return none
  let sql := stripSemi sql
  let _ ← Adbc.query s!"CREATE TEMP TABLE {tblName} AS {sql}"
  match ← requery { base := s!"from {tblName}" } with
  | some t' => return some (t', totalGroups)
  | none => return none

-- | Filter: requery with filter (queries new filtered count)
def filter (t : AdbcTable) (expr : String) : IO (Option AdbcTable) := do
  let q := t.query.filter expr
  let total ← queryCount q
  AdbcTable.requery q total

-- | Distinct: use SQL DISTINCT
def distinct (t : AdbcTable) (col : Nat) : IO (Array String) := do
  let colName := t.colNames.getD col ""
  let some qr ← Prql.query s!"{t.query.render} | uniq {Prql.quote colName}" | return #[]
  let nr ← Adbc.nrows qr
  let mut result : Array String := #[]
  for i in [:nr.toNat] do
    result := result.push (← Adbc.cellStr qr i.toUInt64 0)
  pure result

-- | Find row from starting position, forward or backward (with wrap).
--   PRQL row_number is 1-based; we subtract 1 here for 0-based Lean array indexing.
def findRow (t : AdbcTable) (col : Nat) (val : String) (start : Nat) (fwd : Bool) : IO (Option Nat) := do
  let colName := Prql.quote (t.colNames.getD col "")
  let some qr ← Prql.query s!"{t.query.render} | derive \{_rn = row_number this} | filter ({colName} == '{escSql val}') | select \{_rn}" | return none
  let nr ← Adbc.nrows qr
  let mut rows : Array Nat := #[]
  for i in [:nr.toNat] do
    rows := rows.push ((← Adbc.cellInt qr i.toUInt64 0).toNat - 1)
  if rows.isEmpty then return none
  -- find first row >= start (forward) or last row < start (backward), with wrap
  if fwd then
    return rows.find? (· >= start) |>.orElse fun _ => if rows.isEmpty then none else some (rows.getD 0 0)
  else
    return rows.findRev? (· < start) |>.orElse fun _ => if rows.isEmpty then none else some (rows.back!)

-- | Create AdbcTable from column names + Column arrays via DuckDB temp table
def fromArrays (names : Array String) (cols : Array Column) : IO (Option AdbcTable) := do
  if names.isEmpty || cols.isEmpty then return none
  let nRows := (cols.getD 0 default).size
  if nRows == 0 then return none
  -- build column aliases
  let colAliases := names.map fun n => s!"\"{escSql n}\""
  -- build VALUES rows
  let mut rows : Array String := #[]
  for r in [:nRows] do
    let mut vals : Array String := #[]
    for c in cols do
      let v := match c with
        | .ints data   => s!"{data.getD r 0}"
        | .floats data => s!"{data.getD r 0}"
        | .strs data   => s!"'{escSql (data.getD r "")}'"
      vals := vals.push v
    rows := rows.push s!"({vals.toList |> ", ".intercalate})"
  let valuesSql := rows.toList |> ", ".intercalate
  let aliasSql := colAliases.toList |> ", ".intercalate
  let tblName ← nextTmpName "arr"
  let sql := s!"CREATE OR REPLACE TEMP TABLE {tblName} AS SELECT * FROM (VALUES {valuesSql}) AS t({aliasSql})"
  Log.write "fromArrays" sql
  let _ ← Adbc.query sql
  let qr ← Adbc.query s!"SELECT * FROM {tblName}"
  some <$> ofQueryResult qr { base := s!"from {tblName}" } nRows

end AdbcTable

end Tc
