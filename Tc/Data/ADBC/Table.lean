/-
  ADBC backend: AdbcTable with PRQL query support
  Includes ADBC FFI declarations (DuckDB via Arrow Database Connectivity).
-/
import Tc.Data.ADBC.Prql
import Tc.Error
import Tc.Render
import Tc.Term
import Tc.TmpDir
import Tc.Types

namespace Adbc

opaque QueryResult : Type

@[extern "lean_adbc_init"]
opaque init : IO Bool

@[extern "lean_adbc_shutdown"]
opaque shutdown : IO Unit

@[extern "lean_adbc_query"]
opaque query : @& String → IO QueryResult

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

namespace AdbcTable

-- | Init ADBC backend (DuckDB), install+load httpfs for hf:// support
def init : IO Bool := do
  let ok ← Adbc.init
  if ok then
    try let _ ← Adbc.query "INSTALL httpfs; LOAD httpfs"
    catch e => Log.write "init" s!"httpfs extension: {e}"
  pure ok

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
  let prql := s!"{query.render} | cnt"
  Log.write "prql" prql
  let some sql ← Prql.compile prql | return 0
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  if nr.toNat > 0 then
    let v ← Adbc.cellInt qr 0 0
    return v.toNat
  return 0

-- | Execute PRQL query and return new AdbcTable (preserves totalRows if provided)
def requery (query : Prql.Query) (total : Nat := 0) : IO (Option AdbcTable) := do
  let prql := s!"{query.render} | take {prqlLimit}"
  Log.write "prql" prql
  let some sql ← Prql.compile prql | return none
  let qr ← Adbc.query sql
  some <$> ofQueryResult qr query total

-- | Sanitize path to valid SQL identifier (alphanumeric + underscore)
private def remoteTblName (path : String) : String :=
  "tc_" ++ String.ofList (path.toList.map fun c => if c.isAlphanum then c else '_')

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
  let qr ← Adbc.query "SELECT table_name as name, estimated_size as size, column_count as columns FROM duckdb_tables() WHERE database_name = 'extdb'"
  let total ← Adbc.nrows qr
  if total.toNat == 0 then return none
  some <$> ofQueryResult qr { base := "from duckdb_tables() | filter database_name == 'extdb' | select {name, estimated_size, column_count}" } total.toNat

-- | Get primary key columns for a table in the attached extdb
def duckDBPrimaryKeys (table : String) : IO (Array String) := do
  try
    let qr ← Adbc.query s!"SELECT unnest(constraint_column_names) as col FROM duckdb_constraints() WHERE database_name = 'extdb' AND table_name = '{escSql table}' AND constraint_type = 'PRIMARY KEY'"
    let nr ← Adbc.nrows qr
    let mut keys : Array String := #[]
    for i in [:nr.toNat] do
      keys := keys.push (← Adbc.cellStr qr i.toUInt64 0)
    pure keys
  catch _ => pure #[]

-- | Open a table from an attached .duckdb file
def fromDuckDBTable (table : String) : IO (Option (AdbcTable × Array String)) := do
  let keys ← duckDBPrimaryKeys table
  let query : Prql.Query := { base := s!"from extdb.{table}" }
  let total ← queryCount query
  (·.map (·, keys)) <$> requery query total

-- | Sort: append sort op and re-query (all columns use given direction)
def sortBy (t : AdbcTable) (idxs : Array Nat) (asc : Bool) : IO AdbcTable := do
  let sortCols := idxs.map fun idx => (t.colNames.getD idx "", asc)
  let newQuery := t.query.pipe (.sort sortCols)
  match ← requery newQuery t.totalRows with
  | some t' => pure t'
  | none => pure t

-- | Delete columns: append select op and re-query
def delCols (t : AdbcTable) (delIdxs : Array Nat) : IO AdbcTable := do
  match ← requery (t.query.pipe (.sel (keepCols t.colNames.size delIdxs t.colNames))) t.totalRows with
  | some t' => pure t' | none => pure t

-- | Fetch more rows (increase limit by prqlLimit)
def fetchMore (t : AdbcTable) : IO (Option AdbcTable) := do
  if t.nRows >= t.totalRows then return none
  let limit := t.nRows + prqlLimit
  let prql := s!"{t.query.render} | take {limit}"
  Log.write "fetch" s!"fetchMore limit={limit}"
  let some sql ← Prql.compile prql | return none
  let qr ← Adbc.query sql
  some <$> ofQueryResult qr t.query t.totalRows

-- | Export plot data to tmpdir/plot.dat via DuckDB COPY (downsample in SQL)
-- truncLen: SUBSTRING length for time truncation; step: every-Nth-row for non-time
def plotExport (t : AdbcTable) (xName yName : String) (catName? : Option String) (xIsTime : Bool) (step : Nat) (truncLen : Nat)
    : IO (Option (Array String)) := do
  let q := Prql.quote
  -- time-like: use PRQL ds_trunc; non-time: hand-write SQL (PRQL miscompiles ROW_NUMBER + select)
  let sql' ← do
    if xIsTime then
      let prql := match catName? with
        | some cn => s!"{t.query.render} | ds_trunc_cat {q xName} {q yName} {q cn} {truncLen}"
        | none    => s!"{t.query.render} | ds_trunc {q xName} {q yName} {truncLen}"
      Log.write "plot-prql" prql
      let some sql ← Prql.compile prql | return none
      pure (stripSemi sql)
    else
      let selCols := match catName? with
        | some cn => s!"{q xName}, {q yName}, {q cn}"
        | none    => s!"{q xName}, {q yName}"
      let dsFn := match catName? with
        | some cn => s!"ds_nth_cat {q yName} {q cn} {step}"
        | none    => s!"ds_nth {q yName} {step}"
      let prql := s!"{t.query.render} | {dsFn} | select \{{selCols}}"
      Log.write "plot-prql" prql
      let some sql ← Prql.compile prql | return none
      pure (stripSemi sql)
  let datPath ← Tc.tmpPath "plot.dat"
  let copySql := s!"COPY ({sql'}) TO '{datPath}' (FORMAT CSV, DELIMITER '\\t', HEADER false)"
  Log.write "plot-sql" copySql
  try
    let _ ← Adbc.query copySql
  catch e =>
    Log.write "plot" s!"COPY failed: {e.toString}"
    return none
  -- get distinct categories if needed
  match catName? with
  | some cn =>
    let catPrql := s!"{t.query.render} | uniq {q cn}"
    let some catSql ← Prql.compile catPrql | return some #[]
    let catQr ← Adbc.query catSql
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
    try IO.FS.removeFile tmp catch _ => pure ()
    let q : Prql.Query := { base := s!"from {tbl}" }
    requery q (← queryCount q)
  catch e =>
    try IO.FS.removeFile tmp catch _ => pure ()
    Log.write label s!"error: {e.toString}"
    return none

def fromTsv (content : String) := fromIngest content "tsv" "read_csv_auto"
def fromJson (content : String) := fromIngest content "json" "read_json_auto"

end AdbcTable

-- NOTE: TblOps/ModifyTable instances for AdbcTable are in ADBC/Ops.lean

namespace AdbcTable

-- | Create freq table entirely in SQL (no round-trip to Lean)
def freqTable (t : AdbcTable) (colNames : Array String) : IO (Option (AdbcTable × Nat)) := do
  if colNames.isEmpty then return none
  let cols := colNames.map Prql.quote |> (", ".intercalate ·.toList)
  -- total distinct groups
  let cntPrql := s!"{t.query.render} | cntdist \{{cols}}"
  Log.write "prql-cntdist" cntPrql
  let totalGroups ← do
    let some sql ← Prql.compile cntPrql | pure 0
    let qr ← Adbc.query sql
    let v ← Adbc.cellStr qr 0 0
    pure (v.toNat?.getD 0)
  -- freq table: uses freq PRQL function which computes Cnt, Pct, Bar in SQL
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tblName := s!"tc_freq_{n}"
  let prql := s!"{t.query.render} | freq \{{cols}} | take 1000"
  Log.write "prql-freq" prql
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
  let prql := s!"{t.query.render} | uniq {Prql.quote colName}"
  Log.write "prql" prql
  let some sql ← Prql.compile prql | return #[]
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  let mut result : Array String := #[]
  for i in [:nr.toNat] do
    result := result.push (← Adbc.cellStr qr i.toUInt64 0)
  pure result

-- | Find row from starting position, forward or backward (with wrap).
--   Uses PRQL row_number (sort-aware) instead of raw SQL ROW_NUMBER.
def findRow (t : AdbcTable) (col : Nat) (val : String) (start : Nat) (fwd : Bool) : IO (Option Nat) := do
  let colName := Prql.quote (t.colNames.getD col "")
  let prql := s!"{t.query.render} | derive \{_rn0 = row_number this} | derive \{_rn = _rn0 - 1} | filter ({colName} == '{escSql val}') | select \{_rn}"
  let some sql ← Prql.compile prql | return none
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  let mut rows : Array Nat := #[]
  for i in [:nr.toNat] do
    rows := rows.push (← Adbc.cellInt qr i.toUInt64 0).toNat
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
    rows := rows.push s!"({", ".intercalate vals.toList})"
  let valuesSql := ", ".intercalate rows.toList
  let aliasSql := ", ".intercalate colAliases.toList
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tblName := s!"tc_arr_{n}"
  let sql := s!"CREATE OR REPLACE TEMP TABLE {tblName} AS SELECT * FROM (VALUES {valuesSql}) AS t({aliasSql})"
  Log.write "fromArrays" sql
  let _ ← Adbc.query sql
  let qr ← Adbc.query s!"SELECT * FROM {tblName}"
  some <$> ofQueryResult qr { base := s!"from {tblName}" } nRows

end AdbcTable

end Tc
