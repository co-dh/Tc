/-
  ADBC backend: AdbcTable with PRQL query support
-/
import Tc.Data.ADBC.FFI
import Tc.Data.ADBC.Prql
import Tc.Error
import Tc.Render
import Tc.Term
import Tc.Types

namespace Tc

-- | Default row limit for PRQL queries
def prqlLimit : Nat := 1000

-- | Zero-copy table with PRQL query for re-query on modification
structure AdbcTable where
  qr        : Adbc.QueryResult  -- arrow data (opaque, C memory)
  colNames  : Array String      -- cached column names
  colFmts   : Array Char        -- cached format chars per column
  colTypes  : Array String      -- cached type names (via nanoarrow)
  nRows     : Nat               -- rows in current result (≤ prqlLimit)
  nCols     : Nat
  query     : Prql.Query        -- PRQL query (base + ops)
  totalRows : Nat               -- total rows in underlying data

namespace AdbcTable

-- | Init ADBC backend (DuckDB)
def init : IO Bool := Adbc.init

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
  pure ⟨qr, names, fmts, types, nr.toNat, nc.toNat, query, total⟩

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

-- | Create from file path (queries total count)
def fromFile (path : String) : IO (Option AdbcTable) := do
  let query : Prql.Query := { base := s!"from `{path}`" }
  let total ← queryCount query
  requery query total

-- | Sort: append sort op and re-query (all columns use given direction)
def sortBy (t : AdbcTable) (idxs : Array Nat) (asc : Bool) : IO AdbcTable := do
  let sortCols := idxs.map fun idx => (t.colNames.getD idx "", asc)
  let newQuery := t.query.pipe (.sort sortCols)
  match ← requery newQuery t.totalRows with
  | some t' => pure t'
  | none => pure t

-- | Delete columns: append select op and re-query
def delCols (t : AdbcTable) (delIdxs : Array Nat) : IO AdbcTable := do
  match ← requery (t.query.pipe (.sel (keepCols t.nCols delIdxs t.colNames))) t.totalRows with
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

-- | Export plot data to /tmp/tc-plot.dat via DuckDB COPY (downsample in SQL)
def plotExport (t : AdbcTable) (xName yName : String) (catName? : Option String) (xIsTime : Bool) (step : Nat)
    : IO (Option (Array String)) := do
  let q := Prql.quote
  let prql := match xIsTime, catName? with
    | true,  some cn => s!"{t.query.render} | ds_time_cat {q xName} {q yName} {q cn}"
    | true,  none    => s!"{t.query.render} | ds_time {q xName} {q yName}"
    | false, some cn => s!"{t.query.render} | ds_nth_cat {q xName} {q yName} {q cn} {step}"
    | false, none    => s!"{t.query.render} | ds_nth {q xName} {q yName} {step}"
  Log.write "plot-prql" prql
  let some sql ← Prql.compile prql | return none
  let sql' := sql.trimAscii.toString
  let sql' := if sql'.endsWith ";" then (sql'.take (sql'.length - 1)).toString else sql'
  let copySql := s!"COPY ({sql'}) TO '/tmp/tc-plot.dat' (FORMAT CSV, DELIMITER '\\t', HEADER false)"
  Log.write "plot-sql" copySql
  try
    let _ ← Adbc.query copySql
  catch e =>
    Log.write "plot" s!"COPY failed: {e.toString}"
    return none
  -- get distinct categories if needed
  match catName? with
  | some cn =>
    let catPrql := s!"{t.query.render} | group \{{q cn}} (take 1) | select \{{q cn}}"
    let some catSql ← Prql.compile catPrql | return some #[]
    let catQr ← Adbc.query catSql
    let nr ← Adbc.nrows catQr
    let mut cats : Array String := #[]
    for i in [:nr.toNat] do
      cats := cats.push (← Adbc.cellStr catQr i.toUInt64 0)
    return some cats
  | none => return some #[]

end AdbcTable

-- NOTE: ReadTable/ModifyTable/RenderTable instances for AdbcTable are defined in Table variants
-- (Table.lean, Table/DuckDB.lean) which import queryMeta from ADBC/Meta.lean

namespace AdbcTable

-- | Freq: use SQL GROUP BY, query total distinct count
def queryFreq (t : AdbcTable) (colNames : Array String) : IO FreqResult := do
  if colNames.isEmpty then return emptyFreq
  let cols := colNames.map Prql.quote |> (", ".intercalate ·.toList)
  -- query total distinct groups using cntdist function
  let cntPrql := s!"{t.query.render} | cntdist \{{cols}}"
  Log.write "prql-cntdist" cntPrql
  let totalGroups ← do
    let some sql ← Prql.compile cntPrql | pure 0
    let qr ← Adbc.query sql
    let v ← Adbc.cellStr qr 0 0
    pure (v.toNat?.getD 0)
  -- limit to top 1000 by count (cell-by-cell fetch is O(rows × cols))
  let prql := s!"{t.query.render} | group \{{cols}} (aggregate \{Cnt = std.count this}) | sort \{-Cnt} | take 1000"
  Log.write "prql-freq" prql
  let some sql ← Prql.compile prql | return emptyFreq
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  -- build key columns using sequential indices (freq result: key cols 0..n-1, then Cnt)
  let mut keyCols : Array Column := #[]
  for idx in [:colNames.size] do
    let mut vals : Array String := #[]
    for r in [:nr.toNat] do
      vals := vals.push (← Adbc.cellStr qr r.toUInt64 idx.toUInt64)
    keyCols := keyCols.push (Column.strs vals)
  let cntColIdx := colNames.size  -- Cnt is the column after all key columns
  let mut cntData : Array Int64 := #[]
  for r in [:nr.toNat] do
    let v ← Adbc.cellStr qr r.toUInt64 cntColIdx.toUInt64
    cntData := cntData.push (v.toInt?.getD 0).toInt64
  let fs := freqStats cntData
  let pctData := fs.val.1
  let barData := fs.val.2
  let hData : cntData.size = pctData.size ∧ pctData.size = barData.size :=
    ⟨fs.property.1.symm, by rw [fs.property.1]; exact fs.property.2.symm⟩
  if hKeys : colNames.size = keyCols.size then
    pure ⟨colNames, keyCols, cntData, pctData, barData, totalGroups, hKeys, hData⟩
  else return emptyFreq

-- | Filter: requery with filter (queries new filtered count)
def filter (t : AdbcTable) (expr : String) : IO (Option AdbcTable) := do
  let q := t.query.filter expr
  let total ← queryCount q
  AdbcTable.requery q total

-- | Distinct: use SQL DISTINCT
def distinct (t : AdbcTable) (col : Nat) : IO (Array String) := do
  let colName := t.colNames.getD col ""
  let prql := s!"{t.query.render} | group \{{Prql.quote colName}} (take 1) | select \{{Prql.quote colName}}"
  Log.write "prql" prql
  let some sql ← Prql.compile prql | return #[]
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  let mut result : Array String := #[]
  for i in [:nr.toNat] do
    result := result.push (← Adbc.cellStr qr i.toUInt64 0)
  pure result

-- | Find row from starting position, forward or backward (with wrap)
def findRow (t : AdbcTable) (col : Nat) (val : String) (start : Nat) (fwd : Bool) : IO (Option Nat) := do
  let n := t.nRows
  if fwd then
    for i in [start:n] do
      let c ← t.getCol col i (i + 1)
      if (c.get 0).toRaw == val then return some i
    for i in [:start] do
      let c ← t.getCol col i (i + 1)
      if (c.get 0).toRaw == val then return some i
  else
    for i in [:start] do
      let idx := start - 1 - i
      let c ← t.getCol col idx (idx + 1)
      if (c.get 0).toRaw == val then return some idx
    for i in [:n - start] do
      let idx := n - 1 - i
      let c ← t.getCol col idx (idx + 1)
      if (c.get 0).toRaw == val then return some idx
  return none

end AdbcTable

-- | ExecOp instance for AdbcTable (all ops via PRQL requery)
instance : ExecOp AdbcTable where
  exec t op := AdbcTable.requery (t.query.pipe op) t.totalRows

end Tc
