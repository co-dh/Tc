/-
  TblOps/ModifyTable instances for AdbcTable
  Includes meta (column statistics) functionality.
-/
import Tc.Data.ADBC.Table
import Tc.Data.ADBC.Prql
import Tc.Render

namespace Tc

-- | TblOps instance for AdbcTable
instance : TblOps AdbcTable where
  nRows     := (·.nRows)
  colNames  := (·.colNames)
  totalRows := (·.totalRows)
  filter    := AdbcTable.filter
  distinct  := AdbcTable.distinct
  findRow   := AdbcTable.findRow
  getCols t idxs r0 r1 := idxs.mapM fun i => t.getCol i r0 r1
  colType t col := t.colTypes.getD col "?"
  plotExport := AdbcTable.plotExport
  fetchMore := AdbcTable.fetchMore
  fromFile := AdbcTable.fromFile
  render t ctx := do
    let cols ← (Array.range t.colNames.size).mapM fun i => t.getCol i ctx.r0 ctx.r1
    renderCols cols t.colNames t.colFmts t.nRows ctx ctx.r0 (ctx.r1 - ctx.r0)

-- | ModifyTable instance for AdbcTable
instance : ModifyTable AdbcTable where
  delCols := fun delIdxs t => AdbcTable.delCols t delIdxs
  sortBy  := fun idxs asc t => AdbcTable.sortBy t idxs asc

-- | Format table as plain text
def AdbcTable.toText (t : AdbcTable) : IO String := do
  pure (colsToText t.colNames (← (Array.range t.colNames.size).mapM (t.getCol · 0 t.nRows)) t.nRows)

/-! ## Meta: column statistics from parquet metadata or SQL aggregation -/

namespace AdbcTable

-- | Double-quote identifier for DuckDB
private def quoteId (s : String) : String :=
  "\"" ++ s.replace "\"" "\"\"" ++ "\""

-- | Extract file path from PRQL base: "from `path`" → some "path"
private def extractPath (base : String) : Option String :=
  let parts := base.splitOn "`"
  if parts.length >= 3 then
    let p := parts.getD 1 ""
    if p.isEmpty then none else some p
  else none

-- | PRQL: column stats from parquet file metadata (instant, no data scan)
private def parquetMetaPrql (path : String) : String :=
  let p := escSql path
  "from s\"SELECT * FROM parquet_metadata('" ++ p ++ "')\" | pqmeta"

-- | SQL: per-column stats via UNION ALL (for non-parquet sources)
private def colStatsSql (baseSql : String) (names : Array String) (types : Array String) : String :=
  let one (i : Nat) : String :=
    let nm := escSql (names.getD i "")
    let tp := escSql (types.getD i "?")
    let q := quoteId (names.getD i "")
    s!"SELECT '{nm}' AS \"column\", '{tp}' AS coltype, " ++
    s!"CAST(COUNT({q}) AS BIGINT) AS cnt, " ++
    s!"CAST(COUNT(DISTINCT {q}) AS BIGINT) AS dist, " ++
    s!"CAST(ROUND((1.0 - COUNT({q})::FLOAT / NULLIF(COUNT(*),0)) * 100) AS BIGINT) AS null_pct, " ++
    s!"CAST(MIN({q}) AS VARCHAR) AS mn, CAST(MAX({q}) AS VARCHAR) AS mx FROM __src"
  let parts := (Array.range names.size).map one
  "WITH __src AS (" ++ baseSql ++ ") " ++ " UNION ALL ".intercalate parts.toList

-- | Query meta: parquet uses file metadata (instant), others use SQL aggregation.
def queryMeta (t : AdbcTable) : IO (Option AdbcTable) := do
  let names := t.colNames; let types := t.colTypes
  if names.isEmpty then return none
  let parquetPath := (extractPath t.query.base).filter (·.endsWith ".parquet")
  let metaSql ← match parquetPath with
    | some p => do
      let some sql ← Prql.compile (parquetMetaPrql p) | return none
      pure sql
    | none => do
      let some baseSql ← Prql.compile t.query.base | return none
      pure (colStatsSql (baseSql.trimAsciiEnd).toString names types)
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tblName := s!"tc_meta_{n}"
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP TABLE {tblName} AS ({metaSql})"
  let qr ← Adbc.query s!"SELECT * FROM {tblName}"
  some <$> ofQueryResult qr { base := s!"from {tblName}" }

-- | Query row indices matching PRQL filter on meta table
def queryMetaIndices (tblName : String) (flt : String) : IO (Array Nat) := do
  let prql := "from " ++ tblName ++ " | rowidx | filter " ++ flt ++ " | select {idx}"
  let some sql ← Prql.compile prql | return #[]
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  (Array.range nr.toNat).mapM fun r => (·.toNat) <$> Adbc.cellInt qr r.toUInt64 0

-- | Query column names from meta table at given row indices
def queryMetaColNames (tblName : String) (rows : Array Nat) : IO (Array String) := do
  if rows.isEmpty then return #[]
  let idxs := ", ".intercalate (rows.map (s!"{·}") |>.toList)
  let prql := "from " ++ tblName ++ " | rowidx | filter (idx | in [" ++ idxs ++ "]) | select {column, idx}"
  let some sql ← Prql.compile prql | return #[]
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  (Array.range nr.toNat).mapM fun r => Adbc.cellStr qr r.toUInt64 0

end AdbcTable

end Tc
