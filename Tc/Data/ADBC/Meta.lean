/-
  AdbcTable meta: column statistics from parquet metadata or SQL aggregation.
  Parquet: instant via parquet_metadata() (no data scan).
  Non-parquet: raw SQL UNION ALL per column.
  Result stored as DuckDB temp table tc_meta_N (unique per view).
-/
import Tc.Data.ADBC.Table
import Tc.Data.ADBC.Prql

namespace Tc
namespace AdbcTable

-- | Escape single quotes for SQL string literals
private def escSql (s : String) : String := s.replace "'" "''"

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
--   Uses pqmeta function from funcs.prql
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
--   Creates temp table tc_meta_N for PRQL-based selection queries.
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
  let prql := "from " ++ tblName ++ " | derive {idx = s\"(ROW_NUMBER() OVER () - 1)\"} | filter " ++ flt ++ " | select {idx}"
  let some sql ← Prql.compile prql | return #[]
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  (Array.range nr.toNat).mapM fun r => (·.toNat) <$> Adbc.cellInt qr r.toUInt64 0

-- | Query column names from meta table at given row indices
def queryMetaColNames (tblName : String) (rows : Array Nat) : IO (Array String) := do
  if rows.isEmpty then return #[]
  let idxs := ", ".intercalate (rows.map (s!"{·}") |>.toList)
  let prql := "from " ++ tblName ++ " | derive {idx = s\"(ROW_NUMBER() OVER () - 1)\"} | filter s\"idx IN (" ++ idxs ++ ")\" | select {column, idx}"
  let some sql ← Prql.compile prql | return #[]
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  (Array.range nr.toNat).mapM fun r => Adbc.cellStr qr r.toUInt64 0

end AdbcTable
end Tc
