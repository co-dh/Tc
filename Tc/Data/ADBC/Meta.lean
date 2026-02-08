/-
  AdbcTable meta: column statistics from parquet metadata or SQL aggregation.
  Parquet: instant via parquet_metadata() (no data scan).
  Non-parquet: raw SQL UNION ALL per column.
  Result stored as DuckDB temp table tc_meta.
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

-- | SQL: column stats from parquet file metadata (instant, no data scan)
private def parquetMetaSql (path : String) : String :=
  let p := escSql path
  "SELECT path_in_schema AS \"column\", type AS coltype, " ++
  "SUM(num_values)::BIGINT AS cnt, 0::BIGINT AS dist, " ++
  "CAST(ROUND(SUM(stats_null_count)::FLOAT / NULLIF(SUM(num_values),0) * 100) AS BIGINT) AS null_pct, " ++
  s!"MIN(stats_min_value) AS mn, MAX(stats_max_value) AS mx " ++
  s!"FROM parquet_metadata('{p}') " ++
  "WHERE path_in_schema != '' " ++
  "GROUP BY path_in_schema, type ORDER BY MIN(column_id)"

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
--   Creates temp table tc_meta for PRQL-based selection queries.
def queryMeta (t : AdbcTable) : IO (Option AdbcTable) := do
  let names := t.colNames; let types := t.colTypes
  if names.isEmpty then return none
  let parquetPath := (extractPath t.query.base).filter (·.endsWith ".parquet")
  let metaSql ← match parquetPath with
    | some p => pure (parquetMetaSql p)
    | none => do
      let some baseSql ← Prql.compile t.query.base | return none
      pure (colStatsSql (baseSql.trimAsciiEnd).toString names types)
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP TABLE tc_meta AS ({metaSql})"
  let qr ← Adbc.query "SELECT * FROM tc_meta"
  some <$> ofQueryResult qr { base := "from tc_meta" }

end AdbcTable
end Tc
