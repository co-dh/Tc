/-
  Transpose: swap rows and columns via DuckDB UNPIVOT+PIVOT.
  Useful for wide tables with few rows. Capped at 200 rows (each becomes a column).
-/
import Tc.View
import Tc.Data.ADBC.Ops

namespace Tc.Transpose

-- | Max rows to transpose (each original row becomes a column)
private def maxRows : Nat := 200

-- | Build UNPIVOT+PIVOT SQL for transposing a table.
--   All columns are cast to VARCHAR since mixed types collapse into one column per original row.
--   Preserves original column order via _ord (not alphabetical).
private def transposeSql (baseSql : String) (colNames : Array String) (nRows : Nat) : String :=
  let n := min nRows maxRows
  let qid := AdbcTable.quoteId
  let castCols := colNames.map (fun c => s!"CAST({qid c} AS VARCHAR) AS {qid c}") |>.toList |> ", ".intercalate
  let unpivotCols := colNames.map qid |>.toList |> ", ".intercalate
  let pivotCols := List.range n |>.map (fun i =>
    s!"MAX(CASE WHEN _rn = {i} THEN _val END) AS \"row_{i}\"") |> ", ".intercalate
  -- _ord preserves original column order (UNPIVOT emits names in declaration order,
  -- but GROUP BY would lose that without an explicit ordering column)
  s!"WITH __src AS (SELECT * FROM ({baseSql}) LIMIT {n}), " ++
  s!"__num AS (SELECT ROW_NUMBER() OVER () - 1 AS _rn, {castCols} FROM __src), " ++
  s!"__unp AS (UNPIVOT __num ON {unpivotCols} INTO NAME \"column\" VALUE _val) " ++
  s!"SELECT \"column\", {pivotCols} FROM __unp GROUP BY \"column\" " ++
  -- Preserve original column order via CASE mapping column name → position
  let ordCases := Array.range colNames.size |>.map (fun i =>
    s!"WHEN \"column\" = '{escSql (colNames.getD i "")}' THEN {i}") |>.toList |> " ".intercalate
  s!"ORDER BY CASE {ordCases} END"

-- | Push transposed view onto stack
def push (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  let t := s.tbl
  if t.colNames.isEmpty || t.nRows == 0 then return none
  let some baseSql ← Prql.compile t.query.render | return none
  let sql := transposeSql (stripSemi baseSql) t.colNames t.nRows
  let tblName ← nextTmpName "xpose"
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP TABLE {tblName} AS ({sql})"
  let some adbc ← AdbcTable.fromTmpTbl tblName | return none
  return (View.fromTbl adbc s.cur.path |>.map fun v => s.push { v with disp := "xpose" })

end Tc.Transpose
