/-
  AdbcTable meta: column statistics via SQL UNION ALL
-/
import Tc.Data.ADBC.Table
import Tc.Data.ADBC.Prql

namespace Tc
namespace AdbcTable

-- | Arrow format char → type name
private def fmtToType : Char → String
  | 'l' => "i64" | 'i' => "i32" | 's' => "i16" | 'c' => "i8"
  | 'L' => "u64" | 'I' => "u32" | 'S' => "u16" | 'C' => "u8"
  | 'g' => "f64" | 'f' => "f32" | 'd' => "dec"
  | 'u' | 'U' => "str" | 'b' => "bool"
  | _ => "?"

-- | Quote column name for SQL
private def quoteCol (c : String) : String := "\"" ++ c.replace "\"" "\"\"" ++ "\""

-- | Build SQL for one column's stats
private def colStatsSql (colName colType : String) : String :=
  let q := quoteCol colName
  s!"SELECT '{colName}' AS \"column\", '{colType}' AS type, " ++
  s!"CAST(COUNT({q}) AS BIGINT) AS cnt, " ++
  s!"CAST(COUNT(DISTINCT {q}) AS BIGINT) AS dist, " ++
  s!"CAST(ROUND((1.0 - CAST(COUNT({q}) AS DOUBLE) / NULLIF(COUNT(*), 0)) * 100) AS BIGINT) AS \"null%\", " ++
  s!"CAST(MIN({q}) AS VARCHAR) AS min, CAST(MAX({q}) AS VARCHAR) AS max"

-- | Query meta via SQL UNION
def queryMeta (t : AdbcTable) : IO MetaTuple := do
  let names := t.colNames
  let types := t.colFmts.map fmtToType
  let unions := (Array.range names.size).map fun i =>
    colStatsSql (names.getD i "") (types.getD i "?")
  -- Get FROM clause from base query
  let some sql ← Prql.compile (t.query.render ++ " | take 1") | return (#[], #[], #[], #[], #[], #[], #[])
  let sql' := sql.replace "\n" " " |>.replace "  " " "
  let parts := sql'.splitOn "FROM "
  let rest := parts.getD 1 ""
  let tbl := ((rest.splitOn " WHERE").head?.getD rest).splitOn " ORDER"
             |>.head?.getD rest |>.splitOn " LIMIT" |>.head?.getD rest
  let metaSql := unions.map (· ++ " FROM " ++ tbl) |>.toList |> String.intercalate " UNION ALL "
  Log.write "meta" metaSql
  let qr ← Adbc.query metaSql
  let nr ← Adbc.nrows qr
  let mut rNames : Array String := #[]
  let mut rTypes : Array String := #[]
  let mut rCnts : Array Int64 := #[]
  let mut rDists : Array Int64 := #[]
  let mut rNulls : Array Int64 := #[]
  let mut rMins : Array String := #[]
  let mut rMaxs : Array String := #[]
  for r in [:nr.toNat] do
    rNames := rNames.push (← Adbc.cellStr qr r.toUInt64 0)
    rTypes := rTypes.push (← Adbc.cellStr qr r.toUInt64 1)
    rCnts := rCnts.push (← Adbc.cellInt qr r.toUInt64 2).toInt64
    rDists := rDists.push (← Adbc.cellInt qr r.toUInt64 3).toInt64
    rNulls := rNulls.push (← Adbc.cellInt qr r.toUInt64 4).toInt64
    rMins := rMins.push (← Adbc.cellStr qr r.toUInt64 5)
    rMaxs := rMaxs.push (← Adbc.cellStr qr r.toUInt64 6)
  pure (rNames, rTypes, rCnts, rDists, rNulls, rMins, rMaxs)

end AdbcTable
end Tc
