/-
  AdbcTable meta: column statistics via SQL UNION ALL
  Caches result to .tv.meta.parquet for parquet files.
-/
import Tc.Data.ADBC.Table
import Tc.Data.ADBC.Prql

namespace Tc
namespace AdbcTable

-- | Cache path for meta data (parquet alongside source file)
def metaCachePath (path : String) : String := path ++ ".tv.meta.parquet"

-- | Check if cache is valid (exists and newer than source)
def cacheValid (path : String) : IO Bool := do
  let cp := metaCachePath path
  try
    let srcMeta ← System.FilePath.metadata path
    let cacheMeta ← System.FilePath.metadata cp
    return decide (cacheMeta.modified.sec.toNat >= srcMeta.modified.sec.toNat)
  catch _ => return false

-- | Read string column from query result
private def colStr (qr : Adbc.QueryResult) (nr : Nat) (c : UInt64) : IO (Array String) :=
  (Array.range nr).mapM fun r => Adbc.cellStr qr r.toUInt64 c

-- | Read int column from query result
private def colInt (qr : Adbc.QueryResult) (nr : Nat) (c : UInt64) : IO (Array Int64) :=
  (Array.range nr).mapM fun r => (·.toInt64) <$> Adbc.cellInt qr r.toUInt64 c

-- | Build MetaTuple from query result (cols: name,type,cnt,dist,null%,min,max)
private def metaFromQuery (qr : Adbc.QueryResult) : IO MetaTuple := do
  let nr := (← Adbc.nrows qr).toNat
  let s := colStr qr nr; let i := colInt qr nr  -- s/i: read col as str/int
  pure (← s 0, ← s 1, ← i 2, ← i 3, ← i 4, ← s 5, ← s 6)  -- ← awaits IO

-- | Load meta from parquet cache
def loadCache (path : String) : IO (Option MetaTuple) := do
  if !(← cacheValid path) then return none
  try
    let cp := metaCachePath path
    let qr ← Adbc.query s!"SELECT * FROM read_parquet('{cp}')"
    Log.write "meta" s!"[cache] loaded {cp}"
    some <$> metaFromQuery qr
  catch _ => return none

-- | Save meta to parquet cache
def saveCache (path : String) (metaSql : String) : IO Unit := do
  let cp := metaCachePath path
  let _ ← Adbc.query s!"COPY ({metaSql}) TO '{cp}' (FORMAT PARQUET)"
  Log.write "meta" s!"[cache] saved {cp}"

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

-- | Extract source file path from PRQL base (e.g., "from `x.parquet`" → "x.parquet")
private def extractPath (base : String) : Option String :=
  let s := base.splitOn "`" |>.getD 1 ""
  if s.isEmpty then none else some s

-- | Check if cacheable: base parquet query with no ops
private def canCache (t : AdbcTable) : Option String :=
  if t.query.ops.isEmpty then
    match extractPath t.query.base with
    | some p => if p.endsWith ".parquet" then some p else none
    | none => none
  else none

-- | Query meta via SQL UNION (with parquet caching)
def queryMeta (t : AdbcTable) : IO MetaTuple := do
  let cachePath := canCache t
  -- Try cache for base parquet queries
  if let some p := cachePath then
    if let some cached ← loadCache p then return cached
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
  -- Save to cache for base parquet queries
  if let some p := cachePath then
    try saveCache p metaSql catch _ => pure ()
  let qr ← Adbc.query metaSql
  metaFromQuery qr

end AdbcTable
end Tc
