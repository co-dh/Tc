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
  catch e => Log.write "adbc-meta" (toString e); return false

-- | Load meta from parquet cache
def loadCache (path : String) : IO (Option AdbcTable) := do
  if !(← cacheValid path) then return none
  try
    let cp := metaCachePath path
    let qr ← Adbc.query s!"SELECT * FROM read_parquet('{cp}')"
    Log.write "meta" s!"[cache] loaded {cp}"
    some <$> ofQueryResult qr
  catch e => Log.write "adbc-meta" (toString e); return none

-- | Save meta to parquet cache
def saveCache (path : String) (metaSql : String) : IO Unit := do
  let cp := metaCachePath path
  let _ ← Adbc.query s!"COPY ({metaSql}) TO '{cp}' (FORMAT PARQUET)"
  Log.write "meta" s!"[cache] saved {cp}"

-- | Arrow format char → type name (fallback for cache compat)
private def fmtToType : Char → String
  | 'l' => "i64" | 'i' => "i32" | 's' => "i16" | 'c' => "i8"
  | 'L' => "u64" | 'I' => "u32" | 'S' => "u16" | 'C' => "u8"
  | 'g' => "f64" | 'f' => "f32" | 'd' => "dec"
  | 'u' | 'U' => "str" | 'b' => "bool"
  | _ => "?"

-- | Build PRQL colstat call for one column
private def colstatPrql (base colName colType : String) : String :=
  let q := Prql.quote colName
  s!"{base} | colstat {q} \"{colName}\" \"{colType}\""

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

-- | Query meta via PRQL colstat + append (with parquet caching)
-- Returns AdbcTable wrapping the colstat query result directly.
def queryMeta (t : AdbcTable) : IO (Option AdbcTable) := do
  let cachePath := canCache t
  -- Try cache for base parquet queries
  if let some p := cachePath then
    if let some cached ← loadCache p then return some cached
  let names := t.colNames; let types := t.colTypes
  let base := t.query.base
  -- Build PRQL: first col | append (second col) | append ...
  let first := colstatPrql base (names.getD 0 "") (types.getD 0 "?")
  let rest := (Array.range (names.size - 1)).map fun i =>
    s!" | append ({colstatPrql base (names.getD (i+1) "") (types.getD (i+1) "?")})"
  let prql := first ++ rest.foldl (· ++ ·) ""
  Log.write "meta" prql
  let some sql ← Prql.compile prql | return none
  -- Save to cache for base parquet queries
  if let some p := cachePath then
    try saveCache p sql catch e => Log.write "adbc-meta" (toString e)
  let qr ← Adbc.query sql
  some <$> ofQueryResult qr

end AdbcTable
end Tc
