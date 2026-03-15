/-
  Sparkline: compute per-column distribution strings (Unicode block chars)
  for display as an extra header row in table view.
-/
import Tc.Data.ADBC.Table
import Tc.Data.ADBC.Ops
import Tc.Data.ADBC.Prql

namespace Tc.Sparkline

-- 9 levels: space + 8 Unicode block elements
private def blocks : Array Char := #[' ', '▁', '▂', '▃', '▄', '▅', '▆', '▇', '█']

-- | Compute sparkline strings for all columns via DuckDB histogram.
-- Returns one string per column (empty for non-numeric).
-- nBars = number of histogram buckets.
def compute (t : AdbcTable) (nBars : Nat := 20) : IO (Array String) := do
  let names := t.colNames; let types := t.colTypes
  let empty := names.map (fun _ => "")
  if names.isEmpty || t.nRows == 0 then return empty
  let mut parts : Array String := #[]
  let mut numIdxs : Array Nat := #[]
  for i in [:names.size] do
    let tp := types.getD i "?"
    if tp == "int" || tp == "float" || tp == "decimal" then
      let q := AdbcTable.quoteId (names.getD i "")
      -- LEAST clamps bucket to [0, nBars-1]; CASE handles constant columns
      let sql := s!"SELECT CASE WHEN mx = mn THEN {nBars / 2} " ++
        s!"ELSE LEAST(CAST(FLOOR(({q}::DOUBLE - mn) / (mx - mn + 1e-30) * {nBars}) AS INTEGER), {nBars - 1}) " ++
        s!"END AS bucket, COUNT(*) AS cnt " ++
        s!"FROM __src, (SELECT MIN({q}::DOUBLE) AS mn, MAX({q}::DOUBLE) AS mx FROM __src) " ++
        s!"WHERE {q} IS NOT NULL GROUP BY bucket ORDER BY bucket"
      parts := parts.push s!"SELECT {i} AS col_idx, bucket, cnt FROM ({sql})"
      numIdxs := numIdxs.push i
  if parts.isEmpty then return empty
  let some baseSql ← Prql.compile t.query.base
    | Log.error "sparkline: PRQL compile failed"; return empty
  let unionSql := "WITH __src AS (" ++ (baseSql.trimAsciiEnd).toString ++ ") " ++
    " UNION ALL ".intercalate parts.toList
  let qr ← try Adbc.query unionSql
    catch e => Log.error s!"sparkline: {e}"; return empty
  let nr ← Adbc.nrows qr
  -- Parse results into bucket→count arrays per column
  let mut colBuckets : Array (Array (Nat × Nat)) := numIdxs.map (fun _ => #[])
  for r in [:nr.toNat] do
    let colIdx ← Adbc.cellInt qr r.toUInt64 0
    let bucket ← Adbc.cellInt qr r.toUInt64 1
    let cnt ← Adbc.cellInt qr r.toUInt64 2
    for j in [:numIdxs.size] do
      if numIdxs.getD j 0 == colIdx.toNat then
        colBuckets := colBuckets.set! j ((colBuckets.getD j #[]).push (bucket.toNat, cnt.toNat))
  -- Build sparkline string per column
  let mut result := empty
  for j in [:numIdxs.size] do
    let buckets := colBuckets.getD j #[]
    if buckets.isEmpty then continue
    let mut counts : Array Nat := (Array.range nBars).map (fun _ => 0)
    for (b, c) in buckets do
      if b < nBars then counts := counts.set! b c
    let maxCnt := counts.foldl max 0
    let spark := if maxCnt == 0 then String.ofList (List.replicate nBars ' ')
      else String.ofList (counts.map fun c =>
        let level := (c * 8 + maxCnt - 1) / maxCnt  -- ceil: 0→0, >0→1..8
        blocks.getD level ' ').toList
    result := result.set! (numIdxs.getD j 0) spark
  pure result

end Tc.Sparkline
