/-
  StatusAgg: column aggregation stats (sum/avg/count) for the status bar.
  Computes via DuckDB SQL, cached per column to avoid per-frame queries.
-/
import Tc.Data.ADBC.Ops
import Tc.Term

namespace Tc.StatusAgg

-- | Cache: (path, colIdx, formatted agg string)
abbrev Cache := String × Nat × String

def Cache.empty : Cache := ("", 0, "")

-- | Aggregate stats for a single column: sum, avg, count (non-null).
-- Returns formatted string like "Σ1234 μ12.3 #100" for numeric, "#100" for non-numeric.
-- For large files, skip expensive SUM/AVG (no parquet shortcut) and show count only.
private def compute (t : AdbcTable) (colIdx : Nat) : IO String := do
  if t.totalRows > Tc.prqlLimit then return s!"#{t.totalRows}"
  let colName := t.colNames.getD colIdx ""
  let colType := t.colTypes.getD colIdx "?"
  let q := AdbcTable.quoteId colName
  let isNum := colType == "int" || colType == "float" || colType == "decimal"
  let sql ← do
    let some baseSql ← Prql.compile t.query.render | return ""
    let aggs := if isNum
      then s!"CAST(SUM({q}) AS DOUBLE) AS s, CAST(AVG({q}) AS DOUBLE) AS a, COUNT({q}) AS c"
      else s!"COUNT({q}) AS c"
    pure s!"SELECT {aggs} FROM ({(stripSemi baseSql)})"
  try
    let qr ← Adbc.query sql
    if isNum then
      let sm ← Adbc.cellStr qr 0 0
      let av ← Adbc.cellStr qr 0 1
      let ct ← Adbc.cellStr qr 0 2
      let fmt (s : String) := if s.length > 8 then (s.take 8).toString else s
      pure s!"Σ{fmt sm} μ{fmt av} #{ct}"
    else
      let ct ← Adbc.cellStr qr 0 0
      pure s!"#{ct}"
  catch _ => pure ""

-- | Render aggregation stats on the status bar at 1/3 width
private def render (agg : String) : IO Unit := do
  if agg.isEmpty then return
  let ht ← Term.height; let w ← Term.width
  let pos := w.toNat / 3
  Term.print pos.toUInt32 (ht - 1) Term.brBlack Term.default agg

-- | Update cache if column changed, then render. Returns updated cache.
def update (cache : Cache) (tbl : AdbcTable) (path : String) (colIdx : Nat) : IO Cache := do
  let (cachedPath, cachedCol, _) := cache
  let cache ← if cachedPath == path && cachedCol == colIdx then pure cache
    else do
      let agg ← try compute tbl colIdx catch _ => pure ""
      pure (path, colIdx, agg)
  let (_, _, agg) := cache
  render agg
  pure cache

end Tc.StatusAgg
