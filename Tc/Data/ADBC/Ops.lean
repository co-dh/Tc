/-
  TblOps/ModifyTable instances for AdbcTable
  Shared by DuckDB and Full builds
-/
import Tc.Data.ADBC.Table
import Tc.Data.ADBC.Meta
import Tc.Render

namespace Tc

-- | TblOps instance for AdbcTable
instance : TblOps AdbcTable where
  nRows     := (·.nRows)
  colNames  := (·.colNames)
  totalRows := (·.totalRows)
  queryMeta := AdbcTable.queryMeta
  filter    := AdbcTable.filter
  distinct  := AdbcTable.distinct
  findRow   := AdbcTable.findRow
  getCols t idxs r0 r1 := idxs.mapM fun i => t.getCol i r0 r1
  colType t col := t.colTypes.getD col "?"
  plotExport := AdbcTable.plotExport
  fetchMore := AdbcTable.fetchMore
  render t ctx := do
    if ctx.inWidths.isEmpty then
      let cols ← (Array.range t.nCols).mapM fun i => t.getCol i 0 t.nRows
      renderCols cols t.colNames t.colFmts t.nRows ctx 0 t.nRows
    else
      let cols ← (Array.range t.nCols).mapM fun i => t.getCol i ctx.r0 ctx.r1
      renderCols cols t.colNames t.colFmts t.nRows ctx ctx.r0 (ctx.r1 - ctx.r0)

-- | ModifyTable instance for AdbcTable
instance : ModifyTable AdbcTable where
  delCols := fun delIdxs t => AdbcTable.delCols t delIdxs
  sortBy  := fun idxs asc t => AdbcTable.sortBy t idxs asc

end Tc
