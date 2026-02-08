/-
  TblOps/ModifyTable instances for AdbcTable
  Shared by DuckDB and Full builds
-/
import Tc.Data.ADBC.Table
import Tc.Data.ADBC.Meta

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
    let c := ctx
    if c.inWidths.isEmpty then
      let cols ← (Array.range t.nCols).mapM fun i => t.getCol i 0 t.nRows
      Term.renderTable cols t.colNames t.colFmts c.inWidths c.dispIdxs
        t.nRows.toUInt64 c.nGrp.toUInt64 c.colOff.toUInt64
        0 t.nRows.toUInt64 c.curRow.toUInt64 c.curCol.toUInt64
        c.moveDir.toInt64 c.selColIdxs c.rowSels c.styles c.precAdj.toInt64 c.widthAdj.toInt64
    else
      let cols ← (Array.range t.nCols).mapM fun i => t.getCol i c.r0 c.r1
      let adjCur := c.curRow - c.r0
      let adjSel := c.rowSels.filterMap fun r =>
        if r >= c.r0 && r < c.r1 then some (r - c.r0) else none
      Term.renderTable cols t.colNames t.colFmts c.inWidths c.dispIdxs
        t.nRows.toUInt64 c.nGrp.toUInt64 c.colOff.toUInt64
        0 (c.r1 - c.r0).toUInt64 adjCur.toUInt64 c.curCol.toUInt64
        c.moveDir.toInt64 c.selColIdxs adjSel c.styles c.precAdj.toInt64 c.widthAdj.toInt64

-- | ModifyTable instance for AdbcTable
instance : ModifyTable AdbcTable where
  delCols := fun delIdxs t => AdbcTable.delCols t delIdxs
  sortBy  := fun idxs asc t => AdbcTable.sortBy t idxs asc

end Tc
