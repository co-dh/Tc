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
  queryFreq := AdbcTable.queryFreq
  filter    := AdbcTable.filter
  distinct  := AdbcTable.distinct
  findRow   := AdbcTable.findRow
  getCols t idxs r0 r1 := idxs.mapM fun i => t.getCol i r0 r1
  colType t col := t.colTypes.getD col "?"
  plotExport := AdbcTable.plotExport
  fetchMore := AdbcTable.fetchMore
  render t _ _ _ inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj := do
    if inWidths.isEmpty then
      let cols ← (Array.range t.nCols).mapM fun c => t.getCol c 0 t.nRows
      Term.renderTable cols t.colNames t.colFmts inWidths dispIdxs
        t.nRows.toUInt64 nGrp.toUInt64 colOff.toUInt64
        0 t.nRows.toUInt64 curRow.toUInt64 curCol.toUInt64
        moveDir.toInt64 selColIdxs rowSels st precAdj.toInt64 widthAdj.toInt64
    else
      let cols ← (Array.range t.nCols).mapM fun c => t.getCol c r0 r1
      let adjCur := curRow - r0
      let adjSel := rowSels.filterMap fun r =>
        if r >= r0 && r < r1 then some (r - r0) else none
      Term.renderTable cols t.colNames t.colFmts inWidths dispIdxs
        t.nRows.toUInt64 nGrp.toUInt64 colOff.toUInt64
        0 (r1 - r0).toUInt64 adjCur.toUInt64 curCol.toUInt64
        moveDir.toInt64 selColIdxs adjSel st precAdj.toInt64 widthAdj.toInt64

-- | ModifyTable instance for AdbcTable
instance : ModifyTable AdbcTable where
  delCols := fun delIdxs t => AdbcTable.delCols t delIdxs
  sortBy  := fun idxs asc t => AdbcTable.sortBy t idxs asc

end Tc
