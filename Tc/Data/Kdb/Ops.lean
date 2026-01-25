/-
  TblOps/ModifyTable/ExecOp instances for KdbTable
  Used by Full build only
-/
import Tc.Data.Kdb.Table

namespace Tc

-- | TblOps instance for KdbTable
instance : TblOps KdbTable where
  nRows     := (·.nRows)
  colNames  := (·.colNames)
  totalRows := (·.totalRows)
  isAdbc    := fun _ => true
  queryMeta := KdbTable.queryMeta
  queryFreq := KdbTable.queryFreq
  filter    := KdbTable.filter
  distinct  := KdbTable.distinct
  findRow   := KdbTable.findRow
  render t _ _ _ inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj := do
    let r1' := min r1 (r0 + maxRenderRows)
    let cols ← (Array.range t.nCols).mapM fun c => t.getCol c r0 r1'
    let adjCur := curRow - r0
    let adjSel := rowSels.filterMap fun r =>
      if r >= r0 && r < r1' then some (r - r0) else none
    Term.renderTable cols t.colNames t.colTypes inWidths dispIdxs
      t.nRows.toUInt64 nGrp.toUInt64 colOff.toUInt64
      0 (r1' - r0).toUInt64 adjCur.toUInt64 curCol.toUInt64
      moveDir.toInt64 selColIdxs adjSel st precAdj.toInt64 widthAdj.toInt64

-- | ModifyTable instance for KdbTable
instance : ModifyTable KdbTable where
  delCols := fun delIdxs t => KdbTable.delCols t delIdxs
  sortBy  := fun idxs asc t => KdbTable.sortBy t idxs asc

-- | ExecOp instance for KdbTable
instance : ExecOp KdbTable where
  exec t op := KdbTable.requery (t.query.pipe op) t.totalRows

end Tc
