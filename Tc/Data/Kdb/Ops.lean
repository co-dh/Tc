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
  queryMeta := KdbTable.queryMeta
  filter    := KdbTable.filter
  distinct  := KdbTable.distinct
  findRow   := KdbTable.findRow
  getCols t idxs r0 r1 := idxs.mapM fun i => t.getCol i r0 r1
  colType t col := match t.colTypes.getD col '?' with
    | 'j' | 'i' | 'h' => "int" | 'f' | 'e' => "float"
    | 't' => "time" | 'p' | 'z' => "timestamp" | 'd' => "date"
    | _ => "str"
  render t ctx := do
    let c := ctx
    let r1' := min c.r1 (c.r0 + maxVisRows)
    let cols ← (Array.range t.nCols).mapM fun i => t.getCol i c.r0 r1'
    let adjCur := c.curRow - c.r0
    let adjSel := c.rowSels.filterMap fun r =>
      if r >= c.r0 && r < r1' then some (r - c.r0) else none
    Term.renderTable cols t.colNames t.colTypes c.inWidths c.dispIdxs
      t.nRows.toUInt64 c.nGrp.toUInt64 c.colOff.toUInt64
      0 (r1' - c.r0).toUInt64 adjCur.toUInt64 c.curCol.toUInt64
      c.moveDir.toInt64 c.selColIdxs adjSel c.styles c.precAdj.toInt64 c.widthAdj.toInt64

-- | ModifyTable instance for KdbTable
instance : ModifyTable KdbTable where
  delCols := fun delIdxs t => KdbTable.delCols t delIdxs
  sortBy  := fun idxs asc t => KdbTable.sortBy t idxs asc

-- | ExecOp instance for KdbTable
instance : ExecOp KdbTable where
  exec t op := KdbTable.requery (t.query.pipe op) t.totalRows

end Tc
