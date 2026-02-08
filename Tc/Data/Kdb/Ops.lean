/-
  TblOps/ModifyTable/ExecOp instances for KdbTable
  Used by Full build only
-/
import Tc.Data.Kdb.Table
import Tc.Render

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
    let r1' := min ctx.r1 (ctx.r0 + maxVisRows)
    let cols ← (Array.range t.nCols).mapM fun i => t.getCol i ctx.r0 r1'
    renderCols cols t.colNames t.colTypes t.nRows ctx ctx.r0 (r1' - ctx.r0)

-- | ModifyTable instance for KdbTable
instance : ModifyTable KdbTable where
  delCols := fun delIdxs t => KdbTable.delCols t delIdxs
  sortBy  := fun idxs asc t => KdbTable.sortBy t idxs asc

-- | ExecOp instance for KdbTable
instance : ExecOp KdbTable where
  exec t op := KdbTable.requery (t.query.pipe op) t.totalRows

end Tc
