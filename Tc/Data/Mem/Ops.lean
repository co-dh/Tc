/-
  TblOps/ModifyTable instances for MemTable
  Shared by all Table variants (Mem, DuckDB, Full)
-/
import Tc.Data.Mem.Table
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq

namespace Tc

-- | TblOps instance for MemTable
instance : TblOps MemTable where
  nRows     := MemTable.nRows
  colNames  := (·.names)
  queryMeta := MemTable.queryMeta
  queryFreq := MemTable.queryFreq
  filter    := MemTable.filter
  distinct  := MemTable.distinct
  findRow   := MemTable.findRow
  render t cols names _ inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj :=
    let c := if cols.isEmpty then t.cols else cols
    let n := if names.isEmpty then t.names else names
    Term.renderTable c n #[] inWidths dispIdxs
      (MemTable.nRows t).toUInt64 nGrp.toUInt64 colOff.toUInt64
      r0.toUInt64 r1.toUInt64 curRow.toUInt64 curCol.toUInt64
      moveDir.toInt64 selColIdxs rowSels st precAdj.toInt64 widthAdj.toInt64

-- | ModifyTable instance for MemTable
instance : ModifyTable MemTable where
  delCols := fun delIdxs t => pure <|
    let keepIdxs := (Array.range t.names.size).filter (!delIdxs.contains ·)
    { names := keepIdxs.map fun i => t.names.getD i ""
      cols  := keepIdxs.map fun i => t.cols.getD i default
      h_eq  := by simp [Array.size_map] }
  sortBy := fun idxs asc t => pure (MemTable.sort t idxs asc)

end Tc
