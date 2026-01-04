/-
  Unified Table type: closed sum of MemTable and AdbcTable
  Lowers universe from Type 1 to Type 0, enabling standard IO.
-/
import Tc.Data.Mem.Table
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq
import Tc.Data.ADBC.Table
import Tc.Data.ADBC.Meta

namespace Tc

-- | Unified table: either in-memory or ADBC-backed
inductive Table where
  | mem  : MemTable → Table
  | adbc : AdbcTable → Table

namespace Table

-- | Extract MemTable (for meta/freq views which produce MemTable)
def asMem? : Table → Option MemTable
  | .mem t => some t
  | .adbc _ => none

-- | ReadTable instance
instance : ReadTable Table where
  nRows
    | .mem t => MemTable.nRows t
    | .adbc t => AdbcTable.nRows t
  colNames
    | .mem t => t.names
    | .adbc t => AdbcTable.colNames t
  totalRows
    | .mem t => MemTable.nRows t
    | .adbc t => AdbcTable.totalRows t

-- | ModifyTable instance
instance : ModifyTable Table where
  delCols idxs
    | .mem t => .mem <$> ModifyTable.delCols idxs t
    | .adbc t => .adbc <$> ModifyTable.delCols idxs t
  sortBy idxs asc
    | .mem t => .mem <$> ModifyTable.sortBy idxs asc t
    | .adbc t => .adbc <$> ModifyTable.sortBy idxs asc t

-- | QueryMeta instance
instance : QueryMeta Table where
  queryMeta
    | .mem t => QueryMeta.queryMeta t
    | .adbc t => QueryMeta.queryMeta t

-- | QueryFreq instance
instance : QueryFreq Table where
  queryFreq tbl idxs := match tbl with
    | .mem t => QueryFreq.queryFreq t idxs
    | .adbc t => QueryFreq.queryFreq t idxs

-- | QueryFilter instance
instance : QueryFilter Table where
  filter tbl expr := match tbl with
    | .mem t => QueryFilter.filter t expr <&> (·.map .mem)
    | .adbc t => QueryFilter.filter t expr <&> (·.map .adbc)

-- | QueryDistinct instance
instance : QueryDistinct Table where
  distinct tbl col := match tbl with
    | .mem t => QueryDistinct.distinct t col
    | .adbc t => QueryDistinct.distinct t col

-- | RenderTable instance (direct dispatch to Term.renderTable)
instance : RenderTable Table where
  render nav inWidths colOff r0 r1 moveDir st precAdj widthAdj := match nav.tbl with
    | .mem t =>
      Term.renderTable t.cols t.names #[] inWidths nav.dispColIdxs
        (MemTable.nRows t).toUInt64 nav.grp.size.toUInt64 colOff.toUInt64
        r0.toUInt64 r1.toUInt64 nav.row.cur.val.toUInt64 nav.curColIdx.toUInt64
        moveDir.toInt64 nav.selColIdxs nav.row.sels st precAdj.toInt64 widthAdj.toInt64
    | .adbc t => do
      if inWidths.isEmpty then
        let cols ← (Array.range t.nCols).mapM fun c => t.getCol c 0 t.nRows
        Term.renderTable cols t.colNames t.colFmts inWidths nav.dispColIdxs
          t.nRows.toUInt64 nav.grp.size.toUInt64 colOff.toUInt64
          0 t.nRows.toUInt64 nav.row.cur.val.toUInt64 nav.curColIdx.toUInt64
          moveDir.toInt64 nav.selColIdxs nav.row.sels st precAdj.toInt64 widthAdj.toInt64
      else
        let cols ← (Array.range t.nCols).mapM fun c => t.getCol c r0 r1
        let adjCur := nav.row.cur.val - r0
        let adjSel := nav.row.sels.filterMap fun r =>
          if r >= r0 && r < r1 then some (r - r0) else none
        Term.renderTable cols t.colNames t.colFmts inWidths nav.dispColIdxs
          t.nRows.toUInt64 nav.grp.size.toUInt64 colOff.toUInt64
          0 (r1 - r0).toUInt64 adjCur.toUInt64 nav.curColIdx.toUInt64
          moveDir.toInt64 nav.selColIdxs adjSel st precAdj.toInt64 widthAdj.toInt64

end Table
end Tc
