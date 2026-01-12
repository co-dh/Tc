/-
  Table variant: MemTable + AdbcTable (DuckDB build, no Kdb)
-/
import Tc.Data.Mem.Table
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq
import Tc.Data.ADBC.Table
import Tc.Data.ADBC.Meta

namespace Tc

-- | Table type: mem or adbc (no kdb)
inductive Table where
  | mem  : MemTable → Table
  | adbc : AdbcTable → Table

namespace Table

-- | Extract MemTable (for meta/freq views)
def asMem? : Table → Option MemTable
  | .mem t => some t
  | _ => none

-- | Check if DB-backed
def isAdbc : Table → Bool
  | .adbc _ => true
  | .mem _ => false

-- | WrapMem instance (MemTable → Table)
instance : WrapMem MemTable Table where wrapMem := Table.mem

-- | HasAsMem instance (Table → MemTable)
instance : HasAsMem Table where asMem? := Table.asMem?

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
  isAdbc := Table.isAdbc

-- | ModifyTable instance
instance : ModifyTable Table where
  delCols idxs
    | .mem t => .mem <$> ModifyTable.delCols idxs t
    | .adbc t => .adbc <$> ModifyTable.delCols idxs t
  sortBy idxs asc
    | .mem t => .mem <$> ModifyTable.sortBy idxs asc t
    | .adbc t => .adbc <$> ModifyTable.sortBy idxs asc t

-- | QueryTable instance for MemTable
instance : QueryTable MemTable where
  queryMeta := MemTable.queryMeta
  queryFreq := MemTable.queryFreq
  filter    := MemTable.filter
  distinct  := MemTable.distinct
  findRow   := MemTable.findRow

-- | QueryTable instance for AdbcTable
instance : QueryTable AdbcTable where
  queryMeta := AdbcTable.queryMeta
  queryFreq := AdbcTable.queryFreq
  filter    := AdbcTable.filter
  distinct  := AdbcTable.distinct
  findRow   := AdbcTable.findRow

-- | QueryTable instance for Table
instance : QueryTable Table where
  queryMeta
    | .mem t => MemTable.queryMeta t
    | .adbc t => AdbcTable.queryMeta t
  queryFreq tbl idxs := match tbl with
    | .mem t => MemTable.queryFreq t idxs
    | .adbc t => AdbcTable.queryFreq t idxs
  filter tbl expr := match tbl with
    | .mem t => MemTable.filter t expr <&> (·.map .mem)
    | .adbc t => AdbcTable.filter t expr <&> (·.map .adbc)
  distinct tbl col := match tbl with
    | .mem t => MemTable.distinct t col
    | .adbc t => AdbcTable.distinct t col
  findRow tbl col val start fwd := match tbl with
    | .mem t => MemTable.findRow t col val start fwd
    | .adbc t => AdbcTable.findRow t col val start fwd

-- | RenderTable instance
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

-- | ExecOp instance for Table
instance : ExecOp Table where
  exec tbl op := match tbl with
    | .mem t => ExecOp.exec t op <&> (·.map .mem)
    | .adbc t => ExecOp.exec t op <&> (·.map .adbc)

-- | Format table as plain text
def toText : Table → IO String
  | .mem t => pure (MemTable.toText t)
  | .adbc t => do
    let nr := t.nRows; let nc := t.nCols
    let cols ← (Array.range nc).mapM fun c => t.getCol c 0 nr
    let mut lines : Array String := #["\t".intercalate t.colNames.toList]
    for r in [:nr] do
      let row := cols.map fun col => (col.get r).toRaw
      lines := lines.push ("\t".intercalate row.toList)
    pure ("\n".intercalate lines.toList)

-- | Load from file (CSV via Mem, parquet via ADBC)
def fromFile (path : String) : IO (Option Table) := do
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .ok t => pure (some (.mem t))
    | .error _ => pure none
  else
    match ← AdbcTable.fromFile path with
    | some t => pure (some (.adbc t))
    | none => pure none

-- | Load from URL (kdb not supported in DuckDB build)
def fromUrl (url : String) : IO (Option Table) := do
  Log.error s!"tc-duckdb: kdb:// not supported: {url}"; pure none

-- | LoadTable instance (file loading)
instance : LoadTable Table where fromFile := Table.fromFile

end Table

-- | Backend lifecycle for DuckDB build
namespace Backend
def init : IO Bool := AdbcTable.init
def shutdown : IO Unit := AdbcTable.shutdown
end Backend

end Tc
