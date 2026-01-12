/-
  Table variant: MemTable only (core build without DB backends)
-/
import Tc.Data.Mem.Table
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq

namespace Tc

-- | Table type: MemTable only (no ADBC/Kdb)
inductive Table where
  | mem : MemTable → Table

namespace Table

-- | Extract MemTable
def asMem? : Table → Option MemTable
  | .mem t => some t

-- | Not DB-backed
def isAdbc : Table → Bool
  | .mem _ => false

-- | WrapMem instance (MemTable → Table)
instance : WrapMem MemTable Table where wrapMem := Table.mem

-- | HasAsMem instance (Table → MemTable)
instance : HasAsMem Table where asMem? := Table.asMem?

-- | ReadTable instance
instance : ReadTable Table where
  nRows     | .mem t => MemTable.nRows t
  colNames  | .mem t => t.names
  totalRows | .mem t => MemTable.nRows t
  isAdbc    := Table.isAdbc

-- | ModifyTable instance
instance : ModifyTable Table where
  delCols idxs | .mem t => .mem <$> ModifyTable.delCols idxs t
  sortBy idxs asc | .mem t => .mem <$> ModifyTable.sortBy idxs asc t

-- | QueryTable instance for MemTable
instance : QueryTable MemTable where
  queryMeta := MemTable.queryMeta
  queryFreq := MemTable.queryFreq
  filter    := MemTable.filter
  distinct  := MemTable.distinct
  findRow   := MemTable.findRow

-- | QueryTable instance for Table
instance : QueryTable Table where
  queryMeta | .mem t => MemTable.queryMeta t
  queryFreq tbl idxs := match tbl with
    | .mem t => MemTable.queryFreq t idxs
  filter tbl expr := match tbl with
    | .mem t => MemTable.filter t expr <&> (·.map .mem)
  distinct tbl col := match tbl with
    | .mem t => MemTable.distinct t col
  findRow tbl col val start fwd := match tbl with
    | .mem t => MemTable.findRow t col val start fwd

-- | RenderTable instance
instance : RenderTable Table where
  render nav inWidths colOff r0 r1 moveDir st precAdj widthAdj := match nav.tbl with
    | .mem t =>
      Term.renderTable t.cols t.names #[] inWidths nav.dispColIdxs
        (MemTable.nRows t).toUInt64 nav.grp.size.toUInt64 colOff.toUInt64
        r0.toUInt64 r1.toUInt64 nav.row.cur.val.toUInt64 nav.curColIdx.toUInt64
        moveDir.toInt64 nav.selColIdxs nav.row.sels st precAdj.toInt64 widthAdj.toInt64

-- | ExecOp instance for Table
instance : ExecOp Table where
  exec tbl op := match tbl with
    | .mem t => ExecOp.exec t op <&> (·.map .mem)

-- | Format table as plain text
def toText : Table → IO String
  | .mem t => pure (MemTable.toText t)

-- | Load from file (CSV only in core build)
def fromFile (path : String) : IO (Option Table) := do
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .ok t => pure (some (.mem t))
    | .error e => Log.error s!"CSV parse error: {e}"; pure none
  else Log.error s!"tc-core: only CSV supported, not {path}"; pure none

-- | Load from URL (not supported in core)
def fromUrl (url : String) : IO (Option Table) := do
  Log.error s!"tc-core: URL loading not supported: {url}"; pure none

-- | LoadTable instance (file loading)
instance : LoadTable Table where fromFile := Table.fromFile

end Table

-- | Backend lifecycle for core build (no-op)
namespace Backend
def init : IO Bool := pure true
def shutdown : IO Unit := pure ()
end Backend

end Tc
