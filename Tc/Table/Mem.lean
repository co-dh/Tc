/-
  Table variant: MemTable only (core build without DB backends)
-/
import Tc.Data.Mem.Table
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq

namespace Tc

-- | TblOps instance for MemTable (read + query + render)
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
  delCols := fun delIdxs t => pure
    { names := let keepIdxs := (Array.range t.names.size).filter (!delIdxs.contains ·)
               keepIdxs.map fun i => t.names.getD i ""
      cols  := let keepIdxs := (Array.range t.names.size).filter (!delIdxs.contains ·)
               keepIdxs.map fun i => t.cols.getD i default }
  sortBy := fun idxs asc t => pure (MemTable.sort t idxs asc)

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

-- | MemConvert instance (MemTable ↔ Table)
instance : MemConvert MemTable Table where
  wrap   := Table.mem
  unwrap := Table.asMem?

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

-- | TblOps instance (includes query ops + render)
instance : TblOps Table where
  nRows     | .mem t => MemTable.nRows t
  colNames  | .mem t => t.names
  totalRows | .mem t => MemTable.nRows t
  isAdbc    := Table.isAdbc
  queryMeta | .mem t => MemTable.queryMeta t
  queryFreq tbl idxs := match tbl with
    | .mem t => MemTable.queryFreq t idxs
  filter tbl expr := match tbl with
    | .mem t => MemTable.filter t expr <&> (·.map .mem)
  distinct tbl col := match tbl with
    | .mem t => MemTable.distinct t col
  findRow tbl col val start fwd := match tbl with
    | .mem t => MemTable.findRow t col val start fwd
  render tbl _ _ _ inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj :=
    match tbl with
    | .mem t =>
      Term.renderTable t.cols t.names #[] inWidths dispIdxs
        (MemTable.nRows t).toUInt64 nGrp.toUInt64 colOff.toUInt64
        r0.toUInt64 r1.toUInt64 curRow.toUInt64 curCol.toUInt64
        moveDir.toInt64 selColIdxs rowSels st precAdj.toInt64 widthAdj.toInt64
  fromFile := Table.fromFile

-- | ModifyTable instance
instance : ModifyTable Table where
  delCols idxs | .mem t => .mem <$> ModifyTable.delCols idxs t
  sortBy idxs asc | .mem t => .mem <$> ModifyTable.sortBy idxs asc t

-- | ExecOp instance for Table
instance : ExecOp Table where
  exec tbl op := match tbl with
    | .mem t => ExecOp.exec t op <&> (·.map .mem)

-- | Format table as plain text
def toText : Table → IO String
  | .mem t => pure (MemTable.toText t)

end Table

-- | Backend lifecycle for core build (no-op)
namespace Backend
def init : IO Bool := pure true
def shutdown : IO Unit := pure ()
end Backend

end Tc
