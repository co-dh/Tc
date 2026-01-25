/-
  Table variant: MemTable only (core build without DB backends)
-/
import Tc.Data.Mem.Ops  -- TblOps/ModifyTable for MemTable

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
  render tbl cols names fmts inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj :=
    match tbl with
    | .mem t => TblOps.render t cols names fmts inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj
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
