/-
  Table variant: MemTable + AdbcTable (DuckDB build, no Kdb)
-/
import Tc.Data.Mem.Ops   -- TblOps/ModifyTable for MemTable
import Tc.Data.ADBC.Ops  -- TblOps/ModifyTable for AdbcTable

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

-- | MemConvert instance (MemTable ↔ Table)
instance : MemConvert MemTable Table where
  wrap   := Table.mem
  unwrap := Table.asMem?

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

-- | TblOps instance (includes query ops + render)
instance : TblOps Table where
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
  render tbl cols names fmts inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj :=
    match tbl with
    | .mem t => TblOps.render t cols names fmts inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj
    | .adbc t => TblOps.render t cols names fmts inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj
  fromFile := Table.fromFile

-- | ModifyTable instance
instance : ModifyTable Table where
  delCols idxs
    | .mem t => .mem <$> ModifyTable.delCols idxs t
    | .adbc t => .adbc <$> ModifyTable.delCols idxs t
  sortBy idxs asc
    | .mem t => .mem <$> ModifyTable.sortBy idxs asc t
    | .adbc t => .adbc <$> ModifyTable.sortBy idxs asc t

-- | ExecOp instance for Table
instance : ExecOp Table where
  exec tbl op := match tbl with
    | .mem t => ExecOp.exec t op <&> (·.map .mem)
    | .adbc t => ExecOp.exec t op <&> (·.map .adbc)

-- | Format table as plain text
def toText : Table → IO String
  | .mem t => pure (MemTable.toText t)
  | .adbc t => do pure (colsToText t.colNames (← (Array.range t.nCols).mapM (t.getCol · 0 t.nRows)) t.nRows)

end Table

-- | Backend lifecycle for DuckDB build
namespace Backend
def init : IO Bool := AdbcTable.init
def shutdown : IO Unit := AdbcTable.shutdown
end Backend

end Tc
