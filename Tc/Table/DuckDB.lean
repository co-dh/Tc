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

-- lift pure/IO/filter ops over sum type
@[inline] def lift (m : MemTable → α) (a : AdbcTable → α) : Table → α
  | .mem t => m t | .adbc t => a t
@[inline] def liftM (m : MemTable → IO α) (a : AdbcTable → IO α) : Table → IO α
  | .mem t => m t | .adbc t => a t
@[inline] def liftW (m : MemTable → IO (Option MemTable)) (a : AdbcTable → IO (Option AdbcTable)) : Table → IO (Option Table)
  | .mem t => m t <&> (·.map .mem) | .adbc t => a t <&> (·.map .adbc)
@[inline] def liftIO (m : MemTable → IO MemTable) (a : AdbcTable → IO AdbcTable) : Table → IO Table
  | .mem t => .mem <$> m t | .adbc t => .adbc <$> a t

def asMem? : Table → Option MemTable | .mem t => some t | _ => none
def isAdbc : Table → Bool | .mem _ => false | .adbc _ => true

instance : MemConvert MemTable Table where wrap := .mem; unwrap := asMem?

def fromFile (p : String) : IO (Option Table) := do
  if p.endsWith ".csv" then (← MemTable.load p).toOption.map .mem |> pure
  else (← AdbcTable.fromFile p).map .adbc |> pure
def fromUrl (u : String) : IO (Option Table) := do Log.error s!"tc-duckdb: kdb:// not supported: {u}"; pure none

instance : TblOps Table where
  nRows    := lift MemTable.nRows (·.nRows)
  colNames := lift (·.names) (·.colNames)
  totalRows := lift MemTable.nRows (·.totalRows)
  isAdbc   := isAdbc
  queryMeta := liftM MemTable.queryMeta AdbcTable.queryMeta
  queryFreq t i := liftM (MemTable.queryFreq · i) (AdbcTable.queryFreq · i) t
  filter t e := liftW (MemTable.filter · e) (AdbcTable.filter · e) t
  distinct t c := liftM (MemTable.distinct · c) (AdbcTable.distinct · c) t
  findRow t c v s f := liftM (MemTable.findRow · c v s f) (AdbcTable.findRow · c v s f) t
  render t c n f w d g o r0 r1 cr cc md s rs st pa wa := liftM
    (TblOps.render · c n f w d g o r0 r1 cr cc md s rs st pa wa)
    (TblOps.render · c n f w d g o r0 r1 cr cc md s rs st pa wa) t
  fromFile := fromFile
  fromUrl  := fromUrl

instance : ModifyTable Table where
  delCols i := liftIO (ModifyTable.delCols i) (ModifyTable.delCols i)
  sortBy i a := liftIO (ModifyTable.sortBy i a) (ModifyTable.sortBy i a)

instance : ExecOp Table where exec t o := liftW (ExecOp.exec · o) (ExecOp.exec · o) t

-- toText: mem is pure, adbc fetches cols then formats
def toText : Table → IO String
  | .mem t => pure (MemTable.toText t)
  | .adbc t => do colsToText t.colNames (← (Array.range t.nCols).mapM (t.getCol · 0 t.nRows)) t.nRows |> pure

end Table

-- | Backend lifecycle for DuckDB build
namespace Backend
def init : IO Bool := AdbcTable.init
def shutdown : IO Unit := AdbcTable.shutdown
end Backend

end Tc
