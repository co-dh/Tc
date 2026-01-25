/-
  Unified Table type: closed sum of MemTable, AdbcTable, KdbTable
  Lowers universe from Type 1 to Type 0, enabling standard IO.
-/
import Tc.Data.Mem.Ops   -- TblOps/ModifyTable for MemTable
import Tc.Data.ADBC.Ops  -- TblOps/ModifyTable for AdbcTable
import Tc.Data.Kdb.Ops   -- TblOps/ModifyTable/ExecOp for KdbTable

namespace Tc

-- | Unified table: mem, adbc, or kdb
inductive Table where
  | mem  : MemTable → Table
  | adbc : AdbcTable → Table
  | kdb  : KdbTable → Table

namespace Table

-- lift pure/IO/filter ops over sum type (Arthur Whitney style)
@[inline] def lift (m : MemTable → α) (a : AdbcTable → α) (k : KdbTable → α) : Table → α
  | .mem t => m t | .adbc t => a t | .kdb t => k t
@[inline] def liftM (m : MemTable → IO α) (a : AdbcTable → IO α) (k : KdbTable → IO α) : Table → IO α
  | .mem t => m t | .adbc t => a t | .kdb t => k t
@[inline] def liftW (m : MemTable → IO (Option MemTable)) (a : AdbcTable → IO (Option AdbcTable))
    (k : KdbTable → IO (Option KdbTable)) : Table → IO (Option Table)
  | .mem t => m t <&> (·.map .mem) | .adbc t => a t <&> (·.map .adbc) | .kdb t => k t <&> (·.map .kdb)
@[inline] def liftIO (m : MemTable → IO MemTable) (a : AdbcTable → IO AdbcTable)
    (k : KdbTable → IO KdbTable) : Table → IO Table
  | .mem t => .mem <$> m t | .adbc t => .adbc <$> a t | .kdb t => .kdb <$> k t

def asMem? : Table → Option MemTable | .mem t => some t | _ => none
def isAdbc : Table → Bool | .mem _ => false | _ => true

instance : MemConvert MemTable Table where wrap := .mem; unwrap := asMem?

def fromFile (p : String) : IO (Option Table) := do
  if p.endsWith ".csv" then (← MemTable.load p).toOption.map .mem |> pure
  else (← AdbcTable.fromFile p).map .adbc |> pure
def fromUrl (u : String) : IO (Option Table) := do
  if u.startsWith "kdb://" then (← KdbTable.fromUrl u).map .kdb |> pure else pure none

instance : TblOps Table where
  nRows    := lift MemTable.nRows (·.nRows) (·.nRows)
  colNames := lift (·.names) (·.colNames) (·.colNames)
  totalRows := lift MemTable.nRows (·.totalRows) (·.totalRows)
  isAdbc   := isAdbc
  queryMeta := liftM MemTable.queryMeta AdbcTable.queryMeta KdbTable.queryMeta
  queryFreq t i := liftM (MemTable.queryFreq · i) (AdbcTable.queryFreq · i) (KdbTable.queryFreq · i) t
  filter t e := liftW (MemTable.filter · e) (AdbcTable.filter · e) (KdbTable.filter · e) t
  distinct t c := liftM (MemTable.distinct · c) (AdbcTable.distinct · c) (KdbTable.distinct · c) t
  findRow t c v s f := liftM (MemTable.findRow · c v s f) (AdbcTable.findRow · c v s f) (KdbTable.findRow · c v s f) t
  render t c n f w d g o r0 r1 cr cc m s rs st pa wa := liftM
    (TblOps.render · c n f w d g o r0 r1 cr cc m s rs st pa wa)
    (TblOps.render · c n f w d g o r0 r1 cr cc m s rs st pa wa)
    (TblOps.render · c n f w d g o r0 r1 cr cc m s rs st pa wa) t
  fromFile := fromFile

instance : ModifyTable Table where
  delCols i := liftIO (ModifyTable.delCols i) (ModifyTable.delCols i) (ModifyTable.delCols i)
  sortBy i a := liftIO (ModifyTable.sortBy i a) (ModifyTable.sortBy i a) (ModifyTable.sortBy i a)

instance : ExecOp Table where
  exec t o := liftW (ExecOp.exec · o) (ExecOp.exec · o) (ExecOp.exec · o) t

def toText : Table → IO String
  | .mem t => pure (MemTable.toText t)
  | .adbc t => do pure (colsToText t.colNames (← (Array.range t.nCols).mapM (t.getCol · 0 t.nRows)) t.nRows)
  | .kdb t => do pure (colsToText t.colNames (← (Array.range t.nCols).mapM (t.getCol · 0 t.nRows)) t.nRows)

end Table

-- | Backend lifecycle for full build
namespace Backend
def init : IO Bool := AdbcTable.init
def shutdown : IO Unit := AdbcTable.shutdown
end Backend

end Tc
