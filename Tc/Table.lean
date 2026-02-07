/-
  Unified Table type: closed sum of MemTable, AdbcTable, KdbTable
  Lowers universe from Type 1 to Type 0, enabling standard IO.
-/
import Tc.Data.Mem.Ops   -- TblOps/ModifyTable for MemTable
import Tc.Data.Mem.Freq  -- MemTable.freqRaw
import Tc.Data.ADBC.Ops  -- TblOps/ModifyTable for AdbcTable
import Tc.Data.Kdb.Ops   -- TblOps/ModifyTable/ExecOp for KdbTable

namespace Tc

-- | Unified table: mem, adbc, or kdb
inductive Table where
  | mem  : MemTable → Table
  | adbc : AdbcTable → Table
  | kdb  : KdbTable → Table

namespace Table

-- lift ops over sum type (Arthur Whitney style)
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
-- lift uniform: same function for all 3 types
@[inline] def liftU [TblOps MemTable] [TblOps AdbcTable] [TblOps KdbTable]
    (f : {α : Type} → [TblOps α] → α → IO β) : Table → IO β := liftM f f f

def asMem? : Table → Option MemTable | .mem t => some t | _ => none
instance : MemConvert MemTable Table where wrap := .mem; unwrap := asMem?

def fromFile (p : String) : IO (Option Table) := do
  (← AdbcTable.fromFile p).map .adbc |> pure
def fromUrl (u : String) : IO (Option Table) := do
  if u.startsWith "kdb://" then (← KdbTable.fromUrl u).map .kdb |> pure else pure none

instance : TblOps Table where
  nRows    := lift MemTable.nRows (·.nRows) (·.nRows)
  colNames := lift (·.names) (·.colNames) (·.colNames)
  totalRows := lift MemTable.nRows (·.totalRows) (·.totalRows)
  queryMeta := liftM MemTable.queryMeta AdbcTable.queryMeta KdbTable.queryMeta
  filter t e := liftW (MemTable.filter · e) (AdbcTable.filter · e) (KdbTable.filter · e) t
  distinct t c := liftM (MemTable.distinct · c) (AdbcTable.distinct · c) (KdbTable.distinct · c) t
  findRow t c v s f := liftM (MemTable.findRow · c v s f) (AdbcTable.findRow · c v s f) (KdbTable.findRow · c v s f) t
  render t c n f w d g o r0 r1 cr cc m s rs st pa wa := liftM
    (TblOps.render · c n f w d g o r0 r1 cr cc m s rs st pa wa)
    (TblOps.render · c n f w d g o r0 r1 cr cc m s rs st pa wa)
    (TblOps.render · c n f w d g o r0 r1 cr cc m s rs st pa wa) t
  getCols t idxs r0 r1 := liftM (TblOps.getCols · idxs r0 r1) (TblOps.getCols · idxs r0 r1) (TblOps.getCols · idxs r0 r1) t
  colType := lift (TblOps.colType ·) (TblOps.colType ·) (TblOps.colType ·)
  plotExport t x y c b n := liftM (TblOps.plotExport · x y c b n) (TblOps.plotExport · x y c b n) (TblOps.plotExport · x y c b n) t
  fetchMore := liftW (TblOps.fetchMore ·) (TblOps.fetchMore ·) (TblOps.fetchMore ·)
  fromFile := fromFile
  fromUrl  := fromUrl

instance : ModifyTable Table where
  delCols i := liftIO (ModifyTable.delCols i) (ModifyTable.delCols i) (ModifyTable.delCols i)
  sortBy i a := liftIO (ModifyTable.sortBy i a) (ModifyTable.sortBy i a) (ModifyTable.sortBy i a)

instance : ExecOp Table where
  exec t o := liftW (ExecOp.exec · o) (ExecOp.exec · o) (ExecOp.exec · o) t

-- | Freq table: returns AdbcTable + totalGroups (all backends → DuckDB temp table)
def freqTable (tbl : Table) (colNames : Array String) : IO (Option (AdbcTable × Nat)) :=
  match tbl with
  | .adbc t => AdbcTable.freqTable t colNames
  | .kdb t => do
    let (names, cols, total) ← KdbTable.freqRaw t colNames
    if names.isEmpty then return none
    pure <| (← AdbcTable.fromArrays names cols).map (·, total)
  | .mem t => do
    let (names, cols, total) ← MemTable.freqRaw t colNames
    if names.isEmpty then return none
    pure <| (← AdbcTable.fromArrays names cols).map (·, total)

-- toText: mem is pure, adbc/kdb fetch cols then format
private def dbText (names : Array String) (nc nr : Nat) (getCol : Nat → Nat → Nat → IO Column) : IO String := do
  pure (colsToText names (← (Array.range nc).mapM (getCol · 0 nr)) nr)
def toText : Table → IO String
  | .mem t => pure (MemTable.toText t)
  | .adbc t => dbText t.colNames t.nCols t.nRows t.getCol
  | .kdb t => dbText t.colNames t.nCols t.nRows t.getCol

end Table

-- | Backend lifecycle for full build
namespace Backend
def init : IO Bool := AdbcTable.init
def shutdown : IO Unit := AdbcTable.shutdown
end Backend

end Tc
