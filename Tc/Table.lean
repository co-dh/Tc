/-
  Unified Table type: closed sum of AdbcTable, KdbTable
  Lowers universe from Type 1 to Type 0, enabling standard IO.
-/
import Tc.Data.ADBC.Ops  -- TblOps/ModifyTable for AdbcTable
import Tc.Data.Kdb.Ops   -- TblOps/ModifyTable for KdbTable

namespace Tc

-- | Unified table: adbc or kdb
inductive Table where
  | adbc : AdbcTable → Table
  | kdb  : KdbTable → Table

namespace Table

-- lift ops over sum type
@[inline] def liftW (a : AdbcTable → IO (Option AdbcTable))
    (k : KdbTable → IO (Option KdbTable)) : Table → IO (Option Table)
  | .adbc t => a t <&> (·.map .adbc) | .kdb t => k t <&> (·.map .kdb)
@[inline] def liftIO (a : AdbcTable → IO AdbcTable)
    (k : KdbTable → IO KdbTable) : Table → IO Table
  | .adbc t => .adbc <$> a t | .kdb t => .kdb <$> k t
-- lift when both branches use the same typeclass method
@[inline] def liftS (f : ∀ {α : Type} [TblOps α], α → β) : Table → β
  | .adbc t => f t | .kdb t => f t
@[inline] def liftSM (f : ∀ {α : Type} [TblOps α], α → IO β) : Table → IO β
  | .adbc t => f t | .kdb t => f t

def fromFile (p : String) : IO (Option Table) := do
  (← AdbcTable.fromFile p).map .adbc |> pure
def fromUrl (u : String) : IO (Option Table) := do
  if u.startsWith "kdb://" then (← KdbTable.fromUrl u).map .kdb |> pure else pure none

instance : TblOps Table where
  nRows     := liftS TblOps.nRows
  colNames  := liftS TblOps.colNames
  totalRows := liftS TblOps.totalRows
  filter t e := liftW (AdbcTable.filter · e) (KdbTable.filter · e) t
  distinct t c := liftSM (TblOps.distinct · c) t
  findRow t c v s f := liftSM (TblOps.findRow · c v s f) t
  render t ctx := liftSM (TblOps.render · ctx) t
  getCols t idxs r0 r1 := liftSM (TblOps.getCols · idxs r0 r1) t
  colType   := liftS TblOps.colType
  plotExport t x y c b n tl := liftSM (TblOps.plotExport · x y c b n tl) t
  fetchMore := liftW (TblOps.fetchMore ·) (TblOps.fetchMore ·)
  fromFile := fromFile
  fromUrl  := fromUrl

instance : ModifyTable Table where
  delCols i := liftIO (ModifyTable.delCols i) (ModifyTable.delCols i)
  sortBy i a := liftIO (ModifyTable.sortBy i a) (ModifyTable.sortBy i a)

-- | Freq table: returns AdbcTable + totalGroups (all backends → DuckDB temp table)
def freqTable (tbl : Table) (colNames : Array String) : IO (Option (AdbcTable × Nat)) :=
  match tbl with
  | .adbc t => AdbcTable.freqTable t colNames
  | .kdb t => do
    let (names, cols, total) ← KdbTable.freqRaw t colNames
    if names.isEmpty then return none
    pure <| (← AdbcTable.fromArrays names cols).map (·, total)

-- toText: fetch cols then format
private def dbText (names : Array String) (nc nr : Nat) (getCol : Nat → Nat → Nat → IO Column) : IO String := do
  pure (colsToText names (← (Array.range nc).mapM (getCol · 0 nr)) nr)
def toText : Table → IO String
  | .adbc t => dbText t.colNames t.nCols t.nRows t.getCol
  | .kdb t => dbText t.colNames t.nCols t.nRows t.getCol

end Table

-- | Backend lifecycle for full build
namespace Backend
def init : IO Bool := AdbcTable.init
def shutdown : IO Unit := AdbcTable.shutdown
end Backend

end Tc
