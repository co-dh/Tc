/-
  Table variant: MemTable only (core build without DB backends)
-/
import Tc.Data.Mem.Ops  -- TblOps/ModifyTable for MemTable

namespace Tc

-- | Table type: MemTable only (no ADBC/Kdb)
inductive Table where
  | mem : MemTable → Table

namespace Table

-- unwrap/wrap helpers
@[inline] def un : Table → MemTable | .mem t => t
@[inline] def wr : MemTable → Table := .mem

def asMem? : Table → Option MemTable | .mem t => some t
def isAdbc : Table → Bool | .mem _ => false

instance : MemConvert MemTable Table where wrap := wr; unwrap := asMem?

def fromFile (p : String) : IO (Option Table) := do
  if p.endsWith ".csv" then (← MemTable.load p).toOption.map wr |> pure
  else Log.error s!"tc-core: only CSV supported, not {p}"; pure none
def fromUrl (u : String) : IO (Option Table) := do Log.error s!"tc-core: URL not supported: {u}"; pure none

instance : TblOps Table where
  nRows t := MemTable.nRows t.un;  colNames t := t.un.names;  totalRows t := MemTable.nRows t.un
  isAdbc := isAdbc;  queryMeta t := MemTable.queryMeta t.un
  queryFreq t n := MemTable.queryFreq t.un n
  filter t e := MemTable.filter t.un e <&> (·.map wr)
  distinct t c := MemTable.distinct t.un c
  findRow t c v s f := MemTable.findRow t.un c v s f
  render t c n f w d g o r0 r1 cr cc md s rs st pa wa := TblOps.render t.un c n f w d g o r0 r1 cr cc md s rs st pa wa
  colType t col := TblOps.colType t.un col
  fromFile := fromFile

instance : ModifyTable Table where
  delCols i t := wr <$> ModifyTable.delCols i t.un
  sortBy i a t := wr <$> ModifyTable.sortBy i a t.un

instance : ExecOp Table where exec t o := ExecOp.exec t.un o <&> (·.map wr)

def toText (t : Table) : IO String := pure (MemTable.toText t.un)

end Table

-- | Backend lifecycle for core build (no-op)
namespace Backend
def init : IO Bool := pure true
def shutdown : IO Unit := pure ()
end Backend

end Tc
