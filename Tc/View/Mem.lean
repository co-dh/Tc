/-
  View for MemTable only (tc-core build)
  No ADBC/Kdb imports.
-/
import Tc.View.Generic
import Tc.Data.Mem.Table
import Tc.Data.Mem.Text
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq

namespace Tc

-- | QueryTable instance for MemTable (needs Meta/Freq imports)
instance : QueryTable MemTable where
  queryMeta := MemTable.queryMeta
  queryFreq := MemTable.queryFreq
  filter    := MemTable.filter
  distinct  := MemTable.distinct
  findRow   := MemTable.findRow

-- | View for MemTable
abbrev View := GView MemTable
abbrev ViewStack := GViewStack MemTable

namespace View

-- | Load from CSV or TXT file
def fromFile (path : String) : IO (Option View) := do
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .ok tbl => pure (GView.fromTbl tbl path)
    | .error _ => pure none
  else if path.endsWith ".txt" then
    match MemTable.fromText (← IO.FS.readFile path) with
    | .ok tbl => pure (GView.fromTbl tbl path)
    | .error _ => pure none
  else pure none

end View

end Tc
