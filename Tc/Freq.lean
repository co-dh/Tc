/-
  Freq view: group by columns, count, pct, bar
  Returns MemTable sorted by Cnt descending.
-/
import Tc.Data.Mem.Table

namespace Tc.Freq

-- | Convert queryFreq tuple result to MemTable (sorted by Cnt desc)
def toMemTable (f : FreqTuple) : MemTable :=
  let (keyNames, keyCols, cntData, pctData, barData) := f
  let names := keyNames ++ #["Cnt", "Pct", "Bar"]
  let cols := keyCols ++ #[.ints cntData, .floats pctData, .strs barData]
  -- sort by Cnt (first column after keys) descending
  MemTable.sort ⟨names, cols⟩ #[keyCols.size] false

end Tc.Freq
