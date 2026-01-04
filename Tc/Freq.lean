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

-- | Build filter expression from freq row (col1 == val1 && col2 == val2 ...)
def filterExpr (tbl : MemTable) (cols : Array String) (row : Nat) : String :=
  let vals := cols.mapIdx fun i _ =>
    match tbl.cols.getD i default with
    | .strs data => s!"'{data.getD row ""}'"
    | .ints data => s!"{data.getD row 0}"
    | .floats data => s!"{data.getD row 0}"
  let exprs := cols.zip vals |>.map fun (c, v) => s!"{c} == {v}"
  " && ".intercalate exprs.toList

end Tc.Freq
