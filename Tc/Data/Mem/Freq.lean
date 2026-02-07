/-
  MemTable freq: group by columns, count, pct, bar
-/
import Std.Data.HashMap
import Tc.Data.Mem.Table

namespace Tc
namespace MemTable

-- | Build row key from columns at given indices
private def rowKey (cols : Array Column) (idxs : Array Nat) (row : Nat) : String :=
  idxs.foldl (init := "") fun acc i =>
    let c := (cols.getD i default).get row
    acc ++ (if acc.isEmpty then "" else "\x00") ++ c.toRaw

-- | Freq for MemTable: returns (names, cols, totalGroups) with Cnt/Pct/Bar included
def freqRaw (t : MemTable) (colNames : Array String) : IO (Array String × Array Column × Nat) := pure $
  let colIdxs := colNames.filterMap t.names.idxOf?
  let n := MemTable.nRows t
  let counts := Id.run do
    let mut m : Std.HashMap String Nat := {}
    for r in [:n] do
      let k := rowKey t.cols colIdxs r
      m := m.insert k (m.getD k 0 + 1)
    m
  let (keys, firstRows) := Id.run do
    let mut seen : Std.HashMap String Nat := {}
    let mut ks : Array String := #[]
    let mut rs : Array Nat := #[]
    for r in [:n] do
      let k := rowKey t.cols colIdxs r
      if !seen.contains k then
        seen := seen.insert k r
        ks := ks.push k
        rs := rs.push r
    (ks, rs)
  let keyCols := colIdxs.map fun i => (t.cols.getD i default).gather firstRows
  let keyNames := colIdxs.map fun i => t.names.getD i ""
  let cntData := keys.map fun k => (counts.getD k 0).toInt64
  let (pctData, barData) := freqPctBar cntData
  let names := keyNames ++ #["Cnt", "Pct", "Bar"]
  let allCols := keyCols ++ #[.ints cntData, .floats pctData, .strs barData]
  (names, allCols, keys.size)

end MemTable
end Tc
