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

-- | Query freq for MemTable: group by colIdxs, count, pct, bar
def queryFreq (t : MemTable) (colIdxs : Array Nat) : IO FreqTuple := pure $
  let n := MemTable.nRows t
  -- count occurrences per key
  let counts := Id.run do
    let mut m : Std.HashMap String Nat := {}
    for r in [:n] do
      let k := rowKey t.cols colIdxs r
      m := m.insert k (m.getD k 0 + 1)
    m
  -- collect unique keys with first occurrence row
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
  -- build key columns from first occurrence rows
  let keyCols := colIdxs.map fun i =>
    let col := t.cols.getD i default
    match col with
    | .ints data => Column.ints (firstRows.map fun r => data.getD r 0)
    | .floats data => Column.floats (firstRows.map fun r => data.getD r 0)
    | .strs data => Column.strs (firstRows.map fun r => data.getD r "")
  let keyNames := colIdxs.map fun i => t.names.getD i ""
  -- compute cnt, pct, bar
  let cntData := keys.map fun k => (counts.getD k 0).toInt64
  let total := cntData.foldl (init := 0) (· + ·)
  let pctData := cntData.map fun c => if total > 0 then c.toFloat * 100 / total.toFloat else 0
  let barData := pctData.map fun p => String.ofList (List.replicate (p / 5).toUInt32.toNat '#')
  (keyNames, keyCols, cntData, pctData, barData)

end MemTable

-- | Freq table helpers (shared by Tc.Freq and App.Mem)
namespace Freq

-- | Convert FreqTuple to MemTable (sorted by Cnt desc)
def toMemTable (f : FreqTuple) : MemTable :=
  let (keyNames, keyCols, cntData, pctData, barData) := f
  let names := keyNames ++ #["Cnt", "Pct", "Bar"]
  let cols := keyCols ++ #[.ints cntData, .floats pctData, .strs barData]
  MemTable.sort ⟨names, cols⟩ #[keyCols.size] false

-- | Build filter expression from freq row (col1 == val1 && col2 == val2 ...)
def filterExpr (tbl : MemTable) (cols : Array String) (row : Nat) : String :=
  let vals := cols.mapIdx fun i _ =>
    match tbl.cols.getD i default with
    | .strs d => s!"'{d.getD row ""}'"
    | .ints d => s!"{d.getD row 0}"
    | .floats d => s!"{d.getD row 0}"
  " && ".intercalate (cols.zip vals |>.map fun (c, v) => s!"{c} == {v}").toList

end Freq
end Tc
