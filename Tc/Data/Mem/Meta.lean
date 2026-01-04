/-
  MemTable meta: column statistics via pure Lean scan
-/
import Tc.Data.Mem.Table

namespace Tc
namespace MemTable

-- | Scan column for stats: (type, cnt, dist, nullPct, min, max)
def scanCol (col : Column) : String × Int64 × Int64 × Int64 × String × String :=
  match col with
  | .ints data =>
    let sorted := data.qsort (· < ·)
    let minV := sorted.getD 0 0
    let maxV := sorted.getD (sorted.size - 1) 0
    let dist := (sorted.foldl (init := (#[] : Array Int64)) fun acc v =>
      if acc.isEmpty || acc.back! != v then acc.push v else acc).size
    ("i64", data.size.toInt64, dist.toInt64, 0, s!"{minV}", s!"{maxV}")
  | .floats data =>
    let vals := data.filter (!·.isNaN)
    let sorted := vals.qsort (· < ·)
    let minV := sorted.getD 0 0
    let maxV := sorted.getD (sorted.size - 1) 0
    let nullCnt := data.size - vals.size
    let nullPct := if data.size > 0 then nullCnt * 100 / data.size else 0
    let dist := (sorted.foldl (init := (#[] : Array Float)) fun acc v =>
      if acc.isEmpty || acc.back! != v then acc.push v else acc).size
    ("f64", vals.size.toInt64, dist.toInt64, nullPct.toInt64, s!"{minV}", s!"{maxV}")
  | .strs data =>
    let vals := data.filter (!·.isEmpty)
    let sorted := vals.qsort (· < ·)
    let minV := sorted.getD 0 ""
    let maxV := sorted.getD (sorted.size - 1) ""
    let nullCnt := data.size - vals.size
    let nullPct := if data.size > 0 then nullCnt * 100 / data.size else 0
    let dist := (sorted.foldl (init := (#[] : Array String)) fun acc v =>
      if acc.isEmpty || acc.back! != v then acc.push v else acc).size
    ("str", vals.size.toInt64, dist.toInt64, nullPct.toInt64, minV, maxV)

-- | Query meta for MemTable
def queryMeta (t : MemTable) : IO MetaTuple := do
  let mut names : Array String := #[]
  let mut types : Array String := #[]
  let mut cnts : Array Int64 := #[]
  let mut dists : Array Int64 := #[]
  let mut nulls : Array Int64 := #[]
  let mut mins : Array String := #[]
  let mut maxs : Array String := #[]
  for i in [:t.names.size] do
    let col := t.cols.getD i default
    let (typ, cnt, dist, nullPct, minV, maxV) := scanCol col
    names := names.push (t.names.getD i "")
    types := types.push typ
    cnts := cnts.push cnt
    dists := dists.push dist
    nulls := nulls.push nullPct
    mins := mins.push minV
    maxs := maxs.push maxV
  pure (names, types, cnts, dists, nulls, mins, maxs)

end MemTable
end Tc
