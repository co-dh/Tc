/-
  MemTable meta: column statistics via pure Lean scan
-/
import Tc.Data.Mem.Table

namespace Tc
namespace MemTable

private def cdStep [BEq α] (acc : Array α) (v : α) : Array α :=
  if h : acc.size > 0 then
    if acc[acc.size - 1] != v then acc.push v else acc
  else acc.push v

private theorem cdStep_size_le [BEq α] (acc : Array α) (v : α) :
    (cdStep acc v).size ≤ acc.size + 1 := by
  unfold cdStep
  split
  · split
    · simp [Array.size_push]
    · omega
  · simp [Array.size_push]

private theorem foldl_cdStep_size [BEq α] (xs : List α) (acc : Array α) :
    (xs.foldl cdStep acc).size ≤ acc.size + xs.length := by
  induction xs generalizing acc with
  | nil => simp
  | cons x xs ih =>
    simp only [List.foldl, List.length_cons]
    have h1 := cdStep_size_le acc x
    have h2 := ih (cdStep acc x)
    omega

def countDistinct [BEq α] (sorted : Array α) : Nat :=
  (sorted.toList.foldl cdStep #[]).size

/-- The number of distinct elements in a sorted array cannot exceed its size.
    Each step of the fold processes one element and increases the accumulator
    by at most 1, so after `sorted.size` steps the accumulator size ≤ `sorted.size`. -/
theorem countDistinct_le_size [BEq α] (sorted : Array α) :
    countDistinct sorted ≤ sorted.size := by
  unfold countDistinct
  have h1 := foldl_cdStep_size sorted.toList (#[] : Array α)
  simp only [Array.size_empty] at h1
  simp only [Array.length_toList] at h1
  omega

-- | Scan column for stats: (type, cnt, dist, nullPct, min, max)
def scanCol (col : Column) : String × Int64 × Int64 × Int64 × String × String :=
  match col with
  | .ints data =>
    let sorted := data.qsort (· < ·)
    let (minV, maxV) := if h : sorted.size > 0
      then (sorted[0], sorted[sorted.size - 1])
      else (0, 0)
    let dist := countDistinct sorted
    ("i64", data.size.toInt64, dist.toInt64, 0, s!"{minV}", s!"{maxV}")
  | .floats data =>
    let vals := data.filter (!·.isNaN)
    let sorted := vals.qsort (· < ·)
    let (minV, maxV) := if h : sorted.size > 0
      then (sorted[0], sorted[sorted.size - 1])
      else (0.0, 0.0)
    let nullCnt := data.size - vals.size
    let nullPct := if data.size > 0 then nullCnt * 100 / data.size else 0
    let dist := countDistinct sorted
    ("f64", vals.size.toInt64, dist.toInt64, nullPct.toInt64, s!"{minV}", s!"{maxV}")
  | .strs data =>
    let vals := data.filter (!·.isEmpty)
    let sorted := vals.qsort (· < ·)
    let (minV, maxV) := if h : sorted.size > 0
      then (sorted[0], sorted[sorted.size - 1])
      else ("", "")
    let nullCnt := data.size - vals.size
    let nullPct := if data.size > 0 then nullCnt * 100 / data.size else 0
    let dist := countDistinct sorted
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
