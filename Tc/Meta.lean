/-
  Meta view: column statistics (name, type, count, distinct, null%, min, max)
  Returns MemTable with typed columns for proper sorting.
-/
import Tc.Data.Mem.Table

namespace Tc.Meta

-- Meta column headers
def headers : Array String := #["column", "type", "cnt", "dist", "null%", "min", "max"]

-- Meta column indices
def colDist : Nat := 3  -- distinct count
def colNull : Nat := 4  -- null%

-- Convert queryMeta tuple result to MemTable
def toMemTable (m : Array String × Array String × Array Int64 × Array Int64 × Array Int64 × Array String × Array String) : MemTable :=
  let (names, types, cnts, dists, nulls, mins, maxs) := m
  ⟨headers, #[.strs names, .strs types, .ints cnts, .ints dists, .ints nulls, .strs mins, .strs maxs]⟩

-- Get int value from column at row
def getInt (t : MemTable) (col row : Nat) : Int64 :=
  match t.cols.getD col default with
  | .ints data => data.getD row 0
  | _ => 0

-- Select rows where int column satisfies predicate
def selectRows (t : MemTable) (col : Nat) (pred : Int64 → Bool) : Array Nat :=
  (Array.range (MemTable.nRows t)).filter fun r => pred (getInt t col r)

-- Select 100% null columns
def selNull (t : MemTable) : Array Nat := selectRows t colNull (· == 100)

-- Select single-value columns (distinct == 1)
def selSingle (t : MemTable) : Array Nat := selectRows t colDist (· == 1)

-- Get column names from selected rows (col 0 is "column")
def selNames (t : MemTable) (selRows : Array Nat) : Array String :=
  selRows.filterMap fun r =>
    match t.cols.getD 0 default with
    | .strs data => some (data.getD r "")
    | _ => none

end Tc.Meta
