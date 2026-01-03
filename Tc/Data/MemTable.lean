/-
  In-memory table: column-major with typed columns
-/
import Tc.Data.CSV
import Tc.Data.Table
import Tc.Error
import Tc.Nav
import Tc.Render
import Tc.Term
import Tc.Types

namespace Tc

/-! ## MemTable -/

-- In-memory table: typed column storage
structure MemTable where
  names : Array String
  cols  : Array Column

namespace MemTable

-- Parse string to float (returns NaN on failure)
@[extern "lean_string_to_float"]
opaque stringToFloat : @& String → Float

-- Detect column type from first non-empty value
def detectType (vals : Array String) : Char :=
  match vals.find? (·.length > 0) with
  | none => 's'  -- default to string
  | some v =>
    if v.toInt?.isSome then 'i'
    else if v.contains '.' then 'f'  -- assume float if has decimal point
    else 's'

-- Build typed column from string values
def buildColumn (vals : Array String) : Column :=
  match detectType vals with
  | 'i' => .ints (vals.map fun s => s.toInt?.getD 0 |>.toInt64)
  | 'f' => .floats (vals.map stringToFloat)
  | _ => .strs vals

-- Load CSV file into MemTable
def load (path : String) : IO (Except String MemTable) := do
  let content ← IO.FS.readFile path
  let recs := CSV.parse content
  match recs.toList with
  | [] => pure (.ok ⟨#[], #[]⟩)
  | hdr :: rest =>
    let names := hdr
    let nc := names.size
    -- transpose: build array of string values per column
    let strCols : Array (Array String) := Id.run do
      let mut cols := (List.replicate nc #[]).toArray
      for row in rest do
        for i in [:nc] do
          cols := cols.modify i (·.push (row.getD i ""))
      cols
    let cols := strCols.map buildColumn
    pure (.ok ⟨names, cols⟩)

-- Row count
def nRows (t : MemTable) : Nat := (t.cols.getD 0 default).size

-- Compare floats (handles NaN as null)
def cmpFloat (x y : Float) : Ordering :=
  if x < y then .lt else if x > y then .gt else .eq

-- Compare two cells (for sorting)
def cmpCell (a b : Cell) : Ordering :=
  match a, b with
  | .null, .null => .eq
  | .null, _ => .lt  -- nulls first
  | _, .null => .gt
  | .int x, .int y => compare x y
  | .float x, .float y => cmpFloat x y
  | .str x, .str y => compare x y
  | _, _ => .eq  -- mixed types: treat as equal

-- Compare two rows by multiple column indices
def cmpRows (cols : Array Column) (idxs : Array Nat) (r1 r2 : Nat) : Ordering :=
  idxs.foldl (init := .eq) fun ord c =>
    if ord != .eq then ord
    else cmpCell ((cols.getD c default).get r1) ((cols.getD c default).get r2)

-- Sort table by column indices (asc=true for ascending)
def sort (t : MemTable) (idxs : Array Nat) (asc : Bool) : MemTable :=
  let n := nRows t
  let perm := (Array.range n).qsort fun i j =>
    let ord := cmpRows t.cols idxs i j
    if asc then ord == .lt else ord == .gt
  -- apply permutation to all columns
  let cols' := t.cols.map fun col =>
    match col with
    | .ints data => .ints (perm.map fun i => data.getD i 0)
    | .floats data => .floats (perm.map fun i => data.getD i 0)
    | .strs data => .strs (perm.map fun i => data.getD i "")
  { t with cols := cols' }

end MemTable

-- ModifyTable instance for MemTable (extends ReadTable)
instance : ModifyTable MemTable where
  nRows    := MemTable.nRows
  colNames := (·.names)
  delCols  := fun delIdxs t =>
    let keepIdxs := (Array.range t.names.size).filter (!delIdxs.contains ·)
    { names := keepIdxs.map fun i => t.names.getD i ""
      cols  := keepIdxs.map fun i => t.cols.getD i default }
  sortBy   := fun idxs asc t => MemTable.sort t idxs asc

-- Render MemTable using unified C render
-- C computes widths if inWidths is empty, returns computed widths
instance : RenderTable MemTable where
  render nav inWidths colOff r0 r1 moveDir st precAdj widthAdj :=
    -- call C render with all cols, display order, etc. (empty fmts = use Column tag)
    Term.renderTable nav.tbl.cols nav.tbl.names #[] inWidths nav.dispColIdxs
      (MemTable.nRows nav.tbl).toUInt64 nav.nKeys.toUInt64 colOff.toUInt64
      r0.toUInt64 r1.toUInt64 nav.curRow.toUInt64 nav.curColIdx.toUInt64
      moveDir.toInt64 nav.selColIdxs nav.selRows st precAdj.toInt64 widthAdj.toInt64

end Tc
