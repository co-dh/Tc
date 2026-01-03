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

end MemTable

-- ModifyTable instance for MemTable (extends ReadTable)
instance : ModifyTable MemTable where
  nRows    := MemTable.nRows
  colNames := (·.names)
  delCols  := fun delIdxs t =>
    let keepIdxs := (Array.range t.names.size).filter (!delIdxs.contains ·)
    { names := keepIdxs.map fun i => t.names.getD i ""
      cols  := keepIdxs.map fun i => t.cols.getD i default }

-- Render MemTable using unified C render
-- C computes widths if inWidths is empty, returns computed widths
instance : RenderTable MemTable where
  render nav inWidths colOff r0 r1 st :=
    -- call C render with all cols, display order, etc.
    Term.renderTable nav.tbl.cols nav.tbl.names inWidths nav.dispColIdxs
      (MemTable.nRows nav.tbl).toUInt64 nav.nKeys.toUInt64 colOff.toUInt64
      r0.toUInt64 r1.toUInt64 nav.curRow.toUInt64 nav.curColIdx.toUInt64
      nav.selColIdxs nav.selRows st

end Tc
