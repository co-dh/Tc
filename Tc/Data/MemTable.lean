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
  names  : Array String
  cols   : Array Column
  widths : Array Nat

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

-- Compute column width
def calcWidth (name : String) (col : Column) : Nat :=
  let w0 := name.length
  let wMax := match col with
    | .ints data => data.foldl (fun mx v => max mx (Cell.fmtInt v).length) w0
    | .floats data => data.foldl (fun mx v => max mx s!"{v}".length) w0
    | .strs data => data.foldl (fun mx s => max mx s.length) w0
  min wMax 50

-- Load CSV file into MemTable
def load (path : String) : IO (Except String MemTable) := do
  let content ← IO.FS.readFile path
  let recs := CSV.parse content
  match recs.toList with
  | [] => pure (.ok ⟨#[], #[], #[]⟩)
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
    -- build typed columns
    let cols := strCols.map buildColumn
    let widths := cols.mapIdx fun i c => calcWidth (names.getD i "") c
    pure (.ok ⟨names, cols, widths⟩)

-- Get cell at (row, col)
def cell (t : MemTable) (r c : Nat) : Cell := (t.cols.getD c default).get r

-- Row count
def nRows (t : MemTable) : Nat := (t.cols.getD 0 default).size

end MemTable

-- ModifyTable instance for MemTable (extends ReadTable)
instance : ModifyTable MemTable where
  nRows     := MemTable.nRows
  colNames  := (·.names)
  colWidths := (·.widths)
  cell      := fun t r c => (MemTable.cell t r c).toString
  delCols   := fun delIdxs t =>
    let keepIdxs := (Array.range t.names.size).filter (!delIdxs.contains ·)
    { names  := keepIdxs.map fun i => t.names.getD i ""
      cols   := keepIdxs.map fun i => t.cols.getD i default
      widths := keepIdxs.map fun i => t.widths.getD i 0 }

-- Render MemTable using unified C render (no Lean string alloc)
instance : RenderTable MemTable where
  render nav colOff r0 r1 st := do
    let w ← Term.width
    -- find visible columns (skip first colOff display columns)
    let mut visColIdxs : Array Nat := #[]
    let mut x : Nat := 0
    for h : dispPos in [:nav.dispColIdxs.size] do
      let di := nav.dispColIdxs[dispPos]
      if dispPos < colOff then continue
      if x >= w.toNat then break
      visColIdxs := visColIdxs.push di
      x := x + (nav.tbl.widths.getD di 10) + 1
    -- pass visible columns directly (C reads typed data, no Cell alloc)
    let visCols := visColIdxs.map fun di => nav.tbl.cols.getD di default
    -- call C render (r0, r1 = visible row range)
    Term.renderTable visCols nav.tbl.names nav.tbl.widths visColIdxs
      nav.nKeys.toUInt64 colOff.toUInt64 r0.toUInt64 r1.toUInt64 nav.curRow.toUInt64 nav.curColIdx.toUInt64
      nav.selColIdxs nav.selRows st

end Tc
