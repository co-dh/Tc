/-
  In-memory table: column-major with typed cells
-/
import Tc.Data.CSV
import Tc.Data.Table
import Tc.Nav
import Tc.Render
import Tc.Term
import Tc.Types

namespace Tc

/-! ## MemTable -/

-- In-memory table: column-major storage
structure MemTable where
  names  : Array String
  cols   : Array (Array Cell)  -- column-major
  widths : Array Nat

namespace MemTable

-- Parse cell value (detect type)
def parseCell (s : String) : Cell :=
  if s.isEmpty then .null
  else if let some n := s.toInt? then .int n
  else .str s

-- Transpose rows to columns
def transpose (rows : Array (Array Cell)) (nc : Nat) : Array (Array Cell) := Id.run do
  let mut cols : Array (Array Cell) := (List.replicate nc #[]).toArray
  for r in rows do
    for i in [:nc] do
      cols := cols.modify i (·.push (r.getD i .null))
  cols

-- Compute column width from column data
def calcWidth (name : String) (col : Array Cell) : Nat :=
  let w0 := name.length
  let wMax := col.foldl (fun mx c => max mx c.toString.length) w0
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
    let rows := rest.map (·.map parseCell) |>.toArray
    let cols := transpose rows nc
    let widths := names.mapIdx fun i n => calcWidth n (cols.getD i #[])
    pure (.ok ⟨names, cols, widths⟩)

-- Get cell at (row, col)
def cell (t : MemTable) (r c : Nat) : Cell := (t.cols.getD c #[]).getD r .null

-- Row count
def nRows (t : MemTable) : Nat := (t.cols.getD 0 #[]).size

end MemTable

-- ReadTable instance for MemTable
instance : ReadTable MemTable where
  nRows     := MemTable.nRows
  colNames  := (·.names)
  colWidths := (·.widths)
  cell      := fun t r c => (MemTable.cell t r c).toString

-- Render MemTable using unified C render (no Lean string alloc)
instance : RenderTable MemTable where
  render nav colOff r0 r1 st := do
    let w ← Term.width
    -- find visible columns
    let mut visColIdxs : Array Nat := #[]
    let mut x : Nat := 0
    for di in nav.dispColIdxs do
      if di < colOff then continue
      if x >= w.toNat then break
      visColIdxs := visColIdxs.push di
      x := x + (nav.tbl.widths.getD di 10) + 1
    -- extract visible column data (slice rows r0..r1)
    let visCols := visColIdxs.map fun di =>
      let col := nav.tbl.cols.getD di #[]
      (Array.range (r1 - r0)).map fun i => col.getD (r0 + i) .null
    -- call C render
    Term.renderTable visCols nav.tbl.names nav.tbl.widths visColIdxs
      nav.nKeys.toUInt64 colOff.toUInt64 r0.toUInt64 nav.curRow.toUInt64 nav.curColIdx.toUInt64
      nav.selColIdxs nav.selRows st

end Tc
