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

-- Render MemTable to terminal using shared render logic
instance : RenderTable MemTable where
  render nav colOff r0 r1 st := do
    let w ← Term.width
    -- build visible column info: (origIdx, x, width)
    let mut cols : Array ColInfo := #[]
    let mut x : UInt32 := 0
    for di in nav.dispColIdxs do
      if di < colOff then continue
      if x >= w then break
      let cw := (nav.tbl.widths.getD di 10).toUInt32
      cols := cols.push (di, x, cw)
      x := x + cw + 1
    -- separator x: after last visible key column
    let visKeys := min nav.nKeys cols.size
    let sepX : UInt32 := if visKeys > 0 then
      let (_, sx, sw) := cols.getD (visKeys - 1) (0, 0, 0); sx + sw else 0
    -- header (y=0) and footer (y=nDataRows+1)
    let yFoot := (r1 - r0 + 1).toUInt32
    renderHdrRow nav.tbl.names cols nav.selColIdxs nav.curColIdx st 0
    renderHdrRow nav.tbl.names cols nav.selColIdxs nav.curColIdx st yFoot
    renderSep sepX 0 st; renderSep sepX yFoot st
    -- data rows
    for row in [r0:r1] do
      let y := (row - r0 + 1).toUInt32
      for (di, cx, cw) in cols do
        let si := cellStyle (row == nav.curRow && di == nav.curColIdx) (nav.selRows.contains row)
                            (nav.selColIdxs.contains di) (row == nav.curRow) (di == nav.curColIdx)
        renderCell cx y cw (styleFg st si) (styleBg st si) (MemTable.cell nav.tbl row di).toString
      renderSep sepX y st
    pure ()

end Tc
