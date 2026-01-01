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

-- Render MemTable to terminal (pure Lean)
instance : RenderTable MemTable where
  render nav colOff r0 r1 styles := do
    let t := nav.tbl
    let dispIdxs := nav.dispColIdxs
    let w ← Term.width
    -- header row (y=0)
    let mut x : UInt32 := 0
    for di in dispIdxs do
      if di < colOff then continue
      if x >= w then break
      let name := t.names.getD di ""
      let cw := t.widths.getD di 10
      let fg := if nav.selColIdxs.contains di then Term.magenta else Term.cyan
      Term.printPad x 0 cw.toUInt32 fg Term.default name
      x := x + cw.toUInt32 + 1
    -- data rows
    for row in [r0:r1] do
      let y := (row - r0 + 1).toUInt32
      x := 0
      for di in dispIdxs do
        if di < colOff then continue
        if x >= w then break
        let cell := (MemTable.cell t row di).toString
        let cw := t.widths.getD di 10
        let isCur := row == nav.row.cur.val && di == nav.curColIdx
        let isSelRow := nav.selRows.contains row
        let isSelCol := nav.selColIdxs.contains di
        let (fg, bg) := match (isCur, isSelRow, isSelCol) with
          | (true, _, _) => (styles.getD 0 Term.black, styles.getD 1 Term.white)
          | (_, true, _) => (styles.getD 2 Term.black, styles.getD 3 Term.green)
          | (_, _, true) => (styles.getD 6 Term.magenta, styles.getD 7 Term.default)
          | _ => (Term.default, Term.default)
        Term.printPad x y cw.toUInt32 fg bg cell
        x := x + cw.toUInt32 + 1
    pure ()

end Tc
