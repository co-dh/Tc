/-
  ADBC backend: ReadTable + RenderTable instances for SomeTable
-/
import Tc.Error
import Tc.Render
import Tc.Term
import Tc.Types

namespace Tc

-- ReadTable instance for SomeTable (ADBC/DuckDB backend)
instance : ReadTable SomeTable where
  nRows    := (·.nRows)
  colNames := (·.colNames)
  colWidths:= (·.colWidths)
  cell     := fun t r c => (t.getIdx r c).toString

-- RenderTable instance for SomeTable (uses Term.renderTable)
instance : RenderTable SomeTable where
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
      x := x + (nav.tbl.colWidths.getD di 10) + 1
    -- extract visible columns as typed Column arrays
    let visCols := visColIdxs.map fun di => nav.tbl.getCol di r0 r1
    -- call unified C render
    Term.renderTable visCols nav.tbl.colNames nav.tbl.colWidths visColIdxs
      nav.nKeys.toUInt64 colOff.toUInt64 r0.toUInt64 r1.toUInt64 nav.curRow.toUInt64 nav.curColIdx.toUInt64
      nav.selColIdxs nav.selRows st

end Tc
