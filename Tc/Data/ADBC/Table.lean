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
  cell     := fun t r c => (t.getIdx r c).toString

-- RenderTable instance for SomeTable
-- Extracts all columns for width computation (once), then renders
instance : RenderTable SomeTable where
  render nav inWidths colOff r0 r1 st := do
    -- extract all columns (full range for width calc, 0..nRows)
    let allCols := (Array.range nav.tbl.nCols).map fun c =>
      nav.tbl.getCol c 0 nav.tbl.nRows
    -- call unified C render
    Term.renderTable allCols nav.tbl.colNames inWidths nav.dispColIdxs
      nav.tbl.nRows.toUInt64 nav.nKeys.toUInt64 colOff.toUInt64
      r0.toUInt64 r1.toUInt64 nav.curRow.toUInt64 nav.curColIdx.toUInt64
      nav.selColIdxs nav.selRows st

end Tc
