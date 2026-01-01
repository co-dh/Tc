/-
  ADBC backend: ReadTable + RenderTable instances for SomeTable
-/
import Tc.Render
import Tc.Types

namespace Tc

-- ReadTable instance for SomeTable (ADBC/DuckDB backend)
instance : ReadTable SomeTable where
  nRows    := (·.nRows)
  colNames := (·.colNames)
  colWidths:= (·.colWidths)
  cell     := fun t r c => (t.getIdx r c).toString

-- RenderTable instance for SomeTable (uses C render)
instance : RenderTable SomeTable where
  render nav colOff r0 r1 st := do
    let _ ← nav.tbl.render nav.dispColIdxs nav.nKeys colOff
      r0 r1 nav.curRow nav.curColIdx nav.selColIdxs nav.selRows st 50 20 3
    pure ()

end Tc
