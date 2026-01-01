/-
  ADBC backend: ReadTable + RenderTable instances for SomeTable
-/
import Tc.Render
import Tv.Types

namespace Tc

-- ReadTable instance for SomeTable (ADBC/DuckDB backend)
instance : ReadTable SomeTable where
  nRows    := (·.nRows)
  colNames := (·.colNames)
  colWidths:= (·.colWidths)
  cell     := fun t r c => (t.getIdx r c).toString

-- RenderTable instance for SomeTable
instance : RenderTable SomeTable where
  render nav colOff r0 r1 styles := do
    let _ ← nav.tbl.render nav.dispColIdxs nav.nKeys colOff
      r0 r1 nav.row.cur nav.curColIdx
      nav.selColIdxs nav.selRows styles 50 20 3
    pure ()

end Tc
