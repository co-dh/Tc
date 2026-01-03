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

-- RenderTable instance for SomeTable
-- First render (inWidths empty): extract all rows for width calc
-- Subsequent renders: extract only visible slice [r0, r1)
instance : RenderTable SomeTable where
  render nav inWidths colOff r0 r1 st := do
    if inWidths.isEmpty then
      -- first render: full data for width computation
      let cols := (Array.range nav.tbl.nCols).map fun c => nav.tbl.getCol c 0 nav.tbl.nRows
      Term.renderTable cols nav.tbl.colNames nav.tbl.colFmts inWidths nav.dispColIdxs
        nav.tbl.nRows.toUInt64 nav.nKeys.toUInt64 colOff.toUInt64
        0 nav.tbl.nRows.toUInt64 nav.curRow.toUInt64 nav.curColIdx.toUInt64
        nav.selColIdxs nav.selRows st
    else
      -- subsequent: only visible rows, adjust cursor/selections to local coords
      let cols := (Array.range nav.tbl.nCols).map fun c => nav.tbl.getCol c r0 r1
      let adjCur := nav.curRow - r0  -- local cursor position
      let adjSel := nav.selRows.filterMap fun r =>  -- local selection indices
        if r >= r0 && r < r1 then some (r - r0) else none
      Term.renderTable cols nav.tbl.colNames nav.tbl.colFmts inWidths nav.dispColIdxs
        nav.tbl.nRows.toUInt64 nav.nKeys.toUInt64 colOff.toUInt64
        0 (r1 - r0).toUInt64 adjCur.toUInt64 nav.curColIdx.toUInt64
        nav.selColIdxs adjSel st

end Tc
