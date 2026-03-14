/-
  Derive: add computed column via PRQL derive.
  Press '=' → fzf prompt with column names as hints → requery with derive → push view.
-/
import Tc.Data.ADBC.Ops
import Tc.Fzf
import Tc.View

namespace Tc.Derive

-- Auto-incrementing counter for derived column names (_1, _2, ...)
initialize deriveCount : IO.Ref Nat ← IO.mkRef 0

-- | Prompt for expression, requery with derive, push new view.
-- Returns unchanged stack on cancel (like Export.run).
def run (s : ViewStack AdbcTable) : IO (ViewStack AdbcTable) := do
  let names := TblOps.colNames s.tbl
  let hint := "\n".intercalate names.toList
  let some expr ← Fzf.fzf #["--print-query", "--prompt=derive: "] hint | return s
  let expr := expr.trimAscii.toString
  if expr.isEmpty then return s
  let n ← deriveCount.modifyGet fun n => (n + 1, n + 1)
  let name := s!"_{n}"
  let q := s.tbl.query.pipe (.derive #[(name, expr)])
  Log.write "derive" q.render
  -- derive preserves row count, no need for queryCount
  let some tbl' ← AdbcTable.requery q s.tbl.totalRows | return s
  let some v := s.cur.rebuild tbl' | return s
  return s.push { v with disp := s!"={name}" }

end Tc.Derive
