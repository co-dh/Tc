/-
  Filter: fzf-based column selection and row filtering
-/
import Tc.Fzf
import Tc.View

namespace Tc.ViewStack

-- | col filter: fzf multi-select columns to keep
def colFilter (s : ViewStack) : IO ViewStack := do
  let v := s.cur; let names := ReadTable.colNames v.nav.tbl
  let selected ← Fzf.fzfMulti #["--prompt=Select cols: "] ("\n".intercalate names.toList)
  if selected.isEmpty then return s
  let keepIdxs := selected.filterMap names.idxOf?
  let delIdxs := (Array.range names.size).filter (!keepIdxs.contains ·)
  let tbl' ← ModifyTable.delCols delIdxs v.nav.tbl
  let some v' := View.fromTbl tbl' v.path 0 v.nav.grp 0 | return s
  return s.setCur { v' with disp := s!"select {selected.size}" }

-- | row filter: fzf PRQL filter on current column
def rowFilter (s : ViewStack) : IO ViewStack := do
  let v := s.cur; let names := ReadTable.colNames v.nav.tbl
  let curCol := colIdxAt v.nav.grp names v.nav.col.cur.val
  let curName := names.getD curCol ""
  let vals ← QueryTable.distinct v.nav.tbl curCol
  let prompt := s!"{curName} == 'x' | > 5 | ~= 'pat' > "
  let some result ← Fzf.fzf #["--print-query", s!"--prompt={prompt}"] ("\n".intercalate vals.toList) | return s
  let expr := Fzf.buildFilterExpr curName vals result
  if expr.isEmpty then return s
  let some tbl' ← QueryTable.filter v.nav.tbl expr | return s
  let some v' := View.fromTbl tbl' v.path v.nav.col.cur.val v.nav.grp 0 | return s
  return s.push { v' with disp := s!"filter {curName}" }

end Tc.ViewStack
