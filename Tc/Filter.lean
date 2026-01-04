/-
  Filter: fzf-based column/row filtering and search
-/
import Tc.Fzf
import Tc.View

namespace Tc.ViewStack

-- | col search: fzf jump to column by name
def colSearch (s : ViewStack) : IO ViewStack := do
  let v := s.cur; let names := ReadTable.colNames v.nav.tbl
  let dispNames := v.nav.grp ++ names.filter (!v.nav.grp.contains ·)
  let some idx ← Fzf.fzfIdx #["--prompt=Column: "] dispNames | return s
  let delta : Int := idx - v.nav.col.cur.val
  let nav' := { v.nav with col := { v.nav.col with cur := v.nav.col.cur.clamp delta } }
  return s.setCur { v with nav := nav' }

-- | Core row filter: fzf with distinct vals + PRQL, returns filtered view
def rowFilterCore (s : ViewStack) (tag : String) : IO ViewStack := do
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
  return s.push { v' with disp := s!"{tag}{curName}" }

-- | row search (/) and row filter (\): same behavior, different display
def rowSearch (s : ViewStack) : IO ViewStack := rowFilterCore s "/"
def rowFilter (s : ViewStack) : IO ViewStack := rowFilterCore s "filter "

end Tc.ViewStack
