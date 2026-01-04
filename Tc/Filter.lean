/-
  Filter: fzf-based column/row filtering and search
-/
import Tc.Error
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

-- | row search (/): find value in current column, jump to matching row
-- Disabled for ADBC (no row numbers in SQL); use filter (\) instead
def rowSearch (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  if v.nav.tbl.isAdbc then Error.set "search disabled for DB; use \\ filter"; return s
  let names := ReadTable.colNames v.nav.tbl
  let curCol := colIdxAt v.nav.grp names v.nav.col.cur.val
  let curName := names.getD curCol ""
  let vals ← QueryTable.distinct v.nav.tbl curCol
  let some result ← Fzf.fzf #[s!"--prompt=/{curName}: "] ("\n".intercalate vals.toList) | return s
  let start := v.nav.row.cur.val + 1
  let some rowIdx ← QueryTable.findRow v.nav.tbl curCol result start true | return s
  let delta : Int := rowIdx - v.nav.row.cur.val
  let nav' := { v.nav with row := { v.nav.row with cur := v.nav.row.cur.clamp delta } }
  return s.setCur { v with nav := nav', search := some (curCol, result) }

-- | search next (n): repeat last search forward
def searchNext (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  if v.nav.tbl.isAdbc then Error.set "search disabled for DB"; return s
  let some (col, val) := v.search | return s
  let start := v.nav.row.cur.val + 1
  let some rowIdx ← QueryTable.findRow v.nav.tbl col val start true | return s
  let delta : Int := rowIdx - v.nav.row.cur.val
  let nav' := { v.nav with row := { v.nav.row with cur := v.nav.row.cur.clamp delta } }
  return s.setCur { v with nav := nav' }

-- | search prev (N): repeat last search backward
def searchPrev (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  if v.nav.tbl.isAdbc then Error.set "search disabled for DB"; return s
  let some (col, val) := v.search | return s
  let start := v.nav.row.cur.val
  let some rowIdx ← QueryTable.findRow v.nav.tbl col val start false | return s
  let delta : Int := rowIdx - v.nav.row.cur.val
  let nav' := { v.nav with row := { v.nav.row with cur := v.nav.row.cur.clamp delta } }
  return s.setCur { v with nav := nav' }

-- | row filter (\): filter rows by PRQL expression, push filtered view
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
  return s.push { v' with disp := s!"\\{curName}" }

end Tc.ViewStack
