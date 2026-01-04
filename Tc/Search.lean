/-
  Search: fzf-based column/row jump
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

-- | row search: fzf jump to row number
def rowSearch (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  let some result ← Fzf.fzf #["--prompt=Row#: ", "--print-query"] "" | return s
  let query := (result.splitOn "\n").head?.getD "" |>.trim
  let some n := query.toNat? | return s
  let delta : Int := n - v.nav.row.cur.val
  let nav' := { v.nav with row := { v.nav.row with cur := v.nav.row.cur.clamp delta } }
  return s.setCur { v with nav := nav' }

end Tc.ViewStack
