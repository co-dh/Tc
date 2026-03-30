/-
  Freq: group by columns, count, pct, bar.
  Pure update returns residual Effect; dispatch executes IO inline.
-/
import Tc.View
import Tc.Data.ADBC.Ops

namespace Tc.Freq

-- | Build filter expression from freq view row
def filterExprIO (tbl : AdbcTable) (cols : Array String) (row : Nat) : IO String := do
  let names := TblOps.colNames tbl
  let idxs := cols.filterMap names.idxOf?
  let fetchedCols ← TblOps.getCols tbl idxs row (row + 1)
  let vals := fetchedCols.map fun col => (col.get 0).toPrql
  let exprs := cols.zip vals |>.map fun (c, v) => s!"{c} == {v}"
  pure (exprs.joinWith " && ")

-- | Pure update by Cmd. Returns residual Effect for dispatch to execute.
def update (s : ViewStack AdbcTable) (h : Cmd) : Option (ViewStack AdbcTable × Effect) :=
  let n := s.cur.nav
  let curName := n.curColName
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  match h with
  | .freqOpen => some (s, .freq colNames)
  | .freqFilter => match s.cur.vkind with
    | .freqV cols _ => some (s, .freqFilter cols s.cur.nav.row.cur.val)
    | _ => none
  | _ => none

end Tc.Freq
