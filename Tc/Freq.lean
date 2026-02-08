/-
  Freq view: group by columns, count, pct, bar
  Converts FreqResult to AdbcTable via AdbcTable.fromArrays.
  Pure update returns Effect; Runner executes IO.
-/
import Tc.View
import Tc.Table

namespace Tc.Freq

-- | Build filter expression from freq view row (IO: fetches cells from table)
def filterExprIO (tbl : Table) (cols : Array String) (row : Nat) : IO String := do
  let names := TblOps.colNames tbl
  let idxs := cols.filterMap names.idxOf?
  let fetchedCols ← TblOps.getCols tbl idxs row (row + 1)
  let vals := fetchedCols.map fun col => (col.get 0).toPrql
  let exprs := cols.zip vals |>.map fun (c, v) => s!"{c} == {v}"
  pure (" && ".intercalate exprs.toList)

-- | Push frequency view (group by grp + cursor column)
def push (s : ViewStack Table) : IO (Option (ViewStack Table)) := do
  let n := s.cur.nav; let names := TblOps.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  let some (adbc, totalGroups) ← Table.freqTable n.tbl colNames | return none
  let some v := View.fromTbl (.adbc adbc) s.cur.path 0 colNames | return none
  return some (s.push { v with vkind := .freqV colNames totalGroups, disp := s!"freq {colNames.join ","}" })

-- | Filter parent by selected freq row, pop freq and push filtered view
def filter (s : ViewStack Table) : IO (Option (ViewStack Table)) := do
  let .freqV cols _ := s.cur.vkind | return some s
  if !s.hasParent then return some s
  let expr ← filterExprIO s.cur.nav.tbl cols s.cur.nav.row.cur.val
  let some s' := s.pop | return some s
  let some tbl' ← TblOps.filter s'.cur.nav.tbl expr | return some s'
  let some v := View.fromTbl tbl' s'.cur.path 0 s'.cur.nav.grp 0 | return some s'
  return some (s'.push v)

-- | Pure update: returns Effect for IO operations
def update (s : ViewStack Table) (cmd : Cmd) : Option (ViewStack Table × Effect) :=
  let n := s.cur.nav; let names := TblOps.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  match cmd with
  | .freq .dup => some (s, .query (.freq colNames))  -- push freq view (IO)
  | .freq .ent => match s.cur.vkind with  -- Enter and space F ~ both map to .freq .ent
    | .freqV cols _ => some (s, .query (.freqFilter cols s.cur.nav.row.cur.val))  -- filter (IO)
    | _ => none
  | _ => none

end Tc.Freq
