/-
  Freq view: group by columns, count, pct, bar
  Returns MemTable sorted by Cnt descending.
  Pure update returns Effect; Runner executes IO.
-/
import Tc.Table
import Tc.Data.Mem.Freq

namespace Tc.Freq

-- Uses shared helpers from Tc/Data/Mem/Freq.lean: toMemTable, filterExpr

-- | Push frequency view (group by grp + cursor column)
def push (s : ViewStack) : IO (Option ViewStack) := do
  let n := s.cur.nav; let names := ReadTable.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  let colIdxs := colNames.filterMap names.idxOf?
  let freq ← QueryTable.queryFreq n.tbl colIdxs
  let tbl := toMemTable freq
  let some v := View.fromTbl (.mem tbl) s.cur.path 0 colNames | return none
  return some (s.push { v with vkind := .freqV colNames, disp := s!"freq {colNames.join ","}" })

-- | Filter parent by selected freq row, pop freq and push filtered view
def filter (s : ViewStack) : IO (Option ViewStack) := do
  let .freqV cols := s.cur.vkind | return some s
  if !s.hasParent then return some s
  let some tbl := s.cur.nav.tbl.asMem? | return some s
  let some s' := s.pop | return some s
  let expr := filterExpr tbl cols s.cur.nav.row.cur.val
  let some tbl' ← QueryTable.filter s'.cur.nav.tbl expr | return some s'
  let some v := View.fromTbl tbl' s'.cur.path 0 s'.cur.nav.grp 0 | return some s'
  return some (s'.push v)

-- | Pure update: returns Effect for IO operations
def update (s : ViewStack) (cmd : Cmd) : Option (ViewStack × Effect) :=
  let n := s.cur.nav; let names := ReadTable.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  let colIdxs := colNames.filterMap names.idxOf?
  match cmd with
  | .freq .dup => some (s, .queryFreq colIdxs colNames)  -- push freq view (IO)
  | .freq .ent => match s.cur.vkind with  -- Enter and space F ~ both map to .freq .ent
    | .freqV cols => some (s, .freqFilter cols s.cur.nav.row.cur.val)  -- filter (IO)
    | _ => none
  | _ => none

-- | Execute freq command (IO version for backward compat)
def exec (s : ViewStack) (cmd : Cmd) : IO (Option ViewStack) := do
  match cmd with
  | .freq .dup => (← push s).orElse (fun _ => some s) |> pure
  | .freq .ent => match s.cur.vkind with
    | .freqV _ => filter s
    | _ => pure none
  | _ => pure none

end Tc.Freq
