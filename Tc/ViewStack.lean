/-
  ViewStack operations: meta, freq, exec
  Core ViewStack type is in View.lean
-/
import Tc.Filter
import Tc.Meta
import Tc.Freq
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq

namespace Tc.ViewStack

-- | Push column metadata view
def pushMeta (s : ViewStack) : IO (Option ViewStack) := do
  let tbl ← QueryTable.queryMeta s.cur.nav.tbl <&> Meta.toMemTable
  let some v := View.fromTbl (.mem tbl) s.cur.path | return none
  return some (s.push { v with vkind := .colMeta, disp := "meta" })

-- | Push frequency view (group by grp + cursor column)
def pushFreq (s : ViewStack) : IO (Option ViewStack) := do
  let n := s.cur.nav; let names := ReadTable.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  let colIdxs := colNames.filterMap names.idxOf?
  let freq ← QueryTable.queryFreq n.tbl colIdxs
  let tbl := Freq.toMemTable freq
  let some v := View.fromTbl (.mem tbl) s.cur.path 0 colNames | return none
  return some (s.push { v with vkind := .freqV colNames, disp := s!"freq {colNames.join ","}" })

-- | Select rows in meta view by predicate on MemTable
def metaSel (s : ViewStack) (sel : MemTable → Array Nat) : ViewStack :=
  if s.cur.vkind != .colMeta then s else
  match s.cur.nav.tbl.asMem? with
  | some tbl =>
    let rows := sel tbl
    let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
    s.setCur { s.cur with nav := nav' }
  | none => s

-- | Set key cols from meta view selections, pop to parent, select cols for deletion
def metaSetKey (s : ViewStack) : Option ViewStack :=
  if s.cur.vkind != .colMeta then some s else
  if !s.hasParent then some s else
  match s.cur.nav.tbl.asMem? with
  | some tbl =>
    let colNames := Meta.selNames tbl s.cur.nav.row.sels
    match s.pop with
    | some s' =>
      -- Set grp and also select those cols (user likely wants to delete)
      let nav' := { s'.cur.nav with grp := colNames, col := { s'.cur.nav.col with sels := colNames } }
      some (s'.setCur { s'.cur with nav := nav' })
    | none => some s
  | none => some s

-- | Filter parent by selected freq row, pop freq and push filtered view
def freqFilter (s : ViewStack) : IO (Option ViewStack) := do
  let .freqV cols := s.cur.vkind | return some s
  if !s.hasParent then return some s
  let some tbl := s.cur.nav.tbl.asMem? | return some s
  let some s' := s.pop | return some s
  let expr := Freq.filterExpr tbl cols s.cur.nav.row.cur.val
  let some tbl' ← QueryTable.filter s'.cur.nav.tbl expr | return some s'
  let some v := View.fromTbl tbl' s'.cur.path 0 s'.cur.nav.grp 0 | return some s'
  return some (s'.push v)  -- push filtered view, keep parent

-- | Execute Cmd, returns IO (Option ViewStack) (none = quit or table empty)
def exec (s : ViewStack) (cmd : Cmd) (rowPg colPg : Nat) : IO (Option ViewStack) := do
  match cmd with
  | .stk .inc    => pure (some s.dup)
  | .stk .dec    => pure s.pop  -- pop, or quit at base (none)
  | .stk .ent => pure (some s.swap)
  | .stk .dup    => pure (some s.dup)
  | .stk _       => pure (some s)
  | .metaV .dup    => (← s.pushMeta).orElse (fun _ => some s) |> pure  -- M: push meta view (dup=constructor)
  | .metaV .dec    => pure (some (s.metaSel Meta.selNull))      -- 0: select null cols
  | .metaV .inc    => pure (some (s.metaSel Meta.selSingle))    -- 1: select single-val cols
  | .metaV .ent => match s.cur.vkind with                     -- Enter: dispatch by view kind
    | .colMeta => pure s.metaSetKey
    | .freqV _ => s.freqFilter
    | _ => pure (some s)
  | .metaV _       => pure (some s)                             -- other info: no-op
  | .freq .dup    => (← s.pushFreq).orElse (fun _ => some s) |> pure  -- F: push freq view
  | .freq .ent => s.freqFilter                                      -- Enter: filter by freq row
  | .col .search  => some <$> s.colSearch
  | .row .search  => some <$> s.rowSearch
  | .rowSel .inc  => some <$> s.searchNext    -- n: search next
  | .rowSel .dec  => some <$> s.searchPrev   -- N: search prev
  | .row .filter  => some <$> s.rowFilter
  | _ => match ← s.cur.exec cmd rowPg colPg with
    | some v' => pure (some (s.setCur v'))
    | none => pure none

end Tc.ViewStack
