/-
  ViewStack: non-empty view stack as Array View (a[0] = current)
-/
import Tc.Fzf
import Tc.View
import Tc.Meta
import Tc.Freq
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq

namespace Tc

-- | Non-empty view stack: a[0] = current, a[1:] = parents
abbrev ViewStack := { a : Array View // a.size > 0 }

namespace ViewStack

-- | Current view
@[inline] def cur (s : ViewStack) : View := s.val[0]'s.property

-- | All views
@[inline] def views (s : ViewStack) : Array View := s.val

-- | Stack depth
@[inline] def depth (s : ViewStack) : Nat := s.val.size

-- | Has parent?
@[inline] def hasParent (s : ViewStack) : Bool := s.val.size > 1

-- | Update current view
def setCur (s : ViewStack) (v : View) : ViewStack :=
  ⟨s.val.set (Fin.mk 0 s.property) v, by simp [Array.size_set]; exact s.property⟩

-- | Push new view (current becomes parent)
def push (s : ViewStack) (v : View) : ViewStack :=
  ⟨#[v] ++ s.val, by simp; omega⟩

-- | Pop view (returns to parent, or none if no parent)
def pop (s : ViewStack) : Option ViewStack :=
  if h : s.val.size > 1 then some ⟨s.val.extract 1 s.val.size, by simp [Array.size_extract]; omega⟩
  else none

-- | Swap top two views
def swap (s : ViewStack) : ViewStack :=
  if h : s.val.size > 1 then
    let a := s.val[0]'s.property
    let b := s.val[1]'h
    ⟨#[b, a] ++ s.val.extract 2 s.val.size, by simp; omega⟩
  else s

-- | Duplicate current view (push copy of current)
def dup (s : ViewStack) : ViewStack :=
  ⟨#[s.cur] ++ s.val, by simp; omega⟩

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
  let some v := View.fromTbl (.mem tbl) s.cur.path | return none
  return some (s.push { v with vkind := .freqV colNames, disp := s!"freq {colNames.join ","}" })

-- | Tab names for display (current first)
def tabNames (s : ViewStack) : Array String := s.views.map (·.tabName)

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

-- | Select rows in meta view by predicate on MemTable
def metaSel (s : ViewStack) (sel : MemTable → Array Nat) : ViewStack :=
  if s.cur.vkind != .colMeta then s else
  match s.cur.nav.tbl.asMem? with
  | some tbl =>
    let rows := sel tbl
    let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
    s.setCur { s.cur with nav := nav' }
  | none => s

-- | Set key cols from meta view selections, pop to parent
def metaSetKey (s : ViewStack) : Option ViewStack :=
  if s.cur.vkind != .colMeta then some s else
  if !s.hasParent then some s else
  match s.cur.nav.tbl.asMem? with
  | some tbl =>
    let colNames := Meta.selNames tbl s.cur.nav.row.sels
    match s.pop with
    | some s' =>
      let nav' := { s'.cur.nav with grp := colNames }
      some (s'.setCur { s'.cur with nav := nav' })
    | none => some s
  | none => some s

-- | Filter parent by selected freq row, push filtered view
def freqFilter (s : ViewStack) : IO (Option ViewStack) := do
  let .freqV cols := s.cur.vkind | return some s
  if !s.hasParent then return some s
  let some tbl := s.cur.nav.tbl.asMem? | return some s
  let some s' := s.pop | return some s
  let expr := Freq.filterExpr tbl cols s.cur.nav.row.cur.val
  let some tbl' ← QueryTable.filter s'.cur.nav.tbl expr | return some s'
  let some v := View.fromTbl tbl' s'.cur.path 0 s'.cur.nav.grp 0 | return some s'
  return some (s'.push { v with disp := s!"filter {cols.join ","}" })

-- | Execute Cmd, returns IO (Option ViewStack) (none = quit or table empty)
def exec (s : ViewStack) (cmd : Cmd) (rowPg colPg : Nat) : IO (Option ViewStack) := do
  match cmd with
  | .stk .inc    => pure (some s.dup)
  | .stk .dec    => pure s.pop
  | .stk .ent => pure (some s.swap)
  | .stk .dup    => pure (some s.dup)
  | .stk _       => pure (some s)
  | .info .inc    => (← s.pushMeta).orElse (fun _ => some s) |> pure  -- M: push meta view
  | .info .freq   => pure (some (s.metaSel Meta.selNull))      -- 0: select null cols
  | .info .dup    => pure (some (s.metaSel Meta.selSingle))    -- 1: select single-val cols
  | .info .ent => match s.cur.vkind with                     -- Enter: dispatch by view kind
    | .colMeta => pure s.metaSetKey
    | .freqV _ => s.freqFilter
    | _ => pure (some s)
  | .info _       => pure (some s)                             -- other info: no-op
  | .freq .dup    => (← s.pushFreq).orElse (fun _ => some s) |> pure  -- F: push freq view
  | .freq .ent => s.freqFilter                                      -- Enter: filter by freq row
  | .col .search  => some <$> s.colSearch
  | .row .search  => some <$> s.rowSearch
  | .col .filter  => some <$> s.colFilter
  | .row .filter  => some <$> s.rowFilter
  | _ => match ← s.cur.exec cmd rowPg colPg with
    | some v' => pure (some (s.setCur v'))
    | none => pure none

end ViewStack
end Tc
