/-
  ViewStack: non-empty view stack
-/
import Tc.Fzf
import Tc.View
import Tc.Meta
import Tc.Freq
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq

namespace Tc

-- | Non-empty view stack
structure ViewStack where
  cur : View                 -- current view (always exists)
  parents : Array View := #[]

namespace ViewStack

-- | All views (current first)
def views (s : ViewStack) : Array View := #[s.cur] ++ s.parents

-- | Stack depth
def depth (s : ViewStack) : Nat := 1 + s.parents.size

-- | Has parent?
def hasParent (s : ViewStack) : Bool := s.parents.size > 0

-- | Update current view
def setCur (s : ViewStack) (v : View) : ViewStack := { s with cur := v }

-- | Push new view (current becomes parent)
def push (s : ViewStack) (v : View) : ViewStack :=
  { s with cur := v, parents := #[s.cur] ++ s.parents }

-- | Pop view (returns to parent, or none if no parent)
def pop (s : ViewStack) : Option ViewStack :=
  if h : s.parents.size > 0 then
    some { cur := s.parents[0], parents := s.parents.extract 1 s.parents.size }
  else none

-- | Swap top two views
def swap (s : ViewStack) : ViewStack :=
  if h : s.parents.size > 0 then
    { cur := s.parents[0], parents := #[s.cur] ++ s.parents.extract 1 s.parents.size }
  else s

-- | Duplicate current view
def dup (s : ViewStack) : ViewStack :=
  { s with parents := #[s.cur] ++ s.parents }

-- | Push column metadata view
def pushMeta (s : ViewStack) : IO (Option ViewStack) := do
  let tbl ← QueryTable.queryMeta s.cur.nav.tbl <&> Meta.toMemTable
  pure <| match View.fromTbl (.mem tbl) s.cur.path with
    | some v => some (s.push { v with vkind := .colMeta, disp := "meta" })
    | none => none

-- | Push frequency view (group by grp + cursor column)
def pushFreq (s : ViewStack) : IO (Option ViewStack) := do
  let n := s.cur.nav; let names := ReadTable.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  let colIdxs := colNames.filterMap names.idxOf?
  let freq ← QueryTable.queryFreq n.tbl colIdxs
  let tbl := Freq.toMemTable freq
  pure <| match View.fromTbl (.mem tbl) s.cur.path with
    | some v => some (s.push { v with vkind := .freqV colNames, disp := s!"freq {colNames.join ","}" })
    | none => none

-- | Tab names for display (current first)
def tabNames (s : ViewStack) : Array String := s.views.map (·.tabName)

-- | col search: fzf jump to column by name
def colSearch (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  let names := ReadTable.colNames v.nav.tbl
  let dispNames := v.nav.grp ++ names.filter (!v.nav.grp.contains ·)
  match ← Fzf.fzfIdx #["--prompt=Column: "] dispNames with
  | some idx =>
    let delta : Int := idx - v.nav.col.cur.val
    let nav' := { v.nav with col := { v.nav.col with cur := v.nav.col.cur.clamp delta } }
    pure (s.setCur { v with nav := nav' })
  | none => pure s

-- | row search: fzf jump to row number
def rowSearch (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  match ← Fzf.fzf #["--prompt=Row#: ", "--print-query"] "" with
  | some result =>
    let query := (result.splitOn "\n").head?.getD "" |>.trim
    match query.toNat? with
    | some n =>
      let delta : Int := n - v.nav.row.cur.val
      let nav' := { v.nav with row := { v.nav.row with cur := v.nav.row.cur.clamp delta } }
      pure (s.setCur { v with nav := nav' })
    | none => pure s
  | none => pure s

-- | col filter: fzf multi-select columns to keep
def colFilter (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  let names := ReadTable.colNames v.nav.tbl
  let selected ← Fzf.fzfMulti #["--prompt=Select cols: "] ("\n".intercalate names.toList)
  if selected.isEmpty then pure s
  else
    let keepIdxs := selected.filterMap names.idxOf?
    let delIdxs := (Array.range names.size).filter (!keepIdxs.contains ·)
    let tbl' ← ModifyTable.delCols delIdxs v.nav.tbl
    pure <| match View.fromTbl tbl' v.path 0 v.nav.grp 0 with
      | some v' => s.setCur { v' with disp := s!"select {selected.size}" }
      | none => s

-- | row filter: fzf PRQL filter on current column
def rowFilter (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  let names := ReadTable.colNames v.nav.tbl
  let curCol := colIdxAt v.nav.grp names v.nav.col.cur.val
  let curName := names.getD curCol ""
  let vals ← QueryTable.distinct v.nav.tbl curCol
  let prompt := s!"{curName} == 'x' | > 5 | ~= 'pat' > "
  match ← Fzf.fzf #["--print-query", s!"--prompt={prompt}"] ("\n".intercalate vals.toList) with
  | some result =>
    let expr := Fzf.buildFilterExpr curName vals result
    if expr.isEmpty then pure s
    else match ← QueryTable.filter v.nav.tbl expr with
      | some tbl' => pure <| match View.fromTbl tbl' v.path v.nav.col.cur.val v.nav.grp 0 with
        | some v' => s.push { v' with disp := s!"filter {curName}" }
        | none => s
      | none => pure s
  | none => pure s

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

-- | Execute Cmd, returns IO (Option ViewStack) (none = quit or table empty)
def exec (s : ViewStack) (cmd : Cmd) (rowPg colPg : Nat) : IO (Option ViewStack) := do
  match cmd with
  | .stk .inc    => pure (some s.dup)
  | .stk .dec    => pure s.pop
  | .stk .toggle => pure (some s.swap)
  | .stk .dup    => pure (some s.dup)
  | .stk _       => pure (some s)
  | .info .inc    => (← s.pushMeta).orElse (fun _ => some s) |> pure  -- M: push meta view
  | .info .freq   => pure (some (s.metaSel Meta.selNull))      -- 0: select null cols
  | .info .dup    => pure (some (s.metaSel Meta.selSingle))    -- 1: select single-val cols
  | .info .toggle => pure s.metaSetKey                         -- Enter: set key cols (meta)
  | .info _       => pure (some s)                             -- other info: no-op
  | .freq .dup    => (← s.pushFreq).orElse (fun _ => some s) |> pure  -- F: push freq view
  | .freq _       => pure (some s)                             -- other freq: TODO
  | .col .search  => some <$> s.colSearch
  | .row .search  => some <$> s.rowSearch
  | .col .filter  => some <$> s.colFilter
  | .row .filter  => some <$> s.rowFilter
  | _ => match ← s.cur.exec cmd rowPg colPg with
    | some v' => pure (some (s.setCur v'))
    | none => pure none

end ViewStack
end Tc
