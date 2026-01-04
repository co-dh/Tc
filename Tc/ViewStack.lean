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

-- | Push column metadata view (IO via unsafe)
unsafe def pushMetaImpl (s : ViewStack) : Option ViewStack :=
  letI : ReadTable s.cur.t := s.cur.instR; letI : QueryMeta s.cur.t := s.cur.instQ
  match unsafeIO (QueryMeta.queryMeta s.cur.nav.tbl <&> Meta.toMemTable) with
  | .ok tbl => match View.fromTbl tbl s.cur.path with
    | some v => some (s.push { v with vkind := .colMeta, disp := "meta" })
    | none => none
  | .error _ => none

@[implemented_by pushMetaImpl]
def pushMeta (_ : ViewStack) : Option ViewStack := none

-- | Push frequency view (group by grp + cursor column)
unsafe def pushFreqImpl (s : ViewStack) : Option ViewStack :=
  letI : ReadTable s.cur.t := s.cur.instR; letI : QueryFreq s.cur.t := s.cur.instF
  let n := s.cur.nav; let names := ReadTable.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  -- cols = grp + cursor (if not in grp)
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  let colIdxs := colNames.filterMap names.idxOf?
  match unsafeIO (QueryFreq.queryFreq n.tbl colIdxs) with
  | .ok freq =>
    let tbl := Freq.toMemTable freq
    match View.fromTbl tbl s.cur.path with
    | some v => some (s.push { v with vkind := .freqV colNames, disp := s!"freq {colNames.join ","}" })
    | none => none
  | .error _ => none

@[implemented_by pushFreqImpl]
def pushFreq (_ : ViewStack) : Option ViewStack := none

-- | Tab names for display (current first)
def tabNames (s : ViewStack) : Array String := s.views.map (·.tabName)

-- | col search: fzf jump to column by name
unsafe def colSearchImpl (s : ViewStack) : ViewStack :=
  let v := s.cur; letI : ReadTable v.t := v.instR
  let names := ReadTable.colNames v.nav.tbl
  let dispNames := v.nav.grp ++ names.filter (!v.nav.grp.contains ·)
  match unsafeIO (Fzf.fzfIdx #["--prompt=Column: "] dispNames) with
  | .ok (some idx) =>
    let delta : Int := idx - v.nav.col.cur.val
    let nav' := { v.nav with col := { v.nav.col with cur := v.nav.col.cur.clamp delta } }
    s.setCur { v with nav := nav' }
  | _ => s

@[implemented_by colSearchImpl]
def colSearch (s : ViewStack) : ViewStack := s

-- | row search: fzf jump to row number (uses --print-query to capture typed input)
unsafe def rowSearchImpl (s : ViewStack) : ViewStack :=
  let v := s.cur; letI : ReadTable v.t := v.instR
  match unsafeIO (Fzf.fzf #["--prompt=Row#: ", "--print-query"] "") with
  | .ok (some result) =>
    -- --print-query: first line is query, rest is selection (empty here)
    let query := (result.splitOn "\n").head?.getD "" |>.trim
    match query.toNat? with
    | some n =>
      let delta : Int := n - v.nav.row.cur.val
      let nav' := { v.nav with row := { v.nav.row with cur := v.nav.row.cur.clamp delta } }
      s.setCur { v with nav := nav' }
    | none => s
  | _ => s

@[implemented_by rowSearchImpl]
def rowSearch (s : ViewStack) : ViewStack := s

-- | col filter: fzf multi-select columns to keep
unsafe def colFilterImpl (s : ViewStack) : ViewStack :=
  let v := s.cur
  letI : ReadTable v.t := v.instR; letI : ModifyTable v.t := v.instM
  letI : RenderTable v.t := v.instV; letI : QueryMeta v.t := v.instQ
  letI : QueryFreq v.t := v.instF; letI : QueryFilter v.t := v.instL
  letI : QueryDistinct v.t := v.instD
  let names := ReadTable.colNames v.nav.tbl
  match unsafeIO (Fzf.fzfMulti #["--prompt=Select cols: "] ("\n".intercalate names.toList)) with
  | .ok selected =>
    if selected.isEmpty then s
    else
      let keepIdxs := selected.filterMap names.idxOf?
      let delIdxs := (Array.range names.size).filter (!keepIdxs.contains ·)
      let tbl' := ModifyTable.delCols delIdxs v.nav.tbl
      match View.fromTbl tbl' v.path 0 v.nav.grp 0 with
      | some v' => s.setCur { v' with disp := s!"select {selected.size}" }
      | none => s
  | _ => s

@[implemented_by colFilterImpl]
def colFilter (s : ViewStack) : ViewStack := s

-- | row filter: fzf PRQL filter on current column
unsafe def rowFilterImpl (s : ViewStack) : ViewStack :=
  let v := s.cur
  letI : ReadTable v.t := v.instR; letI : ModifyTable v.t := v.instM
  letI : RenderTable v.t := v.instV; letI : QueryMeta v.t := v.instQ
  letI : QueryFreq v.t := v.instF; letI : QueryFilter v.t := v.instL
  letI : QueryDistinct v.t := v.instD
  let names := ReadTable.colNames v.nav.tbl
  let curCol := colIdxAt v.nav.grp names v.nav.col.cur.val
  let curName := names.getD curCol ""
  match unsafeIO (QueryDistinct.distinct v.nav.tbl curCol) with
  | .ok vals =>
    let prompt := s!"{curName} == 'x' | > 5 | ~= 'pat' > "
    match unsafeIO (Fzf.fzf #["--print-query", s!"--prompt={prompt}"] ("\n".intercalate vals.toList)) with
    | .ok (some result) =>
      let expr := Fzf.buildFilterExpr curName vals result
      if expr.isEmpty then s
      else match unsafeIO (QueryFilter.filter v.nav.tbl expr) with
        | .ok (some tbl') => match View.fromTbl tbl' v.path v.nav.col.cur.val v.nav.grp 0 with
          | some v' => s.push { v' with disp := s!"filter {curName}" }
          | none => s
        | _ => s
    | _ => s
  | _ => s

@[implemented_by rowFilterImpl]
def rowFilter (s : ViewStack) : ViewStack := s

-- | Select rows in meta view by predicate on MemTable
unsafe def metaSelImpl (s : ViewStack) (sel : MemTable → Array Nat) : ViewStack :=
  if s.cur.vkind != .colMeta then s else
  letI : ReadTable s.cur.t := s.cur.instR
  -- Get underlying MemTable from meta view and compute selection
  match unsafeIO (pure (sel (unsafeCast s.cur.nav.tbl : MemTable))) with
  | .ok rows =>
    let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
    s.setCur { s.cur with nav := nav' }
  | .error _ => s

@[implemented_by metaSelImpl]
def metaSel (s : ViewStack) (_ : MemTable → Array Nat) : ViewStack := s

-- | Set key cols from meta view selections, pop to parent
unsafe def metaSetKeyImpl (s : ViewStack) : Option ViewStack :=
  if s.cur.vkind != .colMeta then some s else
  if !s.hasParent then some s else
  letI : ReadTable s.cur.t := s.cur.instR
  let tbl : MemTable := unsafeCast s.cur.nav.tbl
  let colNames := Meta.selNames tbl s.cur.nav.row.sels
  match s.pop with
  | some s' =>
    letI : ReadTable s'.cur.t := s'.cur.instR
    let nav' := { s'.cur.nav with grp := colNames }
    some (s'.setCur { s'.cur with nav := nav' })
  | none => some s

@[implemented_by metaSetKeyImpl]
def metaSetKey (s : ViewStack) : Option ViewStack := some s

-- | Execute Cmd (pure), returns Option ViewStack (none = quit or table empty)
def exec (s : ViewStack) (cmd : Cmd) (rowPg colPg : Nat) : Option ViewStack :=
  match cmd with
  | .stk .inc    => some s.dup
  | .stk .dec    => s.pop
  | .stk .toggle => some s.swap
  | .stk .dup    => some s.dup
  | .stk _       => some s
  | .metaCol .inc    => s.pushMeta.orElse fun _ => some s  -- M: push meta view
  | .metaCol .freq   => some (s.metaSel Meta.selNull)      -- 0: select null cols
  | .metaCol .dup    => some (s.metaSel Meta.selSingle)    -- 1: select single-val cols
  | .metaCol .toggle => s.metaSetKey                       -- Enter: set key cols (meta)
  | .metaCol _       => some s                             -- other metaCol: no-op
  | .freqCol _       => some s                             -- freqCol: TODO
  | .colSel .freq    => s.pushFreq.orElse fun _ => some s  -- F: push freq view
  | .col .search => some s.colSearch
  | .row .search => some s.rowSearch
  | .col .filter => some s.colFilter
  | .row .filter => some s.rowFilter
  | _ => match s.cur.exec cmd rowPg colPg with
    | some v' => some (s.setCur v')
    | none => none

end ViewStack
end Tc
