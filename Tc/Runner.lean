/-
  Runner: interprets Effect values, executing IO operations.
  This is the only module that performs actual IO for effects.
-/
import Tc.Filter
import Tc.Folder
import Tc.Meta
import Tc.Freq
import Tc.Theme
import Tc.Plot

namespace Tc.Runner

-- | Helper: run IO (Option ViewStack), default to original on none
def runOpt (s : ViewStack Table) (io : IO (Option (ViewStack Table))) : IO (ViewStack Table) := do
  match ← io with
  | some s' => pure s'
  | none => pure s

-- | Run effect on ViewStack, return updated stack
def runStackEffect (s : ViewStack Table) (eff : Effect) : IO (ViewStack Table) := do
  match eff with
  | .none => pure s
  -- fzf effects
  | .fzf .col => s.colSearch
  | .fzf .row => s.rowSearch
  | .fzf .filter => s.rowFilter
  -- search effects
  | .search .next => s.searchNext
  | .search .prev => s.searchPrev
  -- query effects
  | .query .«meta» => runOpt s (Meta.push s)
  | .query (.freq colNames) =>
    let some (adbc, totalGroups) ← Table.freqTable s.cur.nav.tbl colNames | pure s
    match View.fromTbl (.adbc adbc) s.cur.path 0 colNames with
    | some v => pure (s.push { v with vkind := .freqV colNames totalGroups, disp := s!"freq {",".intercalate colNames.toList}" })
    | none => pure s
  | .query (.freqFilter cols row) =>
    match s.cur.vkind, s.pop with
    | .freqV _ _, some s' => do
      let expr ← Freq.filterExprIO s.cur.nav.tbl cols row
      match ← TblOps.filter s'.cur.nav.tbl expr with
      | some tbl' => match View.fromTbl tbl' s'.cur.path 0 s'.cur.nav.grp 0 with
        | some v => pure (s'.push v)
        | none => pure s
      | none => pure s
    | _, _ => pure s
  | .query (.filter expr) =>
    match ← TblOps.filter s.cur.nav.tbl expr with
    | some tbl' => match View.fromTbl tbl' s.cur.path s.cur.nav.col.cur.val s.cur.nav.grp 0 with
      | some v => pure (s.push { v with disp := s!"\\filter" })
      | none => pure s
    | none => pure s
  | .query (.sort colIdx sels grp asc) =>
    let tbl' ← ModifyTable.sort s.cur.nav.tbl colIdx sels grp asc
    match s.cur.rebuild tbl' (col := colIdx) (row := s.cur.nav.row.cur.val) with
    | some v => pure (s.setCur v)
    | none => pure s
  | .query (.del colIdx sels grp) =>
    let (tbl', grp') ← ModifyTable.del s.cur.nav.tbl colIdx sels grp
    match s.cur.rebuild tbl' (grp := grp') with
    | some v => pure (s.setCur v)
    | none => pure s
  -- folder effects
  | .folder .push => runOpt s (Folder.push s)
  | .folder .enter => runOpt s (Folder.enter s)
  | .folder .del => runOpt s (Folder.del s)
  | .folder (.depth delta) => runOpt s (Folder.setDepth s delta)
  -- plot effects
  | .plot .line => runOpt s (Plot.run s false)
  | .plot .bar  => runOpt s (Plot.run s true)
  -- fetch more rows (scroll-to-bottom)
  | .fetchMore =>
    match ← TblOps.fetchMore s.cur.nav.tbl with
    | some tbl' =>
      match s.cur.rebuild tbl' (row := s.cur.nav.row.cur.val) with
      | some v => pure (s.setCur v)
      | none => pure s
    | none => pure s
  -- meta effects
  | .«meta» .selNull => Meta.selNull s
  | .«meta» .selSingle => Meta.selSingle s
  | .«meta» .setKey => runOpt s (Meta.setKey s)
  -- other effects handled at AppState level
  | .quit | .fzf .cmd | .themeLoad _ => pure s

end Tc.Runner
