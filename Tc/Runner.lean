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

-- | Fzf effects: column/row search, filter
private def runFzf (s : ViewStack Table) : FzfEffect → IO (ViewStack Table)
  | .col => s.colSearch
  | .row => s.rowSearch
  | .filter => s.rowFilter
  | .cmd => pure s

-- | Search effects: next/prev
private def runSearch (s : ViewStack Table) : SearchEffect → IO (ViewStack Table)
  | .next => s.searchNext
  | .prev => s.searchPrev

-- | Query effects: meta, freq, filter, sort, delete
private def runQuery (s : ViewStack Table) : QueryEffect → IO (ViewStack Table)
  | .«meta» => runOpt s (Meta.push s)
  | .freq colNames => do
    let some (adbc, totalGroups) ← Table.freqTable s.tbl colNames | pure s
    match View.fromTbl (.adbc adbc) s.cur.path 0 colNames with
    | some v => pure (s.push { v with vkind := .freqV colNames totalGroups, disp := s!"freq {",".intercalate colNames.toList}" })
    | none => pure s
  | .freqFilter cols row => do
    match s.cur.vkind, s.pop with
    | .freqV _ _, some s' => do
      let expr ← Freq.filterExprIO s.tbl cols row
      match ← TblOps.filter s'.tbl expr with
      | some tbl' => match View.fromTbl tbl' s'.cur.path 0 s'.cur.nav.grp 0 with
        | some v => pure (s'.push v)
        | none => pure s
      | none => pure s
    | _, _ => pure s
  | .filter expr => do
    match ← TblOps.filter s.tbl expr with
    | some tbl' => match View.fromTbl tbl' s.cur.path s.cur.nav.col.cur.val s.cur.nav.grp 0 with
      | some v => pure (s.push { v with disp := s!"\\filter" })
      | none => pure s
    | none => pure s
  | .sort colIdx sels grp asc => do
    let tbl' ← ModifyTable.sort s.tbl colIdx sels grp asc
    match s.cur.rebuild tbl' (col := colIdx) (row := s.cur.nav.row.cur.val) with
    | some v => pure (s.setCur v)
    | none => pure s
  | .del colIdx sels grp => do
    let (tbl', grp') ← ModifyTable.del s.tbl colIdx sels grp
    match s.cur.rebuild tbl' (grp := grp') with
    | some v => pure (s.setCur v)
    | none => pure s

-- | Folder effects: push, enter, delete, depth
private def runFolder (s : ViewStack Table) : FolderEffect → IO (ViewStack Table)
  | .push => runOpt s (Folder.push s)
  | .enter => runOpt s (Folder.enter s)
  | .del => runOpt s (Folder.del s)
  | .depth delta => runOpt s (Folder.setDepth s delta)

-- | Plot effects: line, bar
private def runPlot (s : ViewStack Table) : PlotEffect → IO (ViewStack Table)
  | .line => runOpt s (Plot.run s false)
  | .bar  => runOpt s (Plot.run s true)

-- | Meta effects: select nulls, select singles, set key
private def runMeta (s : ViewStack Table) : MetaEffect → IO (ViewStack Table)
  | .selNull => Meta.selNull s
  | .selSingle => Meta.selSingle s
  | .setKey => runOpt s (Meta.setKey s)

-- | Run effect on ViewStack, return updated stack
def runStackEffect (s : ViewStack Table) (eff : Effect) : IO (ViewStack Table) :=
  match eff with
  | .none => pure s
  | .fzf e => runFzf s e
  | .search e => runSearch s e
  | .query e => runQuery s e
  | .folder e => runFolder s e
  | .plot e => runPlot s e
  | .«meta» e => runMeta s e
  | .fetchMore => do
    match ← TblOps.fetchMore s.tbl with
    | some tbl' =>
      match s.cur.rebuild tbl' (row := s.cur.nav.row.cur.val) with
      | some v => pure (s.setCur v)
      | none => pure s
    | none => pure s
  | .quit | .themeLoad _ => pure s

end Tc.Runner
