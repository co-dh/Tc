/-
  Runner: interprets Effect values, executing IO operations.
  This is the only module that performs actual IO for effects.
-/
import Tc.Filter
import Tc.Folder
import Tc.Meta
import Tc.Freq
import Tc.Theme

namespace Tc.Runner

-- | Helper: run IO (Option ViewStack), default to original on none
def runOpt (s : ViewStack) (io : IO (Option ViewStack)) : IO ViewStack := do
  match ← io with
  | some s' => pure s'
  | none => pure s

-- | Run effect on ViewStack, return updated stack
def runStackEffect (s : ViewStack) (eff : Effect) : IO ViewStack := do
  match eff with
  | .none => pure s
  -- fzf effects
  | .fzfCol => s.colSearch
  | .fzfRow _ _ => s.rowSearch
  | .fzfFilter _ _ => s.rowFilter
  -- search effects
  | .findNext => s.searchNext
  | .findPrev => s.searchPrev
  -- query effects
  | .queryMeta => runOpt s (Meta.push s)
  | .queryFreq cols colNames =>
    let n := s.cur.nav
    let freq ← QueryTable.queryFreq n.tbl cols
    let tbl := Freq.toMemTable freq
    match View.fromTbl (.mem tbl) s.cur.path 0 colNames with
    | some v => pure (s.push { v with vkind := .freqV colNames, disp := s!"freq {colNames.join ","}" })
    | none => pure s
  | .freqFilter cols row =>
    match s.cur.vkind, s.cur.nav.tbl.asMem?, s.pop with
    | .freqV _, some tbl, some s' =>
      let expr := Freq.filterExpr tbl cols row
      match ← QueryTable.filter s'.cur.nav.tbl expr with
      | some tbl' => match View.fromTbl tbl' s'.cur.path 0 s'.cur.nav.grp 0 with
        | some v => pure (s'.push v)
        | none => pure s
      | none => pure s
    | _, _, _ => pure s
  | .queryFilter expr =>
    match ← QueryTable.filter s.cur.nav.tbl expr with
    | some tbl' => match View.fromTbl tbl' s.cur.path s.cur.nav.col.cur.val s.cur.nav.grp 0 with
      | some v => pure (s.push { v with disp := s!"\\filter" })
      | none => pure s
    | none => pure s
  | .querySort colIdx grp asc =>
    let n := s.cur.nav
    let tbl' ← ModifyTable.sort n.tbl colIdx grp asc
    match View.fromTbl tbl' s.cur.path colIdx (n.grp) n.row.cur.val with
    | some v => pure (s.setCur { v with precAdj := s.cur.precAdj, widthAdj := s.cur.widthAdj })
    | none => pure s
  | .queryDel colIdx sels grp =>
    let n := s.cur.nav
    let (tbl', grp') ← ModifyTable.del n.tbl colIdx sels grp
    match View.fromTbl tbl' s.cur.path n.col.cur.val grp' 0 with
    | some v => pure (s.setCur { v with precAdj := s.cur.precAdj, widthAdj := s.cur.widthAdj })
    | none => pure s  -- table became empty, keep current
  -- folder effects
  | .folderPush => runOpt s (Folder.push s)
  | .folderEnter => runOpt s (Folder.enter s)
  | .folderDel => runOpt s (Folder.del s)
  | .folderDepth delta => runOpt s (Folder.setDepth s delta)
  -- other effects handled at AppState level
  | .quit | .themeLoad _ => pure s

end Tc.Runner
