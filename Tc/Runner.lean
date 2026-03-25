/-
  Runner: interprets Effect values, executing IO operations.
  This is the only module that performs actual IO for effects.
  Includes Freq view logic.
-/
import Tc.Filter
import Tc.Folder
import Tc.Meta
import Tc.Theme
import Tc.Plot
import Tc.Export
import Tc.Clip

namespace Tc

/-! ## Freq: group by columns, count, pct, bar -/

namespace Freq

-- | Build filter expression from freq view row
def filterExprIO (tbl : AdbcTable) (cols : Array String) (row : Nat) : IO String := do
  let names := TblOps.colNames tbl
  let idxs := cols.filterMap names.idxOf?
  let fetchedCols ← TblOps.getCols tbl idxs row (row + 1)
  let vals := fetchedCols.map fun col => (col.get 0).toPrql
  let exprs := cols.zip vals |>.map fun (c, v) => s!"{c} == {v}"
  pure (" && ".intercalate exprs.toList)

-- | Pure update: returns Effect for IO operations
def update (s : ViewStack AdbcTable) (cmd : Cmd) : Option (ViewStack AdbcTable × Effect) :=
  let n := s.cur.nav; let names := TblOps.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  match cmd with
  | .freq .dup => some (s, .query (.freq colNames))
  | .freq .ent => match s.cur.vkind with
    | .freqV cols _ => some (s, .query (.freqFilter cols s.cur.nav.row.cur.val))
    | _ => none
  | _ => none

end Freq

namespace Runner

-- | Run effect on ViewStack, return updated stack
def runStackEffect (s : ViewStack AdbcTable) (eff : Effect) : IO (ViewStack AdbcTable) :=
  let opt io := (·.getD s) <$> io
  match eff with
  | .none => pure s
  | .fzf .col => s.colSearch
  | .fzf .row => s.rowSearch
  | .fzf .filter => s.rowFilter
  | .fzf .cmd => pure s
  | .search .next => s.searchDir true
  | .search .prev => s.searchDir false
  | .query .colMeta => opt (Meta.push s)
  | .query (.freq colNames) => do
    let some (adbc, totalGroups) ← AdbcTable.freqTable s.tbl colNames | pure s
    match View.fromTbl adbc s.cur.path 0 colNames with
    | some v => pure (s.push { v with vkind := .freqV colNames totalGroups, disp := s!"freq {",".intercalate colNames.toList}" })
    | none => pure s
  | .query (.freqFilter cols row) => do
    match s.cur.vkind, s.pop with
    | .freqV _ _, some s' => do
      let expr ← Freq.filterExprIO s.tbl cols row
      match ← TblOps.filter s'.tbl expr with
      | some tbl' => match s'.cur.rebuild tbl' (row := 0) with
        | some v => pure (s'.push v)
        | none => pure s
      | none => pure s
    | _, _ => pure s
  | .query (.filter expr) => do
    match ← TblOps.filter s.tbl expr with
    | some tbl' => match s.cur.rebuild tbl' (row := 0) with
      | some v => pure (s.push { v with disp := s!"\\filter" })
      | none => pure s
    | none => pure s
  | .query (.sort colIdx sels grp asc) => do
    let tbl' ← ModifyTable.sort s.tbl colIdx sels grp asc
    match s.cur.rebuild tbl' (col := colIdx) (row := s.cur.nav.row.cur.val) with
    | some v => pure (s.setCur v)
    | none => pure s
  | .folder .push => opt (Folder.push s)
  | .folder .enter => opt (Folder.enter s)
  | .folder .parent => opt (Folder.goParent s)
  | .folder .del => opt (Folder.del s)
  | .folder (.depth d) => opt (Folder.setDepth s d)
  | .plot kind => opt (Plot.run s kind)
  | .colMeta .selNull => Meta.selNull s
  | .colMeta .selSingle => Meta.selSingle s
  | .colMeta .setKey => opt (Meta.setKey s)
  | .fetchMore => do
    match ← TblOps.fetchMore s.tbl with
    | some tbl' =>
      match s.cur.rebuild tbl' (row := s.cur.nav.row.cur.val) with
      | some v => pure (s.setCur v)
      | none => pure s
    | none => pure s
  | .clip ce => Clip.run s ce
  | .export fmt => Export.run s fmt
  | .sessionSave | .sessionLoad | .join | .quit | .themeLoad _
  | .transpose | .diff => pure s  -- handled in runEffectCore, not here

end Runner
end Tc
