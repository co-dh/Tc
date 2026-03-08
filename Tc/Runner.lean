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

-- | Helper: run IO (Option ViewStack), default to original on none
def runOpt (s : ViewStack AdbcTable) (io : IO (Option (ViewStack AdbcTable))) : IO (ViewStack AdbcTable) :=
  (·.getD s) <$> io

-- | Fzf effects: column/row search, filter
private def runFzf (s : ViewStack AdbcTable) : FzfEffect → IO (ViewStack AdbcTable)
  | .col => s.colSearch
  | .row => s.rowSearch
  | .filter => s.rowFilter
  | .cmd => pure s

-- | Search effects: next/prev
private def runSearch (s : ViewStack AdbcTable) : SearchEffect → IO (ViewStack AdbcTable)
  | .next => s.searchDir true
  | .prev => s.searchDir false

-- | Query effects: meta, freq, filter, sort, delete
private def runQuery (s : ViewStack AdbcTable) : QueryEffect → IO (ViewStack AdbcTable)
  | .«meta» => runOpt s (Meta.push s)
  | .freq colNames => do
    let some (adbc, totalGroups) ← AdbcTable.freqTable s.tbl colNames | pure s
    match View.fromTbl adbc s.cur.path 0 colNames with
    | some v => pure (s.push { v with vkind := .freqV colNames totalGroups, disp := s!"freq {",".intercalate colNames.toList}" })
    | none => pure s
  | .freqFilter cols row => do
    match s.cur.vkind, s.pop with
    | .freqV _ _, some s' => do
      let expr ← Freq.filterExprIO s.tbl cols row
      match ← TblOps.filter s'.tbl expr with
      | some tbl' => match s'.cur.rebuild tbl' (row := 0) with
        | some v => pure (s'.push v)
        | none => pure s
      | none => pure s
    | _, _ => pure s
  | .filter expr => do
    match ← TblOps.filter s.tbl expr with
    | some tbl' => match s.cur.rebuild tbl' (row := 0) with
      | some v => pure (s.push { v with disp := s!"\\filter" })
      | none => pure s
    | none => pure s
  | .sort colIdx sels grp asc => do
    let tbl' ← ModifyTable.sort s.tbl colIdx sels grp asc
    match s.cur.rebuild tbl' (col := colIdx) (row := s.cur.nav.row.cur.val) with
    | some v => pure (s.setCur v)
    | none => pure s

-- | Folder effects: push, enter, delete, depth
private def runFolder (s : ViewStack AdbcTable) : FolderEffect → IO (ViewStack AdbcTable)
  | .push => runOpt s (Folder.push s)
  | .enter => runOpt s (Folder.enter s)
  | .del => runOpt s (Folder.del s)
  | .depth delta => runOpt s (Folder.setDepth s delta)

-- | Plot effects: line, bar
private def runPlot (s : ViewStack AdbcTable) : PlotEffect → IO (ViewStack AdbcTable)
  | .line => runOpt s (Plot.run s false)
  | .bar  => runOpt s (Plot.run s true)

-- | Meta effects: select nulls, select singles, set key
private def runMeta (s : ViewStack AdbcTable) : MetaEffect → IO (ViewStack AdbcTable)
  | .selNull => Meta.selNull s
  | .selSingle => Meta.selSingle s
  | .setKey => runOpt s (Meta.setKey s)

-- | Run effect on ViewStack, return updated stack
def runStackEffect (s : ViewStack AdbcTable) (eff : Effect) : IO (ViewStack AdbcTable) :=
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

end Runner
end Tc
