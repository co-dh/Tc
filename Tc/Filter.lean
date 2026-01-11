/-
  Filter: fzf-based column/row filtering and search
  Pure update returns Effect; Runner executes IO.
-/
import Tc.Error
import Tc.Fzf
import Tc.Render
import Tc.View

namespace Tc.ViewStack

-- | col search: fzf jump to column by name (IO version for backward compat)
def colSearch (s : ViewStack) : IO ViewStack := do
  let v := s.cur; let names := ReadTable.colNames v.nav.tbl
  let dispNames := v.nav.grp ++ names.filter (!v.nav.grp.contains ·)
  let some idx ← Fzf.fzfIdx #["--prompt=Column: "] dispNames | return s
  return moveColTo s idx

-- | row search (/): find value in current column, jump to matching row (IO)
def rowSearch (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  if v.nav.tbl.isAdbc then errorPopup "search disabled for DB; use \\ filter"; return s
  let names := ReadTable.colNames v.nav.tbl
  let curCol := colIdxAt v.nav.grp names v.nav.col.cur.val
  let curName := names.getD curCol ""
  let vals ← QueryTable.distinct v.nav.tbl curCol
  let some result ← Fzf.fzf #[s!"--prompt=/{curName}: "] ("\n".intercalate vals.toList) | return s
  let start := v.nav.row.cur.val + 1
  let some rowIdx ← QueryTable.findRow v.nav.tbl curCol result start true | return s
  return moveRowTo s rowIdx (some (curCol, result))

-- | search next (n): repeat last search forward (IO)
def searchNext (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  if v.nav.tbl.isAdbc then errorPopup "search disabled for DB"; return s
  let some (col, val) := v.search | return s
  let start := v.nav.row.cur.val + 1
  let some rowIdx ← QueryTable.findRow v.nav.tbl col val start true | return s
  return moveRowTo s rowIdx

-- | search prev (N): repeat last search backward (IO)
def searchPrev (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  if v.nav.tbl.isAdbc then errorPopup "search disabled for DB"; return s
  let some (col, val) := v.search | return s
  let start := v.nav.row.cur.val
  let some rowIdx ← QueryTable.findRow v.nav.tbl col val start false | return s
  return moveRowTo s rowIdx

-- | row filter (\): filter rows by PRQL expression, push filtered view (IO)
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
  return s.push { v' with disp := s!"\\{curName}" }

end Tc.ViewStack

namespace Tc.Filter

-- | Pure update: returns Effect describing fzf/search operation
def update (s : ViewStack) (cmd : Cmd) : Option (ViewStack × Effect) :=
  let v := s.cur; let names := ReadTable.colNames v.nav.tbl
  let curCol := colIdxAt v.nav.grp names v.nav.col.cur.val
  let curName := names.getD curCol ""
  match cmd with
  | .col .ent    => some (s, .fzfCol)                          -- s: column picker
  | .rowSel .inc => some (s, .fzfRow curCol curName)           -- /: row search
  | .rowSel .dec => some (s, .fzfFilter curCol curName)        -- \: row filter
  | .grp .inc    => some (s, .findNext)                        -- n: search next
  | .grp .dec    => some (s, .findPrev)                        -- N: search prev
  | _ => none

-- | Execute search/filter command (IO version for backward compat)
def exec (s : ViewStack) (cmd : Cmd) : IO (Option ViewStack) := do
  match cmd with
  | .col .ent    => some <$> s.colSearch   -- s: fzf jump to column
  | .rowSel .inc => some <$> s.rowSearch   -- /: fzf search in column
  | .rowSel .dec => some <$> s.rowFilter   -- \: fzf PRQL filter
  | .grp .inc    => some <$> s.searchNext  -- n: repeat search forward
  | .grp .dec    => some <$> s.searchPrev  -- N: repeat search backward
  | _ => pure none  -- not handled

end Tc.Filter

-- | Compile-time: rowSearch uses errorPopup (fails if errorPopup missing/wrong type)
#check @Tc.ViewStack.rowSearch
