/-
  Filter: fzf-based column/row filtering and search
  Pure update returns Effect; Runner executes IO.
  ADBC check wrappers around generic GViewStack search functions.
-/
import Tc.Error
import Tc.View

namespace Tc.ViewStack

-- | col search: fzf jump to column by name
def colSearch (s : ViewStack) : IO ViewStack := GViewStack.colSearch s

-- | row search (/): find value in current column, jump to matching row
def rowSearch (s : ViewStack) : IO ViewStack := do
  if s.cur.nav.tbl.isAdbc then errorPopup "search disabled for DB; use \\ filter"; return s
  GViewStack.rowSearch s

-- | search next (n): repeat last search forward
def searchNext (s : ViewStack) : IO ViewStack := do
  if s.cur.nav.tbl.isAdbc then errorPopup "search disabled for DB"; return s
  GViewStack.searchNext s

-- | search prev (N): repeat last search backward
def searchPrev (s : ViewStack) : IO ViewStack := do
  if s.cur.nav.tbl.isAdbc then errorPopup "search disabled for DB"; return s
  GViewStack.searchPrev s

-- | row filter (\): filter rows by PRQL expression, push filtered view
def rowFilter (s : ViewStack) : IO ViewStack := GViewStack.rowFilter s

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
