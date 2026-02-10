/-
  Filter: fzf-based column/row filtering and search
  Pure update returns Effect; Runner executes IO.
-/
import Tc.Error
import Tc.Fzf
import Tc.Render
import Tc.View

namespace Tc.ViewStack

variable {T : Type} [TblOps T]

-- | Move row cursor to target index (pure helper)
private def moveRowTo (s : ViewStack T) (rowIdx : Nat) (search : Option (Nat × String) := none) : ViewStack T :=
  let v := s.cur
  let delta : Int := rowIdx - v.nav.row.cur.val
  let nav' := { v.nav with row := { v.nav.row with cur := v.nav.row.cur.clamp delta } }
  s.setCur { v with nav := nav', search := search.orElse (fun _ => v.search) }

-- | Move col cursor to target index (pure helper)
private def moveColTo (s : ViewStack T) (colIdx : Nat) : ViewStack T :=
  let v := s.cur
  let delta : Int := colIdx - v.nav.col.cur.val
  let nav' := { v.nav with col := { v.nav.col with cur := v.nav.col.cur.clamp delta } }
  s.setCur { v with nav := nav' }

-- | col search: fzf jump to column by name (IO version for backward compat)
def colSearch (s : ViewStack T) : IO (ViewStack T) := do
  let v := s.cur; let names := TblOps.colNames v.nav.tbl
  let dispNames := v.nav.grp ++ names.filter (!v.nav.grp.contains ·)
  let some idx ← Fzf.fzfIdx #["--prompt=Column: "] dispNames | return s
  return moveColTo s idx

-- | Shared: resolve current column, fetch sorted distinct values
private def withDistinct (s : ViewStack T)
    (f : Nat → String → Array String → IO (ViewStack T)) : IO (ViewStack T) := do
  let v := s.cur; let names := TblOps.colNames v.nav.tbl
  let curCol := colIdxAt v.nav.grp names v.nav.col.cur.val
  let curName := names.getD curCol ""
  let vals ← TblOps.distinct v.nav.tbl curCol
  f curCol curName (vals.qsort (· < ·))

-- | row search (/): find value in current column, jump to matching row (IO)
def rowSearch (s : ViewStack T) : IO (ViewStack T) := withDistinct s fun curCol curName vals => do
  let some result ← Fzf.fzf #[s!"--prompt=/{curName}: "] ("\n".intercalate vals.toList) | return s
  let start := s.cur.nav.row.cur.val + 1
  let some rowIdx ← TblOps.findRow s.tbl curCol result start true | return s
  return moveRowTo s rowIdx (some (curCol, result))

-- | search next (n): repeat last search forward (IO)
def searchNext (s : ViewStack T) : IO (ViewStack T) := do
  let v := s.cur
  let some (col, val) := v.search | return s
  let start := v.nav.row.cur.val + 1
  let some rowIdx ← TblOps.findRow v.nav.tbl col val start true | return s
  return moveRowTo s rowIdx

-- | search prev (N): repeat last search backward (IO)
def searchPrev (s : ViewStack T) : IO (ViewStack T) := do
  let v := s.cur
  let some (col, val) := v.search | return s
  let start := v.nav.row.cur.val
  let some rowIdx ← TblOps.findRow v.nav.tbl col val start false | return s
  return moveRowTo s rowIdx

-- | row filter (\): filter rows by expression, push filtered view (IO)
-- Uses TblOps.buildFilter for backend-specific syntax (PRQL vs q)
def rowFilter (s : ViewStack T) : IO (ViewStack T) := withDistinct s fun _curCol curName vals => do
  let prompt := TblOps.filterPrompt s.tbl curName
  let some result ← Fzf.fzf #["--print-query", s!"--prompt={prompt}"] ("\n".intercalate vals.toList) | return s
  let typ := TblOps.colType s.tbl _curCol
  let numeric := typ == "int" || typ == "float" || typ == "decimal"
  let expr := TblOps.buildFilter s.tbl curName vals result numeric
  if expr.isEmpty then return s
  let some tbl' ← TblOps.filter s.tbl expr | return s
  let some v' := View.fromTbl tbl' s.cur.path s.cur.nav.col.cur.val s.cur.nav.grp 0 | return s
  return s.push { v' with disp := s!"\\{curName}" }

end Tc.ViewStack

namespace Tc.Filter

variable {T : Type} [TblOps T]

-- | Pure update: returns Effect describing fzf/search operation
def update (s : ViewStack T) (cmd : Cmd) : Option (ViewStack T × Effect) :=
  match cmd with
  | .col .ent    => some (s, .fzf .col)                         -- s: column picker
  | .rowSel .inc => some (s, .fzf .row)                        -- /: row search
  | .rowSel .dec => some (s, .fzf .filter)                     -- \: row filter
  | .grp .inc    => some (s, .search .next)                    -- n: search next
  | .grp .dec    => some (s, .search .prev)                    -- N: search prev
  | _ => none

end Tc.Filter

