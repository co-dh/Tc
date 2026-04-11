/-
  View: wraps NavState + metadata for table type T
  Generic over T to support different build variants (Core, DuckDB, Full).
-/
import Tc.Types
import Tc.Render
import Tc.Lens

namespace Tc

-- | View: wraps NavState for table type T
structure View (T : Type) [TblOps T] where
  nRows : Nat
  nCols : Nat
  nav : NavState nRows nCols T
  path : String              -- source file/command (for tab display)
  vkind : ViewKind := .tbl
  disp : String := ""        -- custom display name (overrides filename)
  prec : Nat := 3            -- float decimal count (0-17)
  widthAdj : Int := 0        -- width adjustment offset (-=narrower, +=wider)
  widths : Array Nat := #[]  -- cached column widths (per-view for type safety)
  search : Option (Nat × String) := none  -- last search: (colIdx, value)
  sameHide : Array String := #[]  -- diff: columns with identical values (hidden separately from user hide)

namespace View

variable {T : Type} [TblOps T]

-- | Field lenses for non-dependent View fields, auto-generated via `gen_lenses`.
-- (nRows/nCols/nav are skipped — their types are mutually dependent, so they can't
-- be expressed as simple `Lens' (View T) A` — the codomain would depend on the source.)
gen_lenses (View T) where path, vkind, disp, prec, widthAdj, widths, search, sameHide

-- | Create from NavState + path
def new {nr nc : Nat} (nav : NavState nr nc T) (path : String) : View T :=
  { nRows := nr, nCols := nc, nav, path }

-- | Current folder directory (or "." for non-folder views)
@[inline] def curDir (v : View T) : String := match v.vkind with | .fld dir _ => dir | _ => "."

-- | Tab display name: custom disp or filename from path
@[inline] def tabName (v : View T) : String :=
  match v.vkind with
  | .fld p _ => if v.disp.isEmpty || p.startsWith "/" then p else v.disp
  | _ => if v.disp.isEmpty then v.path.splitOn "/" |>.getLast? |>.getD v.path else v.disp

-- | Render the view, returns (ViewState, updated View with new widths)
@[inline] def doRender (v : View T) (vs : ViewState) (styles : Array UInt32)
    (heatMode : UInt8 := 1) (sparklines : Array String := #[]) : IO (ViewState × View T) := do
  let names := TblOps.colNames v.nav.tbl
  let extraHidden := v.sameHide |>.filterMap names.idxOf?
  let (vs', widths) ← render v.nav vs v.widths styles (Int.ofNat v.prec) v.widthAdj v.vkind heatMode sparklines extraHidden
  pure (vs', { v with widths })

-- | Create View from table + path (returns none if empty)
def fromTbl (tbl : T) (path : String)
    (col : Nat := 0) (grp : Array String := #[]) (row : Nat := 0) : Option (View T) := do
  let nCols := (TblOps.colNames tbl).size
  let nRows := TblOps.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then some (View.new (NavState.newAt tbl rfl rfl hr hc col grp row) path)
    else none
  else none

-- | Rebuild view with new table, preserving all attributes from old view.
-- Only nRows/nCols/nav change; everything else (vkind, disp, prec, etc.) is kept.
def rebuild (old : View T) (tbl : T) (col : Nat := old.nav.col.cur.val)
    (grp : Array String := old.nav.grp) (row : Nat := 0) : Option (View T) :=
  let nCols := (TblOps.colNames tbl).size
  let nRows := TblOps.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then
      let nav := NavState.newAt tbl rfl rfl hr hc col grp row
              |> NavState.hiddenL.set old.nav.hidden
      some { old with nRows, nCols, nav, widths := #[] }
    else none
  else none

-- | Pure update by command
def update (v : View T) (h : Cmd) (rowPg : Nat) : Option (View T × Effect) :=
  let n := v.nav; let names := n.colNames
  let curCol := n.curColIdx
  match h with
  | .sortAsc | .sortDesc =>
    let asc := h == .sortAsc
    let selIdxs := n.col.sels |>.filterMap names.idxOf?
    let grpIdxs := n.grp |>.filterMap names.idxOf?
    some (v, .sort curCol selIdxs grpIdxs asc)
  | .colExclude =>
    let name := n.curColName
    let cols := if n.hidden.isEmpty then #[name]
      else if n.hidden.contains name then n.hidden else n.hidden.push name
    some (v, .exclude cols)
  | _ => NavState.exec h n rowPg |>.map fun nav' =>
    let needsMore := nav'.row.cur.val + 1 >= v.nRows
      && TblOps.totalRows n.tbl > v.nRows
      && (h == .rowInc || h == .rowPgdn || h == .rowBot)
    ({ v with nav := nav' }, if needsMore then .fetchMore else .none)

end View

/-! ## ViewStack: non-empty view stack -/

-- | Non-empty view stack: hd = current, tl = parents (List for O(1) push/pop)
structure ViewStack (T : Type) [TblOps T] where
  hd : View T
  tl : List (View T) := []

namespace ViewStack

variable {T : Type} [TblOps T]

-- | Field lens for the current (head) view on the stack.
def hdL : Lens' (ViewStack T) (View T) := fieldL% hd

@[inline] def cur (s : ViewStack T) : View T := s.hd
@[inline] def tbl (s : ViewStack T) : T := s.hd.nav.tbl
@[inline] def hasParent (s : ViewStack T) : Bool := !s.tl.isEmpty
@[inline] def setCur (s : ViewStack T) (v : View T) : ViewStack T := { s with hd := v }
def push (s : ViewStack T) (v : View T) : ViewStack T := ⟨v, s.hd :: s.tl⟩

def pop (s : ViewStack T) : Option (ViewStack T) :=
  match s.tl with
  | hd :: tl => some ⟨hd, tl⟩
  | [] => none

def swap (s : ViewStack T) : ViewStack T :=
  match s.tl with
  | hd :: tl => ⟨hd, s.hd :: tl⟩
  | [] => s

def dup (s : ViewStack T) : ViewStack T := ⟨s.hd, s.hd :: s.tl⟩
def tabNames (s : ViewStack T) : Array String := (s.hd :: s.tl) |>.toArray |>.map (·.tabName)

-- | Pure update by command. q on empty stack → quit
def update (s : ViewStack T) (h : Cmd) : Option (ViewStack T × Effect) :=
  match h with
  | .stkDup => some (s.dup, .none)
  | .stkPop => match s.pop with
    | some s' => some (s', .none)
    | none => some (s, .quit)
  | .stkSwap => some (s.swap, .none)
  | _ => none

end ViewStack

end Tc
