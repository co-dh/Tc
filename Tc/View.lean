/-
  View: wraps NavState + metadata for table type T
  Generic over T to support different build variants (Core, DuckDB, Full).
-/
import Tc.Cmd
import Tc.Render

namespace Tc

-- | View: wraps NavState for table type T
structure View (T : Type) [TblOps T] where
  nRows : Nat
  nCols : Nat
  nav : NavState nRows nCols T
  path : String              -- source file/command (for tab display)
  vkind : ViewKind := .tbl
  disp : String := ""        -- custom display name (overrides filename)
  precAdj : Int := 0         -- precision adjustment (-=fewer, +=more decimals)
  widthAdj : Int := 0        -- width adjustment offset (-=narrower, +=wider)
  widths : Array Nat := #[]  -- cached column widths (per-view for type safety)
  search : Option (Nat × String) := none  -- last search: (colIdx, value)

namespace View

variable {T : Type} [TblOps T]

-- | Create from NavState + path
def new {nr nc : Nat} (nav : NavState nr nc T) (path : String) : View T :=
  ⟨nr, nc, nav, path, .tbl, "", 0, 0, #[], none⟩

-- | Tab display name: custom disp or filename from path
@[inline] def tabName (v : View T) : String :=
  match v.vkind with
  | .fld p _ => p  -- folder: show full path
  | _ => if v.disp.isEmpty then v.path.splitOn "/" |>.getLast? |>.getD v.path else v.disp

-- | Render the view, returns (ViewState, updated View with new widths)
@[inline] def doRender (v : View T) (vs : ViewState) (styles : Array UInt32) : IO (ViewState × View T) := do
  let (vs', widths) ← render v.nav vs v.widths styles v.precAdj v.widthAdj v.vkind
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

-- | Verb to delta: inc=+1, dec=-1
private def verbDelta (verb : Verb) : Int := if verb == .inc then 1 else -1

-- | verbDelta theorems: .inc → +1, .dec → -1
@[simp] theorem verbDelta_inc : verbDelta .inc = 1 := rfl
@[simp] theorem verbDelta_dec : verbDelta .dec = -1 := rfl

-- | Rebuild view with new table, carrying forward display settings
def rebuild (old : View T) (tbl : T) (col : Nat := old.nav.col.cur.val)
    (grp : Array String := old.nav.grp) (row : Nat := 0) : Option (View T) :=
  (View.fromTbl tbl old.path col grp row).map fun v =>
    { v with precAdj := old.precAdj, widthAdj := old.widthAdj, search := old.search }

-- | Pure update: returns (new view, effect). IO ops return Effect to defer.
def update (v : View T) (cmd : Cmd) (rowPg : Nat) : Option (View T × Effect) :=
  let n := v.nav; let names := TblOps.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  match cmd with
  -- pure: precision/width adjustment
  | .prec verb  => some ({ v with precAdj := v.precAdj + verbDelta verb }, .none)
  | .width verb => some ({ v with widthAdj := v.widthAdj + verbDelta verb }, .none)
  -- effect: column delete (runner will execute and rebuild view)
  | .colSel .del =>
    let sels := n.col.sels.filterMap names.idxOf?
    some (v, .query (.del curCol sels n.grp))
  -- effect: sort (runner will execute and rebuild view)
  | .colSel .inc =>
    let selIdxs := n.col.sels.filterMap names.idxOf?
    let grpIdxs := n.grp.filterMap names.idxOf?
    some (v, .query (.sort curCol selIdxs grpIdxs true))
  | .colSel .dec =>
    let selIdxs := n.col.sels.filterMap names.idxOf?
    let grpIdxs := n.grp.filterMap names.idxOf?
    some (v, .query (.sort curCol selIdxs grpIdxs false))
  -- pure: navigation (detect at-bottom for fetchMore on downward scroll)
  | _ => (NavState.exec cmd n rowPg colPageSize).map fun nav' =>
    let needsMore := nav'.row.cur.val + 1 >= v.nRows
      && TblOps.totalRows n.tbl > v.nRows
      && (cmd matches .row .inc | .vPage .inc | .ver .inc)
    ({ v with nav := nav' }, if needsMore then .fetchMore else .none)

instance : Update (View T) where update v cmd := update v cmd defaultRowPg

-- | width update: .inc adds 1, .dec subtracts 1
theorem width_inc_adds (v : View T) (rowPg : Nat) :
    (update v (.width .inc) rowPg).map (fun p => p.1.widthAdj) = some (v.widthAdj + 1) := rfl
theorem width_dec_subs (v : View T) (rowPg : Nat) :
    (update v (.width .dec) rowPg).map (fun p => p.1.widthAdj) = some (v.widthAdj - 1) := rfl


end View

/-! ## ViewStack: non-empty view stack -/

-- | Non-empty view stack: hd = current, tl = parents (List for O(1) push/pop)
structure ViewStack (T : Type) [TblOps T] where
  hd : View T
  tl : List (View T) := []

namespace ViewStack

variable {T : Type} [TblOps T]

@[inline] def cur (s : ViewStack T) : View T := s.hd
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
def tabNames (s : ViewStack T) : Array String := (s.hd :: s.tl).toArray.map (·.tabName)

-- | Pure update: returns (new stack, effect). q on empty stack → quit
def update (s : ViewStack T) (cmd : Cmd) : Option (ViewStack T × Effect) :=
  match cmd with
  | .stk .inc | .stk .dup => some (s.dup, .none)
  | .stk .dec => match s.pop with
    | some s' => some (s', .none)
    | none => some (s, .quit)
  | .stk .ent => some (s.swap, .none)
  | _ => none

instance : Update (ViewStack T) where update := update

end ViewStack

end Tc
