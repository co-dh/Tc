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
theorem verbDelta_inc : verbDelta .inc = 1 := rfl
theorem verbDelta_dec : verbDelta .dec = -1 := rfl

-- | Preserve precAdj/widthAdj when recreating View
-- | Preserve view properties (vkind, disp, precAdj, widthAdj) when recreating
private def preserve (v : View T) (v' : Option (View T)) : Option (View T) :=
  v'.map fun x => { x with vkind := v.vkind, disp := v.disp, precAdj := v.precAdj, widthAdj := v.widthAdj }

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
    some (v, .queryDel curCol sels n.grp)
  -- effect: sort (runner will execute and rebuild view)
  | .colSel .inc =>
    let selIdxs := n.col.sels.filterMap names.idxOf?
    let grpIdxs := n.grp.filterMap names.idxOf?
    some (v, .querySort curCol selIdxs grpIdxs true)
  | .colSel .dec =>
    let selIdxs := n.col.sels.filterMap names.idxOf?
    let grpIdxs := n.grp.filterMap names.idxOf?
    some (v, .querySort curCol selIdxs grpIdxs false)
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

-- | Execute Cmd, returns IO (Option View) (for backward compat)
-- Delegates to pure update for non-IO cases; only .colSel .del/.inc/.dec need IO
def exec [ModifyTable T] (v : View T) (cmd : Cmd) : IO (Option (View T)) := do
  let n := v.nav; let names := TblOps.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let mk tbl col grp row := preserve v (fromTbl tbl v.path col grp row)
  match cmd with
  | .colSel .del =>
    let (tbl', grp') ← ModifyTable.del n.tbl curCol (n.col.sels.filterMap names.idxOf?) n.grp
    pure (mk tbl' n.col.cur.val grp' 0)
  | .colSel .inc | .colSel .dec =>
    let selIdxs := n.col.sels.filterMap names.idxOf?
    let grpIdxs := n.grp.filterMap names.idxOf?
    let tbl' ← ModifyTable.sort n.tbl curCol selIdxs grpIdxs (cmd == .colSel .inc)
    pure (mk tbl' curCol n.grp n.row.cur.val)
  | _ => -- delegate to pure update
    let rowPg := ((← Term.height).toNat - reservedLines) / 2
    pure <| (update v cmd rowPg).map Prod.fst

instance [ModifyTable T] : Exec (View T) where exec := exec

end View

/-! ## ViewStack: non-empty view stack -/

-- | Non-empty view stack: a[0] = current, a[1:] = parents
abbrev ViewStack (T : Type) [TblOps T] := { a : Array (View T) // a.size > 0 }

namespace ViewStack

variable {T : Type} [TblOps T]

-- | Current view
@[inline] def cur (s : ViewStack T) : View T := s.val[0]'s.property

-- | All views
@[inline] def views (s : ViewStack T) : Array (View T) := s.val

-- | Has parent?
@[inline] def hasParent (s : ViewStack T) : Bool := s.val.size > 1

-- | Update current view
def setCur (s : ViewStack T) (v : View T) : ViewStack T :=
  ⟨s.val.set (Fin.mk 0 s.property) v, by simp [Array.size_set]; exact s.property⟩

-- | Push new view (current becomes parent)
def push (s : ViewStack T) (v : View T) : ViewStack T :=
  ⟨#[v] ++ s.val, by simp; omega⟩

-- | Pop view (returns to parent, or none if no parent)
def pop (s : ViewStack T) : Option (ViewStack T) :=
  if h : s.val.size > 1 then some ⟨s.val.extract 1 s.val.size, by simp [Array.size_extract]; omega⟩
  else none

-- | Swap top two views
def swap (s : ViewStack T) : ViewStack T :=
  if h : s.val.size > 1 then
    let a := s.val[0]'s.property
    let b := s.val[1]'h
    ⟨#[b, a] ++ s.val.extract 2 s.val.size, by simp; omega⟩
  else s

-- | Duplicate current view (push copy of current)
def dup (s : ViewStack T) : ViewStack T :=
  ⟨#[s.cur] ++ s.val, by simp; omega⟩

-- | Tab names for display (current first)
def tabNames (s : ViewStack T) : Array String := s.views.map (·.tabName)

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

-- | IO wrapper (for backward compat)
def exec (s : ViewStack T) (cmd : Cmd) : IO (Option (ViewStack T)) := pure <|
  match update s cmd with
  | some (s', _) => some s' | none => none

instance : Exec (ViewStack T) where exec := exec

end ViewStack

end Tc
