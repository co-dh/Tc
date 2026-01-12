/-
  View: wraps NavState + metadata for table type T
  Generic over T to support different build variants (Core, DuckDB, Full).
-/
import Tc.Effect
import Tc.Render
import Tc.Data.Mem.Text

namespace Tc

-- | View: wraps NavState for table type T
structure View (T : Type) [ReadTable T] where
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

variable {T : Type} [ReadTable T]

-- | Create from NavState + path
def new {nr nc : Nat} (nav : NavState nr nc T) (path : String) : View T :=
  ⟨nr, nc, nav, path, .tbl, "", 0, 0, #[], none⟩

-- | Tab display name: custom disp or filename from path
@[inline] def tabName (v : View T) : String :=
  match v.vkind with
  | .fld p _ => p  -- folder: show full path
  | _ => if v.disp.isEmpty then v.path.splitOn "/" |>.getLast? |>.getD v.path else v.disp

-- | Render the view, returns (ViewState, updated View with new widths)
@[inline] def doRender [RenderTable T] (v : View T) (vs : ViewState) (styles : Array UInt32) : IO (ViewState × View T) := do
  let (vs', widths) ← render v.nav vs v.widths styles v.precAdj v.widthAdj
  pure (vs', { v with widths })

-- | Create View from table + path (returns none if empty)
def fromTbl (tbl : T) (path : String)
    (col : Nat := 0) (grp : Array String := #[]) (row : Nat := 0) : Option (View T) := do
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
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
private def preserve (v : View T) (v' : Option (View T)) : Option (View T) :=
  v'.map fun x => { x with precAdj := v.precAdj, widthAdj := v.widthAdj }

-- | Pure update: returns (new view, effect). IO ops return Effect to defer.
def update (v : View T) (cmd : Cmd) (rowPg : Nat) : Option (View T × Effect) :=
  let n := v.nav; let names := ReadTable.colNames n.tbl
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
  | .colSel .inc => some (v, .querySort curCol (n.grp.filterMap names.idxOf?) true)
  | .colSel .dec => some (v, .querySort curCol (n.grp.filterMap names.idxOf?) false)
  -- pure: navigation
  | _ => (NavState.exec cmd n rowPg colPageSize).map fun nav' => ({ v with nav := nav' }, .none)

instance : Update (View T) where update v cmd := update v cmd defaultRowPg
  where defaultRowPg := 20  -- will be overridden by Runner with actual height

-- | width update: .inc adds 1, .dec subtracts 1
theorem width_inc_adds (v : View T) (rowPg : Nat) :
    (update v (.width .inc) rowPg).map (fun p => p.1.widthAdj) = some (v.widthAdj + 1) := rfl
theorem width_dec_subs (v : View T) (rowPg : Nat) :
    (update v (.width .dec) rowPg).map (fun p => p.1.widthAdj) = some (v.widthAdj - 1) := rfl

-- | Execute Cmd, returns IO (Option View) (for backward compat)
def exec [ModifyTable T] (v : View T) (cmd : Cmd) : IO (Option (View T)) := do
  let n := v.nav; let names := ReadTable.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let mk tbl col grp row := preserve v (fromTbl tbl v.path col grp row)
  let rowPg := ((← Term.height).toNat - reservedLines) / 2
  match cmd with
  | .colSel .del =>
    let (tbl', grp') ← ModifyTable.del n.tbl curCol (n.col.sels.filterMap names.idxOf?) n.grp
    pure (mk tbl' n.col.cur.val grp' 0)
  | .colSel .inc | .colSel .dec =>
    let tbl' ← ModifyTable.sort n.tbl curCol (n.grp.filterMap names.idxOf?) (cmd == .colSel .inc)
    pure (mk tbl' curCol n.grp n.row.cur.val)
  | .prec verb  => pure (some { v with precAdj := v.precAdj + verbDelta verb })
  | .width verb => pure (some { v with widthAdj := v.widthAdj + verbDelta verb })
  | _ => pure <| match NavState.exec cmd n rowPg colPageSize with
    | some nav' => some { v with nav := nav' }
    | none => none

instance [ModifyTable T] : Exec (View T) where exec := exec

end View

/-! ## ViewStack: non-empty view stack -/

-- | Non-empty view stack: a[0] = current, a[1:] = parents
abbrev ViewStack (T : Type) [ReadTable T] := { a : Array (View T) // a.size > 0 }

namespace ViewStack

variable {T : Type} [ReadTable T]

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

end ViewStack

end Tc
