/-
  View: wraps NavState + metadata for unified Table type
  Uses closed sum Table (Type 0) instead of existential (Type 1).
-/
import Tc.Nav
import Tc.Render
import Tc.Table

namespace Tc

-- | View kind: how to render/interact
inductive ViewKind where
  | tbl                                  -- table view
  | freqV (cols : Array String)          -- frequency view
  | colMeta                              -- column metadata
  | fld (path : String) (depth : Nat)    -- folder browser: path + find depth
  deriving Inhabited, Repr, BEq

-- | View: wraps NavState for unified Table type
structure View where
  nRows : Nat
  nCols : Nat
  nav : NavState nRows nCols Table
  path : String              -- source file/command (for tab display)
  vkind : ViewKind := .tbl
  disp : String := ""        -- custom display name (overrides filename)
  precAdj : Int := 0         -- precision adjustment (-=fewer, +=more decimals)
  widthAdj : Int := 0        -- width adjustment offset (-=narrower, +=wider)
  widths : Array Nat := #[]  -- cached column widths (per-view for type safety)
  search : Option (Nat × String) := none  -- last search: (colIdx, value)

namespace View

-- | Create from NavState + path
def new {nr nc : Nat} (nav : NavState nr nc Table) (path : String) : View :=
  ⟨nr, nc, nav, path, .tbl, "", 0, 0, #[], none⟩

-- | Tab display name: custom disp or filename from path
@[inline] def tabName (v : View) : String :=
  match v.vkind with
  | .fld p _ => p  -- folder: show full path
  | _ => if v.disp.isEmpty then v.path.splitOn "/" |>.getLast? |>.getD v.path else v.disp

-- | Render the view, returns (ViewState, updated View with new widths)
@[inline] def doRender (v : View) (vs : ViewState) (styles : Array UInt32) : IO (ViewState × View) := do
  let (vs', widths) ← render v.nav vs v.widths styles v.precAdj v.widthAdj
  pure (vs', { v with widths })

-- | Create View from Table + path (returns none if empty)
def fromTbl (tbl : Table) (path : String)
    (col : Nat := 0) (grp : Array String := #[]) (row : Nat := 0) : Option View := do
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then some (View.new (NavState.newAt tbl rfl rfl hr hc col grp row) path)
    else none
  else none

-- | Verb to delta: inc=+1, dec=-1
private def verbDelta (verb : Verb) : Int := if verb == .inc then 1 else -1

-- | Preserve precAdj/widthAdj when recreating View
private def preserve (v : View) (v' : Option View) : Option View :=
  v'.map fun x => { x with precAdj := v.precAdj, widthAdj := v.widthAdj }

-- | Execute Cmd, returns IO (Option View) (none if table becomes empty after del)
def exec (v : View) (cmd : Cmd) : IO (Option View) := do
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

instance : Exec View where exec := exec

end View

/-! ## ViewStack: non-empty view stack -/

-- | Non-empty view stack: a[0] = current, a[1:] = parents
abbrev ViewStack := { a : Array View // a.size > 0 }

namespace ViewStack

-- | Current view
@[inline] def cur (s : ViewStack) : View := s.val[0]'s.property

-- | All views
@[inline] def views (s : ViewStack) : Array View := s.val

-- | Has parent?
@[inline] def hasParent (s : ViewStack) : Bool := s.val.size > 1

-- | Update current view
def setCur (s : ViewStack) (v : View) : ViewStack :=
  ⟨s.val.set (Fin.mk 0 s.property) v, by simp [Array.size_set]; exact s.property⟩

-- | Push new view (current becomes parent)
def push (s : ViewStack) (v : View) : ViewStack :=
  ⟨#[v] ++ s.val, by simp; omega⟩

-- | Pop view (returns to parent, or none if no parent)
def pop (s : ViewStack) : Option ViewStack :=
  if h : s.val.size > 1 then some ⟨s.val.extract 1 s.val.size, by simp [Array.size_extract]; omega⟩
  else none

-- | Swap top two views
def swap (s : ViewStack) : ViewStack :=
  if h : s.val.size > 1 then
    let a := s.val[0]'s.property
    let b := s.val[1]'h
    ⟨#[b, a] ++ s.val.extract 2 s.val.size, by simp; omega⟩
  else s

-- | Duplicate current view (push copy of current)
def dup (s : ViewStack) : ViewStack :=
  ⟨#[s.cur] ++ s.val, by simp; omega⟩

-- | Tab names for display (current first)
def tabNames (s : ViewStack) : Array String := s.views.map (·.tabName)

end ViewStack

end Tc
