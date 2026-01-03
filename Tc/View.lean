/-
  View: existential wrapper for NavState + metadata
-/
import Tc.Nav
import Tc.Render

namespace Tc

-- | View kind: how to render/interact
inductive ViewKind where
  | tbl                          -- table view
  | freqV (cols : Array String)  -- frequency view
  | colMeta                      -- column metadata
  | fld                          -- folder browser
  deriving Inhabited, Repr, BEq

-- | View: existential wrapper for NavState + metadata
structure View where
  nRows : Nat
  nCols : Nat
  t : Type
  instR : ReadTable t
  instM : ModifyTable t
  instV : RenderTable t
  nav : NavState nRows nCols t
  path : String              -- source file/command (for tab display)
  vkind : ViewKind := .tbl
  disp : String := ""        -- custom display name (overrides filename)

namespace View

-- | Create from NavState + path (infers instances)
def new {nr nc : Nat} {τ : Type} [ir : ReadTable τ] [im : ModifyTable τ] [iv : RenderTable τ]
    (nav : NavState nr nc τ) (path : String) : View :=
  ⟨nr, nc, τ, ir, im, iv, nav, path, .tbl, ""⟩

-- | Tab display name: custom disp or filename from path
@[inline] def tabName (v : View) : String :=
  if v.disp.isEmpty then v.path.splitOn "/" |>.getLast? |>.getD v.path else v.disp

@[inline] def doRender (v : View) (vs : ViewState) : IO ViewState :=
  @render v.nRows v.nCols v.t v.instR v.instV v.nav vs

@[inline] def tbl (v : View) : v.t := @NavState.tbl v.t v.instR v.nRows v.nCols v.nav
@[inline] def colNames (v : View) : Array String := @ReadTable.colNames v.t v.instR v.tbl
@[inline] def curRow (v : View) : Nat := @NavState.curRow v.t v.instR v.nRows v.nCols v.nav
@[inline] def curDispCol (v : View) : Nat := @NavState.curDispCol v.t v.instR v.nRows v.nCols v.nav
@[inline] def curColIdx (v : View) : Nat := @NavState.curColIdx v.t v.instR v.nRows v.nCols v.nav
@[inline] def getGroup (v : View) : Array String := @NavState.group v.t v.instR v.nRows v.nCols v.nav
@[inline] def selColIdxs (v : View) : Array Nat := @NavState.selColIdxs v.t v.instR v.nRows v.nCols v.nav

@[inline] def dispatch (v : View) (cmd : Cmd) (rowPg colPg : Nat) : View :=
  { v with nav := @NavState.dispatch v.t v.instR v.nRows v.nCols cmd v.nav rowPg colPg }

-- | Create View from table + path (returns none if empty)
def fromTbl {τ : Type} [ReadTable τ] [ModifyTable τ] [RenderTable τ]
    (tbl : τ) (path : String) (col : Nat := 0) (grp : Array String := #[]) (row : Nat := 0)
    : Option View := do
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then some (View.new (NavState.newAt tbl rfl rfl hr hc col grp row) path)
    else none
  else none

-- | Execute Cmd, returns Option View (none if table becomes empty after del)
def exec (v : View) (cmd : Cmd) (rowPg colPg : Nat) : Option View :=
  match cmd with
  | .col .del =>
    let tbl' := @ModifyTable.del v.t v.instM v.tbl v.curColIdx v.selColIdxs v.getGroup
    @fromTbl v.t v.instR v.instM v.instV tbl'.1 v.path v.curDispCol tbl'.2 0
  | .colSel .sortAsc =>
    let grpIdxs := v.getGroup.filterMap v.colNames.idxOf?
    let tbl' := @ModifyTable.sort v.t v.instM v.tbl v.curColIdx grpIdxs true
    @fromTbl v.t v.instR v.instM v.instV tbl' v.path v.curColIdx v.getGroup v.curRow
  | .colSel .sortDesc =>
    let grpIdxs := v.getGroup.filterMap v.colNames.idxOf?
    let tbl' := @ModifyTable.sort v.t v.instM v.tbl v.curColIdx grpIdxs false
    @fromTbl v.t v.instR v.instM v.instV tbl' v.path v.curColIdx v.getGroup v.curRow
  | _ => some (v.dispatch cmd rowPg colPg)

end View
end Tc
