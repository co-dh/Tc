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

end View
end Tc
