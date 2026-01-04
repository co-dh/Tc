/-
  View: wraps NavState + metadata for unified Table type
  Uses closed sum Table (Type 0) instead of existential (Type 1).
-/
import Tc.Nav
import Tc.Table

namespace Tc

-- | View kind: how to render/interact
inductive ViewKind where
  | tbl                          -- table view
  | freqV (cols : Array String)  -- frequency view
  | colMeta                      -- column metadata
  | fld                          -- folder browser
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

namespace View

-- | Create from NavState + path
def new {nr nc : Nat} (nav : NavState nr nc Table) (path : String) : View :=
  ⟨nr, nc, nav, path, .tbl, "", 0, 0⟩

-- | Tab display name: custom disp or filename from path
@[inline] def tabName (v : View) : String :=
  if v.disp.isEmpty then v.path.splitOn "/" |>.getLast? |>.getD v.path else v.disp

-- | Render the view
@[inline] def doRender (v : View) (vs : ViewState) : IO ViewState :=
  render v.nav vs v.precAdj v.widthAdj

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

-- | Execute Cmd, returns Option View (none if table becomes empty after del)
def exec (v : View) (cmd : Cmd) (rowPg colPg : Nat) : Option View :=
  let n := v.nav; let names := ReadTable.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let mk tbl col grp row := preserve v (fromTbl tbl v.path col grp row)
  match cmd with
  | .colSel .del =>
    let (tbl', grp') := ModifyTable.del n.tbl curCol (n.col.sels.filterMap names.idxOf?) n.grp
    mk tbl' n.col.cur.val grp' 0
  | .colSel .sortAsc | .colSel .sortDesc =>
    let tbl' := ModifyTable.sort n.tbl curCol (n.grp.filterMap names.idxOf?) (cmd == .colSel .sortAsc)
    mk tbl' curCol n.grp n.row.cur.val
  | .prec verb  => some { v with precAdj := v.precAdj + verbDelta verb }
  | .width verb => some { v with widthAdj := v.widthAdj + verbDelta verb }
  | _ => match NavState.exec cmd n rowPg colPg with
    | some nav' => some { v with nav := nav' }
    | none => none

end View
end Tc
