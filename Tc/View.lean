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
  instQ : QueryMeta t
  instF : QueryFreq t
  instL : QueryFilter t
  instD : QueryDistinct t
  nav : NavState nRows nCols t
  path : String              -- source file/command (for tab display)
  vkind : ViewKind := .tbl
  disp : String := ""        -- custom display name (overrides filename)
  precAdj : Int := 0         -- precision adjustment (-=fewer, +=more decimals)
  widthAdj : Int := 0        -- width adjustment offset (-=narrower, +=wider)

namespace View

-- | Create from NavState + path (infers instances)
def new {nr nc : Nat} {τ : Type} [ir : ReadTable τ] [im : ModifyTable τ] [iv : RenderTable τ]
    [iq : QueryMeta τ] [if_ : QueryFreq τ] [il : QueryFilter τ] [id_ : QueryDistinct τ]
    (nav : NavState nr nc τ) (path : String) : View :=
  ⟨nr, nc, τ, ir, im, iv, iq, if_, il, id_, nav, path, .tbl, "", 0, 0⟩

-- | Tab display name: custom disp or filename from path
@[inline] def tabName (v : View) : String :=
  if v.disp.isEmpty then v.path.splitOn "/" |>.getLast? |>.getD v.path else v.disp

@[inline] def doRender (v : View) (vs : ViewState) : IO ViewState :=
  @render v.nRows v.nCols v.t v.instR v.instV v.nav vs v.precAdj v.widthAdj

-- | Create View from table + path (returns none if empty)
def fromTbl {τ : Type} [ReadTable τ] [ModifyTable τ] [RenderTable τ] [QueryMeta τ] [QueryFreq τ]
    [QueryFilter τ] [QueryDistinct τ] (tbl : τ) (path : String)
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
  letI : ReadTable v.t := v.instR; letI : ModifyTable v.t := v.instM
  letI : RenderTable v.t := v.instV; letI : QueryMeta v.t := v.instQ
  letI : QueryFreq v.t := v.instF; letI : QueryFilter v.t := v.instL
  letI : QueryDistinct v.t := v.instD
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
