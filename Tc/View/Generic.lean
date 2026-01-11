/-
  Generic View: parameterized over table type T
  No ADBC imports - works with any T that has required typeclasses.
-/
import Tc.Effect
import Tc.Render

namespace Tc

-- | Generic View over table type T
structure GView (T : Type) [ReadTable T] [RenderTable T] where
  nRows : Nat
  nCols : Nat
  nav : NavState nRows nCols T
  path : String
  vkind : ViewKind := .tbl
  disp : String := ""
  precAdj : Int := 0
  widthAdj : Int := 0
  widths : Array Nat := #[]
  search : Option (Nat × String) := none

namespace GView

variable {T : Type} [ReadTable T] [ModifyTable T] [RenderTable T]

-- | Create from NavState + path
def new {nr nc : Nat} (nav : NavState nr nc T) (path : String) : GView T :=
  ⟨nr, nc, nav, path, .tbl, "", 0, 0, #[], none⟩

-- | Tab display name
@[inline] def tabName (v : GView T) : String :=
  match v.vkind with
  | .fld p _ => p
  | _ => if v.disp.isEmpty then v.path.splitOn "/" |>.getLast? |>.getD v.path else v.disp

-- | Render view
@[inline] def doRender (v : GView T) (vs : ViewState) (styles : Array UInt32) : IO (ViewState × GView T) := do
  let (vs', widths) ← render v.nav vs v.widths styles v.precAdj v.widthAdj
  pure (vs', { v with widths })

-- | Create View from table + path
def fromTbl (tbl : T) (path : String)
    (col : Nat := 0) (grp : Array String := #[]) (row : Nat := 0) : Option (GView T) := do
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then some (GView.new (NavState.newAt tbl rfl rfl hr hc col grp row) path)
    else none
  else none

-- | Verb to delta
private def verbDelta (verb : Verb) : Int := if verb == .inc then 1 else -1

-- | Preserve precAdj/widthAdj
private def preserve (v : GView T) (v' : Option (GView T)) : Option (GView T) :=
  v'.map fun x => { x with precAdj := v.precAdj, widthAdj := v.widthAdj }

-- | Pure update
def update (v : GView T) (cmd : Cmd) (rowPg : Nat) : Option (GView T × Effect) :=
  let n := v.nav; let names := ReadTable.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  match cmd with
  | .prec verb  => some ({ v with precAdj := v.precAdj + verbDelta verb }, .none)
  | .width verb => some ({ v with widthAdj := v.widthAdj + verbDelta verb }, .none)
  | .colSel .del =>
    let sels := n.col.sels.filterMap names.idxOf?
    some (v, .queryDel curCol sels n.grp)
  | .colSel .inc => some (v, .querySort curCol (n.grp.filterMap names.idxOf?) true)
  | .colSel .dec => some (v, .querySort curCol (n.grp.filterMap names.idxOf?) false)
  | _ => (NavState.exec cmd n rowPg colPageSize).map fun nav' => ({ v with nav := nav' }, .none)

-- | Execute Cmd with IO
def exec (v : GView T) (cmd : Cmd) : IO (Option (GView T)) := do
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

end GView

-- | Generic ViewStack
abbrev GViewStack (T : Type) [ReadTable T] [RenderTable T] := { a : Array (GView T) // a.size > 0 }

namespace GViewStack

variable {T : Type} [ReadTable T] [ModifyTable T] [RenderTable T]

@[inline] def cur (s : GViewStack T) : GView T := s.val[0]'s.property
@[inline] def views (s : GViewStack T) : Array (GView T) := s.val
@[inline] def hasParent (s : GViewStack T) : Bool := s.val.size > 1

def setCur (s : GViewStack T) (v : GView T) : GViewStack T :=
  ⟨s.val.set (Fin.mk 0 s.property) v, by simp [Array.size_set]; exact s.property⟩

def push (s : GViewStack T) (v : GView T) : GViewStack T :=
  ⟨#[v] ++ s.val, by simp; omega⟩

def pop (s : GViewStack T) : Option (GViewStack T) :=
  if h : s.val.size > 1 then some ⟨s.val.extract 1 s.val.size, by simp [Array.size_extract]; omega⟩
  else none

def swap (s : GViewStack T) : GViewStack T :=
  if h : s.val.size > 1 then
    let a := s.val[0]'s.property
    let b := s.val[1]'h
    ⟨#[b, a] ++ s.val.extract 2 s.val.size, by simp; omega⟩
  else s

def dup (s : GViewStack T) : GViewStack T :=
  ⟨#[s.cur] ++ s.val, by simp; omega⟩

def tabNames (s : GViewStack T) : Array String := s.views.map (·.tabName)

-- | Move row cursor to target index
def moveRowTo (s : GViewStack T) (rowIdx : Nat) (search : Option (Nat × String) := none) : GViewStack T :=
  let v := s.cur
  let delta : Int := rowIdx - v.nav.row.cur.val
  let nav' := { v.nav with row := { v.nav.row with cur := v.nav.row.cur.clamp delta } }
  s.setCur { v with nav := nav', search := search.orElse (fun _ => v.search) }

-- | Move col cursor to target index
def moveColTo (s : GViewStack T) (colIdx : Nat) : GViewStack T :=
  let v := s.cur
  let delta : Int := colIdx - v.nav.col.cur.val
  let nav' := { v.nav with col := { v.nav.col with cur := v.nav.col.cur.clamp delta } }
  s.setCur { v with nav := nav' }

end GViewStack

end Tc
