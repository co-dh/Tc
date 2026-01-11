/-
  View (Core): CSV-only version without ADBC
  Uses Table.Mem instead of full Table
-/
import Tc.Effect
import Tc.Render
import Tc.Table.Mem
import Tc.Data.Mem.Text

namespace Tc

-- | View: wraps NavState for MemTable only
structure View where
  nRows : Nat
  nCols : Nat
  nav : NavState nRows nCols Table
  path : String
  vkind : ViewKind := .tbl
  disp : String := ""
  precAdj : Int := 0
  widthAdj : Int := 0
  widths : Array Nat := #[]
  search : Option (Nat × String) := none

namespace View

-- | Create from NavState + path
def new {nr nc : Nat} (nav : NavState nr nc Table) (path : String) : View :=
  ⟨nr, nc, nav, path, .tbl, "", 0, 0, #[], none⟩

-- | Tab display name
@[inline] def tabName (v : View) : String :=
  match v.vkind with
  | .fld p _ => p
  | _ => if v.disp.isEmpty then v.path.splitOn "/" |>.getLast? |>.getD v.path else v.disp

-- | Render view
@[inline] def doRender (v : View) (vs : ViewState) (styles : Array UInt32) : IO (ViewState × View) := do
  let (vs', widths) ← render v.nav vs v.widths styles v.precAdj v.widthAdj
  pure (vs', { v with widths })

-- | Create View from Table + path
def fromTbl (tbl : Table) (path : String)
    (col : Nat := 0) (grp : Array String := #[]) (row : Nat := 0) : Option View := do
  let nCols := (ReadTable.colNames tbl).size
  let nRows := ReadTable.nRows tbl
  if hc : nCols > 0 then
    if hr : nRows > 0 then some (View.new (NavState.newAt tbl rfl rfl hr hc col grp row) path)
    else none
  else none

-- | Create View from file path (CSV only, no parquet)
def fromFile (path : String) : IO (Option View) := do
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .error e => IO.eprintln s!"CSV load error: {e}"; pure none
    | .ok tbl => pure (fromTbl (.mem tbl) path)
  else
    IO.eprintln s!"Unsupported format: {path} (core build only supports .csv)"
    pure none

-- | Verb to delta
private def verbDelta (verb : Verb) : Int := if verb == .inc then 1 else -1

-- | Preserve precAdj/widthAdj when recreating View
private def preserve (v : View) (v' : Option View) : Option View :=
  v'.map fun x => { x with precAdj := v.precAdj, widthAdj := v.widthAdj }

-- | Pure update
def update (v : View) (cmd : Cmd) (rowPg : Nat) : Option (View × Effect) :=
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

instance : Update View where update v cmd := update v cmd defaultRowPg
  where defaultRowPg := 20

-- | Execute Cmd
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

-- | Non-empty view stack
abbrev ViewStack := { a : Array View // a.size > 0 }

namespace ViewStack

@[inline] def cur (s : ViewStack) : View := s.val[0]'s.property
@[inline] def views (s : ViewStack) : Array View := s.val
@[inline] def hasParent (s : ViewStack) : Bool := s.val.size > 1

def setCur (s : ViewStack) (v : View) : ViewStack :=
  ⟨s.val.set (Fin.mk 0 s.property) v, by simp [Array.size_set]; exact s.property⟩

def push (s : ViewStack) (v : View) : ViewStack := ⟨#[v] ++ s.val, by simp; omega⟩

def pop (s : ViewStack) : Option ViewStack :=
  if h : s.val.size > 1 then some ⟨s.val.extract 1 s.val.size, by simp [Array.size_extract]; omega⟩
  else none

def swap (s : ViewStack) : ViewStack :=
  if h : s.val.size > 1 then
    let a := s.val[0]'s.property
    let b := s.val[1]'h
    ⟨#[b, a] ++ s.val.extract 2 s.val.size, by simp; omega⟩
  else s

def dup (s : ViewStack) : ViewStack := ⟨#[s.cur] ++ s.val, by simp; omega⟩

def tabNames (s : ViewStack) : Array String := s.views.map (·.tabName)

end ViewStack

end Tc
