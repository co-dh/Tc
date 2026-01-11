/-
  View (Core): CSV-only version without ADBC
  Uses Table.Mem instead of full Table
-/
import Tc.Effect
import Tc.Fzf
import Tc.Render
import Tc.Table.Mem
import Tc.Data.Mem.Text
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq

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

/-! ## Meta: column statistics -/

namespace Meta

def headers : Array String := #["column", "type", "cnt", "dist", "null%", "min", "max"]
def colDist : Nat := 3
def colNull : Nat := 4

-- | Convert queryMeta result to MemTable
def toMemTable (m : MetaTuple) : MemTable :=
  let (names, types, cnts, dists, nulls, mins, maxs) := m
  ⟨headers, #[.strs names, .strs types, .ints cnts, .ints dists, .ints nulls, .strs mins, .strs maxs]⟩

-- | Get int from column at row
def getInt (t : MemTable) (col row : Nat) : Int64 :=
  match t.cols.getD col default with
  | .ints data => data.getD row 0
  | _ => 0

-- | Select rows where column satisfies predicate
def selectRows (t : MemTable) (col : Nat) (pred : Int64 → Bool) : Array Nat :=
  (Array.range (MemTable.nRows t)).filter fun r => pred (getInt t col r)

def selNull (t : MemTable) : Array Nat := selectRows t colNull (· == 100)
def selSingle (t : MemTable) : Array Nat := selectRows t colDist (· == 1)

-- | Get column names from selected rows
def selNames (t : MemTable) (selRows : Array Nat) : Array String :=
  selRows.filterMap fun r =>
    match t.cols.getD 0 default with
    | .strs data => some (data.getD r "")
    | _ => none

-- | Push meta view
def push (s : ViewStack) : IO (Option ViewStack) := do
  let tbl ← QueryTable.queryMeta s.cur.nav.tbl <&> toMemTable
  let some v := View.fromTbl (.mem tbl) s.cur.path | return none
  return some (s.push { v with vkind := .colMeta, disp := "meta" })

-- | Select rows by predicate
def sel (s : ViewStack) (f : MemTable → Array Nat) : ViewStack :=
  if s.cur.vkind != .colMeta then s else
  match s.cur.nav.tbl with
  | .mem tbl =>
    let rows := f tbl
    let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
    s.setCur { s.cur with nav := nav' }

-- | Set key cols from meta selections, pop to parent
def setKey (s : ViewStack) : Option ViewStack :=
  if s.cur.vkind != .colMeta then some s else
  if !s.hasParent then some s else
  match s.cur.nav.tbl with
  | .mem tbl =>
    let colNames := selNames tbl s.cur.nav.row.sels
    match s.pop with
    | some s' =>
      let nav' := { s'.cur.nav with grp := colNames, col := { s'.cur.nav.col with sels := colNames } }
      some (s'.setCur { s'.cur with nav := nav' })
    | none => some s

-- | Execute meta command
def exec (s : ViewStack) (cmd : Cmd) : IO (Option ViewStack) := do
  match cmd with
  | .metaV .dup => (← push s).orElse (fun _ => some s) |> pure
  | .metaV .dec => pure (some (sel s selNull))
  | .metaV .inc => pure (some (sel s selSingle))
  | .metaV .ent => if s.cur.vkind == .colMeta then pure (setKey s) else pure none
  | _ => pure none

end Meta

/-! ## Freq: frequency distribution -/

namespace Freq

-- | Convert queryFreq result to MemTable (sorted by Cnt desc)
def toMemTable (freq : FreqTuple) : MemTable :=
  let (keyNames, keyCols, cntData, pctData, barData) := freq
  let names := keyNames ++ #["Cnt", "Pct", "Bar"]
  let cols := keyCols ++ #[.ints cntData, .floats pctData, .strs barData]
  MemTable.sort ⟨names, cols⟩ #[keyCols.size] false

-- | Check if view is freq view
def isFreq : ViewKind → Bool
  | .freqV _ => true
  | _ => false

-- | Push freq view
def push (s : ViewStack) : IO (Option ViewStack) := do
  let n := s.cur.nav; let names := ReadTable.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  let colIdxs := colNames.filterMap names.idxOf?
  let freq ← QueryTable.queryFreq n.tbl colIdxs
  let tbl := toMemTable freq
  let some v := View.fromTbl (.mem tbl) s.cur.path 0 colNames | return none
  return some (s.push { v with vkind := .freqV colNames, disp := s!"freq {colNames.join ","}" })

-- | Build filter expression from freq row
def filterExpr (tbl : MemTable) (cols : Array String) (row : Nat) : String :=
  let vals := cols.mapIdx fun i _ =>
    match tbl.cols.getD i default with
    | .strs data => s!"'{data.getD row ""}'"
    | .ints data => s!"{data.getD row 0}"
    | .floats data => s!"{data.getD row 0}"
  let exprs := cols.zip vals |>.map fun (c, v) => s!"{c} == {v}"
  " && ".intercalate exprs.toList

-- | Filter parent by selected freq row
def filter (s : ViewStack) : IO (Option ViewStack) := do
  let .freqV cols := s.cur.vkind | return some s
  if !s.hasParent then return some s
  let .mem tbl := s.cur.nav.tbl  -- Table.Mem only has .mem
  let some s' := s.pop | return some s
  let expr := filterExpr tbl cols s.cur.nav.row.cur.val
  let some tbl' ← QueryTable.filter s'.cur.nav.tbl expr | return some s'
  let some v := View.fromTbl tbl' s'.cur.path 0 s'.cur.nav.grp 0 | return some s'
  return some (s'.push v)

-- | Execute freq command
def exec (s : ViewStack) (cmd : Cmd) : IO (Option ViewStack) := do
  match cmd with
  | .freq .dup => (← push s).orElse (fun _ => some s) |> pure
  | .freq .ent => match s.cur.vkind with
    | .freqV _ => filter s
    | _ => pure none
  | _ => pure none

end Freq

/-! ## Search/Filter -/

namespace Filter

-- | Move row cursor to target (helper)
private def moveRowTo (s : ViewStack) (rowIdx : Nat) (search : Option (Nat × String) := none) : ViewStack :=
  let v := s.cur
  let delta : Int := rowIdx - v.nav.row.cur.val
  let nav' := { v.nav with row := { v.nav.row with cur := v.nav.row.cur.clamp delta } }
  s.setCur { v with nav := nav', search := search.orElse (fun _ => v.search) }

-- | Move col cursor to target (helper)
private def moveColTo (s : ViewStack) (colIdx : Nat) : ViewStack :=
  let v := s.cur
  let delta : Int := colIdx - v.nav.col.cur.val
  let nav' := { v.nav with col := { v.nav.col with cur := v.nav.col.cur.clamp delta } }
  s.setCur { v with nav := nav' }

-- | Column search: fzf jump to column by name
def colSearch (s : ViewStack) : IO ViewStack := do
  let v := s.cur; let names := ReadTable.colNames v.nav.tbl
  let dispNames := v.nav.grp ++ names.filter (!v.nav.grp.contains ·)
  let some idx ← Fzf.fzfIdx #["--prompt=Column: "] dispNames | return s
  return moveColTo s idx

-- | Row search: fzf distinct values, jump to matching row
def rowSearch (s : ViewStack) : IO ViewStack := do
  let v := s.cur; let names := ReadTable.colNames v.nav.tbl
  let curCol := colIdxAt v.nav.grp names v.nav.col.cur.val
  let curName := names.getD curCol ""
  let vals ← QueryTable.distinct v.nav.tbl curCol
  let some result ← Fzf.fzf #[s!"--prompt=/{curName}: "] ("\n".intercalate vals.toList) | return s
  let start := v.nav.row.cur.val + 1
  let some rowIdx ← QueryTable.findRow v.nav.tbl curCol result start true | return s
  return moveRowTo s rowIdx (some (curCol, result))

-- | Search next: repeat last search forward
def searchNext (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  let some (col, val) := v.search | return s
  let start := v.nav.row.cur.val + 1
  let some rowIdx ← QueryTable.findRow v.nav.tbl col val start true | return s
  return moveRowTo s rowIdx

-- | Search prev: repeat last search backward
def searchPrev (s : ViewStack) : IO ViewStack := do
  let v := s.cur
  let some (col, val) := v.search | return s
  let start := v.nav.row.cur.val
  let some rowIdx ← QueryTable.findRow v.nav.tbl col val start false | return s
  return moveRowTo s rowIdx

-- | Execute search/filter command
def exec (s : ViewStack) (cmd : Cmd) : IO (Option ViewStack) := do
  match cmd with
  | .col .ent    => some <$> colSearch s   -- s: fzf jump to column
  | .rowSel .inc => some <$> rowSearch s   -- /: fzf search in column
  | .grp .inc    => some <$> searchNext s  -- n: repeat search forward
  | .grp .dec    => some <$> searchPrev s  -- N: repeat search backward
  | _ => pure none

end Filter

end Tc
