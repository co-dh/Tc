/-
  Freq view: group by columns, count, pct, bar
  Returns MemTable sorted by Cnt descending.
  Pure update returns Effect; Runner executes IO.
-/
import Tc.View
import Tc.Data.Mem.Freq

namespace Tc.Freq

variable {T : Type} [TblOps T] [MemConvert MemTable T]

-- | Convert queryFreq tuple result to MemTable (sorted by Cnt desc)
def toMemTable (f : FreqResult) : MemTable :=
  let names := f.keyNames ++ #["Cnt", "Pct", "Bar"]
  let cols := f.keyCols ++ #[.ints f.cntData, .floats f.pctData, .strs f.barData]
  MemTable.sort { names, cols, h_eq := by simp [names, cols, Array.size_append, f.hKeys] } #[f.keyCols.size] false

-- | Build filter expression from freq row (col1 == val1 && col2 == val2 ...)
def filterExpr (tbl : MemTable) (cols : Array String) (row : Nat) : String :=
  let vals := cols.mapIdx fun i _ =>
    match tbl.cols.getD i default with
    | .strs data => s!"'{data.getD row ""}'"
    | .ints data => s!"{data.getD row 0}"
    | .floats data => s!"{data.getD row 0}"
  let exprs := cols.zip vals |>.map fun (c, v) => s!"{c} == {v}"
  " && ".intercalate exprs.toList

-- | Push frequency view (group by grp + cursor column)
def push (s : ViewStack T) : IO (Option (ViewStack T)) := do
  let n := s.cur.nav; let names := TblOps.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  let colIdxs := colNames.filterMap names.idxOf?
  let freq ← TblOps.queryFreq n.tbl colIdxs
  let tbl := toMemTable freq
  let some v := View.fromTbl (MemConvert.wrap tbl) s.cur.path 0 colNames | return none
  return some (s.push { v with vkind := .freqV colNames freq.totalGroups, disp := s!"freq {colNames.join ","}" })

-- | Filter parent by selected freq row, pop freq and push filtered view
def filter (s : ViewStack T) : IO (Option (ViewStack T)) := do
  let .freqV cols _ := s.cur.vkind | return some s
  if !s.hasParent then return some s
  let some tbl := MemConvert.unwrap s.cur.nav.tbl | return some s
  let some s' := s.pop | return some s
  let expr := filterExpr tbl cols s.cur.nav.row.cur.val
  let some tbl' ← TblOps.filter s'.cur.nav.tbl expr | return some s'
  let some v := View.fromTbl tbl' s'.cur.path 0 s'.cur.nav.grp 0 | return some s'
  return some (s'.push v)

-- | Pure update: returns Effect for IO operations
def update (s : ViewStack T) (cmd : Cmd) : Option (ViewStack T × Effect) :=
  let n := s.cur.nav; let names := TblOps.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  let colIdxs := colNames.filterMap names.idxOf?
  match cmd with
  | .freq .dup => some (s, .queryFreq colIdxs colNames)  -- push freq view (IO)
  | .freq .ent => match s.cur.vkind with  -- Enter and space F ~ both map to .freq .ent
    | .freqV cols _ => some (s, .freqFilter cols s.cur.nav.row.cur.val)  -- filter (IO)
    | _ => none
  | _ => none

-- | Execute freq command (IO version for backward compat)
def exec (s : ViewStack T) (cmd : Cmd) : IO (Option (ViewStack T)) := do
  match cmd with
  | .freq .dup => (← push s).orElse (fun _ => some s) |> pure
  | .freq .ent => match s.cur.vkind with
    | .freqV _ _ => filter s
    | _ => pure none
  | _ => pure none

end Tc.Freq
