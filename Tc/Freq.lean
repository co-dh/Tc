/-
  Freq view: group by columns, count, pct, bar
  Returns MemTable sorted by Cnt descending.
  Pure update returns Effect; Runner executes IO.
-/
import Tc.View
import Tc.Data.Mem.Freq

namespace Tc.Freq

-- | Convert queryFreq tuple result to MemTable (sorted by Cnt desc)
def toMemTable (f : FreqTuple) : MemTable :=
  let (keyNames, keyCols, cntData, pctData, barData) := f
  let names := keyNames ++ #["Cnt", "Pct", "Bar"]
  let cols := keyCols ++ #[.ints cntData, .floats pctData, .strs barData]
  -- sort by Cnt (first column after keys) descending
  MemTable.sort ⟨names, cols⟩ #[keyCols.size] false

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

-- | Filter parent by selected freq row, pop freq and push filtered view
def filter (s : ViewStack) : IO (Option ViewStack) := do
  let .freqV cols := s.cur.vkind | return some s
  if !s.hasParent then return some s
  let some tbl := s.cur.nav.tbl.asMem? | return some s
  let some s' := s.pop | return some s
  let expr := filterExpr tbl cols s.cur.nav.row.cur.val
  let some tbl' ← QueryTable.filter s'.cur.nav.tbl expr | return some s'
  let some v := View.fromTbl tbl' s'.cur.path 0 s'.cur.nav.grp 0 | return some s'
  return some (s'.push v)

-- | Pure update: returns Effect for IO operations
def update (s : ViewStack) (cmd : Cmd) : Option (ViewStack × Effect) :=
  let n := s.cur.nav; let names := ReadTable.colNames n.tbl
  let curCol := colIdxAt n.grp names n.col.cur.val
  let curName := names.getD curCol ""
  let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
  let colIdxs := colNames.filterMap names.idxOf?
  match cmd with
  | .freq .dup => some (s, .queryFreq colIdxs colNames)  -- push freq view (IO)
  | .view .ent => match s.cur.vkind with
    | .freqV cols => some (s, .freqFilter cols s.cur.nav.row.cur.val)  -- filter (IO)
    | _ => none
  | _ => none

-- | Execute freq command (IO version for backward compat)
def exec (s : ViewStack) (cmd : Cmd) : IO (Option ViewStack) := do
  match cmd with
  | .freq .dup => (← push s).orElse (fun _ => some s) |> pure
  | .view .ent => match s.cur.vkind with
    | .freqV _ => filter s
    | _ => pure none
  | _ => pure none

end Tc.Freq
