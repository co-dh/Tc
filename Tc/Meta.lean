/-
  Meta view: column statistics (name, type, count, distinct, null%, min, max)
  Returns MemTable with typed columns for proper sorting.
  Pure update returns Effect; Runner executes IO.
-/
import Tc.View
import Tc.Data.Mem.Meta

namespace Tc.Meta

-- Meta column headers
def headers : Array String := #["column", "type", "cnt", "dist", "null%", "min", "max"]

-- Meta column indices
def colDist : Nat := 3  -- distinct count
def colNull : Nat := 4  -- null%

-- Convert queryMeta tuple result to MemTable
def toMemTable (m : Array String × Array String × Array Int64 × Array Int64 × Array Int64 × Array String × Array String) : MemTable :=
  let (names, types, cnts, dists, nulls, mins, maxs) := m
  ⟨headers, #[.strs names, .strs types, .ints cnts, .ints dists, .ints nulls, .strs mins, .strs maxs]⟩

-- Get int value from column at row
def getInt (t : MemTable) (col row : Nat) : Int64 :=
  match t.cols.getD col default with
  | .ints data => data.getD row 0
  | _ => 0

-- Select rows where int column satisfies predicate
def selectRows (t : MemTable) (col : Nat) (pred : Int64 → Bool) : Array Nat :=
  (Array.range (MemTable.nRows t)).filter fun r => pred (getInt t col r)

-- Select 100% null columns
def selNull (t : MemTable) : Array Nat := selectRows t colNull (· == 100)

-- Select single-value columns (distinct == 1)
def selSingle (t : MemTable) : Array Nat := selectRows t colDist (· == 1)

-- Get column names from selected rows (col 0 is "column")
def selNames (t : MemTable) (selRows : Array Nat) : Array String :=
  selRows.filterMap fun r =>
    match t.cols.getD 0 default with
    | .strs data => some (data.getD r "")
    | _ => none

-- | Push column metadata view onto stack
def push (s : ViewStack) : IO (Option ViewStack) := do
  let tbl ← QueryTable.queryMeta s.cur.nav.tbl <&> toMemTable
  let some v := View.fromTbl (.mem tbl) s.cur.path | return none
  return some (s.push { v with vkind := .colMeta, disp := "meta" })

-- | Select rows in meta view by predicate on MemTable
def sel (s : ViewStack) (f : MemTable → Array Nat) : ViewStack :=
  if s.cur.vkind != .colMeta then s else
  match s.cur.nav.tbl.asMem? with
  | some tbl =>
    let rows := f tbl
    let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
    s.setCur { s.cur with nav := nav' }
  | none => s

-- | Set key cols from meta view selections, pop to parent, select cols
def setKey (s : ViewStack) : Option ViewStack :=
  if s.cur.vkind != .colMeta then some s else
  if !s.hasParent then some s else
  match s.cur.nav.tbl.asMem? with
  | some tbl =>
    let colNames := selNames tbl s.cur.nav.row.sels
    match s.pop with
    | some s' =>
      let nav' := { s'.cur.nav with grp := colNames, col := { s'.cur.nav.col with sels := colNames } }
      some (s'.setCur { s'.cur with nav := nav' })
    | none => some s
  | none => some s

-- | Pure update: returns Effect for IO operations, pure for selections
def update (s : ViewStack) (cmd : Cmd) : Option (ViewStack × Effect) :=
  match cmd with
  | .metaV .dup => some (s, .queryMeta)              -- push meta view (IO)
  | .metaV .dec => some (sel s selNull, .none)       -- select null cols (pure)
  | .metaV .inc => some (sel s selSingle, .none)     -- select single-val cols (pure)
  | .view .ent => if s.cur.vkind == .colMeta then setKey s |>.map (·, .none) else none
  | _ => none

-- | Execute meta command (IO version for backward compat)
def exec (s : ViewStack) (cmd : Cmd) : IO (Option ViewStack) := do
  match cmd with
  | .metaV .dup => (← push s).orElse (fun _ => some s) |> pure
  | .metaV .dec => pure (some (sel s selNull))
  | .metaV .inc => pure (some (sel s selSingle))
  | .view .ent => if s.cur.vkind == .colMeta then pure (setKey s) else pure none
  | _ => pure none

end Tc.Meta
