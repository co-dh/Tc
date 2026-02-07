/-
  Meta view: column statistics (name, type, count, distinct, null%, min, max)
  Creates AdbcTable directly via fromArrays. IO-based selection via getCols.
-/
import Tc.View
import Tc.Table

namespace Tc.Meta

-- Meta column headers
def headers : Array String := #["column", "type", "cnt", "dist", "null%", "min", "max"]

-- Meta column indices
def colDist : Nat := 3  -- distinct count
def colNull : Nat := 4  -- null%

-- | Push column metadata view onto stack
def push (s : ViewStack Table) : IO (Option (ViewStack Table)) := do
  let m ← TblOps.queryMeta s.cur.nav.tbl
  let (names, types, cnts, dists, nulls, mins, maxs) := m
  let cols : Array Column := #[.strs names, .strs types, .ints cnts, .ints dists, .ints nulls, .strs mins, .strs maxs]
  match ← AdbcTable.fromArrays headers cols with
  | some adbc =>
    match View.fromTbl (Table.adbc adbc) s.cur.path with
    | some v => return some (s.push { v with vkind := .colMeta, disp := "meta" })
    | none => return none
  | none => return none

-- | Select rows where int column satisfies predicate (IO via getCols)
private def selBy (s : ViewStack Table) (col : Nat) (pred : Int64 → Bool) : IO (ViewStack Table) := do
  if s.cur.vkind != .colMeta then return s
  let cols ← TblOps.getCols s.cur.nav.tbl #[col] 0 s.cur.nRows
  let c := cols.getD 0 default
  let rows := (Array.range s.cur.nRows).filter fun r =>
    match c with | .ints data => pred (data.getD r 0) | _ => false
  let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
  return s.setCur { s.cur with nav := nav' }

-- | Select 100% null columns
def selNull (s : ViewStack Table) : IO (ViewStack Table) := selBy s colNull (· == 100)

-- | Select single-value columns (distinct == 1)
def selSingle (s : ViewStack Table) : IO (ViewStack Table) := selBy s colDist (· == 1)

-- | Set key cols from meta view selections, pop to parent, select cols
def setKey (s : ViewStack Table) : IO (Option (ViewStack Table)) := do
  if s.cur.vkind != .colMeta then return some s
  if !s.hasParent then return some s
  -- read column 0 (name column) to get selected column names
  let cols ← TblOps.getCols s.cur.nav.tbl #[0] 0 s.cur.nRows
  let nameCol := cols.getD 0 default
  let colNames := s.cur.nav.row.sels.filterMap fun r =>
    let v := (nameCol.get r).toRaw
    if v.isEmpty then none else some v
  match s.pop with
  | some s' =>
    let nav' := { s'.cur.nav with grp := colNames, col := { s'.cur.nav.col with sels := colNames } }
    return some (s'.setCur { s'.cur with nav := nav' })
  | none => return some s

-- | Pure update: returns Effect for IO operations
def update (s : ViewStack Table) (cmd : Cmd) : Option (ViewStack Table × Effect) :=
  match cmd with
  | .metaV .dup => some (s, .queryMeta)
  | .metaV .dec => some (s, .metaSelNull)
  | .metaV .inc => some (s, .metaSelSingle)
  | .metaV .ent => if s.cur.vkind matches .colMeta then some (s, .metaSetKey) else none
  | _ => none

end Tc.Meta
