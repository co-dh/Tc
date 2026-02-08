/-
  Meta view: column statistics via DuckDB temp table tc_meta.
  Selection and key operations use composable PRQL.
-/
import Tc.View
import Tc.Table
import Tc.Data.ADBC.Prql

namespace Tc.Meta

-- | Query row indices matching PRQL filter on tc_meta
private def queryIndices (flt : String) : IO (Array Nat) := do
  let prql := "from tc_meta | derive {idx = s\"(ROW_NUMBER() OVER () - 1)\"} | filter " ++ flt ++ " | select {idx}"
  let some sql ← Prql.compile prql | return #[]
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  (Array.range nr.toNat).mapM fun r => (·.toNat) <$> Adbc.cellInt qr r.toUInt64 0

-- | Query column names from tc_meta at given row indices
private def queryColNames (rows : Array Nat) : IO (Array String) := do
  if rows.isEmpty then return #[]
  let idxs := ", ".intercalate (rows.map (s!"{·}") |>.toList)
  let prql := "from tc_meta | derive {idx = s\"(ROW_NUMBER() OVER () - 1)\"} | filter s\"idx IN (" ++ idxs ++ ")\" | select {column, idx}"
  let some sql ← Prql.compile prql | return #[]
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  (Array.range nr.toNat).mapM fun r => Adbc.cellStr qr r.toUInt64 0

-- | Push column metadata view onto stack
def push (s : ViewStack Table) : IO (Option (ViewStack Table)) := do
  let adbc? ← match s.cur.nav.tbl with
    | .adbc t => AdbcTable.queryMeta t
    | .kdb t => do
      let (headers, cols) ← KdbTable.queryMeta t
      AdbcTable.fromArrays headers cols
  let some adbc := adbc? | return none
  match View.fromTbl (Table.adbc adbc) s.cur.path with
  | some v => return some (s.push { v with vkind := .colMeta, disp := "meta" })
  | none => return none

-- | Select 100% null columns
def selNull (s : ViewStack Table) : IO (ViewStack Table) := do
  if s.cur.vkind != .colMeta then return s
  let rows ← queryIndices "null_pct == 100"
  let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
  return s.setCur { s.cur with nav := nav' }

-- | Select single-value columns (distinct == 1)
def selSingle (s : ViewStack Table) : IO (ViewStack Table) := do
  if s.cur.vkind != .colMeta then return s
  let rows ← queryIndices "dist == 1"
  let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
  return s.setCur { s.cur with nav := nav' }

-- | Set key cols from meta view selections, pop to parent, select cols
def setKey (s : ViewStack Table) : IO (Option (ViewStack Table)) := do
  if s.cur.vkind != .colMeta then return some s
  if !s.hasParent then return some s
  let colNames ← queryColNames s.cur.nav.row.sels
  match s.pop with
  | some s' =>
    let col' := { s'.cur.nav.col with sels := colNames }
    let di := dispOrder colNames (TblOps.colNames s'.cur.nav.tbl)
    let nav' := { s'.cur.nav with grp := colNames, col := col', dispIdxs := di }
    return some (s'.setCur { s'.cur with nav := nav' })
  | none => return some s

-- | Pure update: returns Effect for IO operations
def update (s : ViewStack Table) (cmd : Cmd) : Option (ViewStack Table × Effect) :=
  match cmd with
  | .metaV .dup => some (s, .query .«meta»)
  | .metaV .dec => some (s, .«meta» .selNull)
  | .metaV .inc => some (s, .«meta» .selSingle)
  | .metaV .ent => if s.cur.vkind matches .colMeta then some (s, .«meta» .setKey) else none
  | _ => none

end Tc.Meta
