/-
  Meta view: column statistics via DuckDB temp table tc_meta_N.
  Selection and key operations use composable PRQL (via ADBC/Meta).
  Each meta view gets a unique temp table so concurrent views don't collide.
-/
import Tc.View
import Tc.Data.ADBC.Ops

namespace Tc.Meta

-- | Extract meta table name from the current view's AdbcTable query base.
--   e.g. "from tc_meta_3" → "tc_meta_3"
private def metaTblName (s : ViewStack AdbcTable) : String :=
  ((s.tbl.query.base.drop 5).trimAscii).toString  -- drop "from "

-- | Push column metadata view onto stack
def push (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  let some adbc ← AdbcTable.queryMeta s.tbl | return none
  -- Enrich meta with DuckDB column comments (e.g. osquery views with COMMENT ON COLUMN)
  let metaBase := (adbc.query.base.drop 5).trimAscii.toString
  if ← AdbcTable.enrichComments metaBase s.cur.path then
    match ← AdbcTable.requery adbc.query with
    | some adbc' => match View.fromTbl adbc' s.cur.path with
      | some v => return some (s.push { v with vkind := .colMeta, disp := "meta" })
      | none => return none
    | none => return none
  match View.fromTbl adbc s.cur.path with
  | some v => return some (s.push { v with vkind := .colMeta, disp := "meta" })
  | none => return none

-- | Select meta rows matching PRQL filter
private def selBy (s : ViewStack AdbcTable) (flt : String) : IO (ViewStack AdbcTable) := do
  if s.cur.vkind != .colMeta then return s
  let rows ← AdbcTable.queryMetaIndices (metaTblName s) flt
  let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
  return s.setCur { s.cur with nav := nav' }

def selNull (s : ViewStack AdbcTable)   := selBy s "null_pct == 100"
def selSingle (s : ViewStack AdbcTable) := selBy s "dist == 1"

-- | Set key cols from meta view selections, pop to parent, select cols
def setKey (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  if s.cur.vkind != .colMeta then return some s
  if !s.hasParent then return some s
  let colNames ← AdbcTable.queryMetaColNames (metaTblName s) s.cur.nav.row.sels
  match s.pop with
  | some s' =>
    let col' := { s'.cur.nav.col with sels := colNames }
    let di := dispOrder colNames (TblOps.colNames s'.tbl)
    let nav' := { s'.cur.nav with grp := colNames, col := col', dispIdxs := di }
    return some (s'.setCur { s'.cur with nav := nav' })
  | none => return some s

-- | Pure update: returns Effect for IO operations
def update (s : ViewStack AdbcTable) (cmd : Cmd) : Option (ViewStack AdbcTable × Effect) :=
  match cmd with
  | .metaV .dup => some (s, .query .colMeta)
  | .metaV (.val 0) => some (s, .colMeta .selNull)
  | .metaV (.val 1) => some (s, .colMeta .selSingle)
  | .metaV .ent => if s.cur.vkind matches .colMeta then some (s, .colMeta .setKey) else none
  | _ => none

end Tc.Meta
