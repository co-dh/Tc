/-
  Meta view: column statistics via DuckDB temp table tc_meta_N.
  Selection and key operations use composable PRQL (via ADBC/Meta).
  Each meta view gets a unique temp table so concurrent views don't collide.
-/
import Tc.View
import Tc.Table

namespace Tc.Meta

-- | Extract meta table name from the current view's AdbcTable query base.
--   e.g. "from tc_meta_3" → "tc_meta_3"
private def metaTblName (s : ViewStack Table) : String :=
  match s.tbl with
  | .adbc t => ((t.query.base.drop 5).trimAscii).toString  -- drop "from "
  | _ => "tc_meta"

-- | Push column metadata view onto stack
def push (s : ViewStack Table) : IO (Option (ViewStack Table)) := do
  let adbc? ← match s.tbl with
    | .adbc t => AdbcTable.queryMeta t
    | .kdb t => do
      let (headers, cols) ← KdbTable.queryMeta t
      AdbcTable.fromArrays headers cols
  let some adbc := adbc? | return none
  match View.fromTbl (Table.adbc adbc) s.cur.path with
  | some v => return some (s.push { v with vkind := .colMeta, disp := "meta" })
  | none => return none

-- | Select meta rows matching PRQL filter
private def selBy (s : ViewStack Table) (flt : String) : IO (ViewStack Table) := do
  if s.cur.vkind != .colMeta then return s
  let rows ← AdbcTable.queryMetaIndices (metaTblName s) flt
  let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
  return s.setCur { s.cur with nav := nav' }

def selNull (s : ViewStack Table)   := selBy s "null_pct == 100"
def selSingle (s : ViewStack Table) := selBy s "dist == 1"

-- | Set key cols from meta view selections, pop to parent, select cols
def setKey (s : ViewStack Table) : IO (Option (ViewStack Table)) := do
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
def update (s : ViewStack Table) (cmd : Cmd) : Option (ViewStack Table × Effect) :=
  match cmd with
  | .metaV .dup => some (s, .query .«meta»)
  | .metaV .dec => some (s, .«meta» .selNull)
  | .metaV .inc => some (s, .«meta» .selSingle)
  | .metaV .ent => if s.cur.vkind matches .colMeta then some (s, .«meta» .setKey) else none
  | _ => none

end Tc.Meta
