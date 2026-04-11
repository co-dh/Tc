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
  s.tbl.query.base.drop 5 |>.trimAscii |>.toString  -- drop "from "

-- | Push column metadata view onto stack
def push (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  let some adbc ← AdbcTable.queryMeta s.tbl | return none
  -- Enrich meta with DuckDB column comments (e.g. osquery views with COMMENT ON COLUMN)
  let metaBase := adbc.query.base.drop 5 |>.trimAscii |>.toString
  let adbc ← if ← AdbcTable.enrichComments metaBase s.cur.path then
    pure ((← AdbcTable.requery adbc.query).getD adbc) else pure adbc
  return View.fromTbl adbc s.cur.path |>.map fun v =>
    s.push { v with vkind := .colMeta, disp := "meta" }

-- | Select meta rows matching PRQL filter
private def selBy (s : ViewStack AdbcTable) (flt : String) : IO (ViewStack AdbcTable) := do
  if s.cur.vkind != .colMeta then return s
  let rows ← AdbcTable.queryMetaIndices (metaTblName s) flt
  return (ViewStack.hdL ∘ₗ View.navL ∘ₗ NavState.rowSelsL).set rows s

def selNull (s : ViewStack AdbcTable)   := selBy s "null_pct == 100"
def selSingle (s : ViewStack AdbcTable) := selBy s "dist == 1"

-- | Set key cols from meta view selections, pop to parent, select cols
def setKey (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  if s.cur.vkind != .colMeta then return some s
  if !s.hasParent then return some s
  let colNames ← AdbcTable.queryMetaColNames (metaTblName s) s.cur.nav.row.sels
  match s.pop with
  | some s' =>
    let di := dispOrder colNames (TblOps.colNames s'.tbl)
    let nav' := { s'.cur.nav with grp := colNames, dispIdxs := di }
      |> NavState.colSelsL.set colNames
    return some ((ViewStack.hdL ∘ₗ View.navL).set nav' s')
  | none => return some s

-- | Dispatch meta handler to IO action. Returns none if handler not recognized.
def dispatch (s : ViewStack AdbcTable) (h : Cmd) : Option (IO (Option (ViewStack AdbcTable))) :=
  match h with
  | .metaPush      => some (push s)
  | .metaSelNull   => some (some <$> selNull s)
  | .metaSelSingle => some (some <$> selSingle s)
  | .metaSetKey    => if s.cur.vkind matches .colMeta then some (setKey s) else none
  | _ => none

end Tc.Meta
