/-
  Diff: compare top 2 stack views.
  Auto-keys categorical columns (same name+type in both tables).
  Columns with identical values are hidden via sameHide (separate from user hide).
  Diff columns get Δ prefix in header for visibility.
-/
import Tc.View
import Tc.Data.ADBC.Ops

namespace Tc.Diff

private def isNumeric (typ : String) : Bool :=
  typ == "int" || typ == "float" || typ == "decimal"

-- | DuckDB double-quoted identifier (escapes " → "" for identifiers)
private def dq (s : String) : String := "\"" ++ s.replace "\"" "\"\"" ++ "\""

-- | Find columns present in both tables with same name and type
private def commonCols (left right : AdbcTable) : Array (String × String) :=
  left.colNames.filterMap fun name =>
    let li := left.colNames.idxOf? name
    let ri := right.colNames.idxOf? name
    match li, ri with
    | some li', some ri' =>
      let lt := left.colTypes.getD li' "?"
      let rt := right.colTypes.getD ri' "?"
      if lt == rt then some (name, lt) else none
    | _, _ => none

-- | Full outer join on key columns, with _left/_right suffixes for value columns.
--   Returns the result table, auto-keyed group columns, and same-value column names.
def run (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  let some parent := s.tl.head? | return none
  let left := parent.nav.tbl
  let right := s.tbl
  let common := commonCols left right
  if common.isEmpty then
    statusMsg "diff: no common columns"
    return none
  -- Auto-key: categorical (non-numeric) common columns become group/key columns
  -- Ignore columns already keyed by user in either view
  let existingGrp := parent.nav.grp ++ s.cur.nav.grp
  let autoKeys := common.filterMap fun (name, typ) =>
    if !isNumeric typ && !existingGrp.contains name then some name else none
  let allKeys := existingGrp ++ autoKeys
    |>.foldl (init := #[]) fun acc k => if acc.contains k then acc else acc.push k
  if allKeys.isEmpty then
    statusMsg "diff: no key columns (need categorical columns with same name+type)"
    return none
  -- Value columns: numeric common columns not in keys
  let valCols := common.filterMap fun (name, _) =>
    if allKeys.contains name then none else some name
  -- Build SQL: full outer join on keys, with suffixed value columns
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let (lName, lSql) ← prepareView left s!"dl{n}"
  let (rName, rSql) ← prepareView right s!"dr{n}"
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP VIEW {lName} AS {lSql}"
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP VIEW {rName} AS {rSql}"
  -- Build column list: keys from COALESCE, values from both sides
  let keySel := allKeys.map fun k => s!"COALESCE(L.{dq k}, R.{dq k}) AS {dq k}"
  let valSel := valCols.map fun v =>
    s!"L.{dq v} AS {dq (v ++ "_left")}, R.{dq v} AS {dq (v ++ "_right")}"
  -- Columns only in left or right (not common)
  let leftOnly := left.colNames.filter fun n => !common.any (·.1 == n) && !allKeys.contains n
  let rightOnly := right.colNames.filter fun n => !common.any (·.1 == n) && !allKeys.contains n
  let leftOnlySel := leftOnly.map fun n => s!"L.{dq n} AS {dq (n ++ "_left")}"
  let rightOnlySel := rightOnly.map fun n => s!"R.{dq n} AS {dq (n ++ "_right")}"
  let allSel := keySel ++ valSel ++ leftOnlySel ++ rightOnlySel
  let joinCond := allKeys.map fun k => s!"L.{dq k} IS NOT DISTINCT FROM R.{dq k}"
  let joinCondStr := " AND ".intercalate joinCond.toList
  let tblName := s!"tc_diff_{n}"
  let sql := s!"CREATE OR REPLACE TEMP TABLE {tblName} AS SELECT {", ".intercalate allSel.toList} FROM {lName} L FULL OUTER JOIN {rName} R ON {joinCondStr}"
  Log.write "diff-sql" sql
  let _ ← Adbc.query sql
  let q : Prql.Query := { base := s!"from {tblName}" }
  let total ← AdbcTable.queryCount q
  -- Detect same-value columns: for each value col pair, check if all values match
  let mut sameHide : Array String := #[]
  for v in valCols do
    let lCol := v ++ "_left"
    let rCol := v ++ "_right"
    -- Count rows where left != right (NULLs treated as equal)
    let checkSql := s!"SELECT COUNT(*) FROM {tblName} WHERE NOT ({dq lCol} IS NOT DISTINCT FROM {dq rCol})"
    let checkQr ← Adbc.query checkSql
    let diffCount ← Adbc.cellInt checkQr 0 0
    if diffCount == 0 then
      sameHide := sameHide.push lCol
      sameHide := sameHide.push rCol
  -- Rename diff columns with Δ prefix for visibility
  let diffValCols := valCols.filter fun v =>
    !sameHide.contains (v ++ "_left")
  let mut renameExprs : Array String := #[]
  for v in diffValCols do
    renameExprs := renameExprs.push s!"ALTER TABLE {tblName} RENAME COLUMN {dq (v ++ "_left")} TO {dq ("Δ" ++ v ++ "_L")}"
    renameExprs := renameExprs.push s!"ALTER TABLE {tblName} RENAME COLUMN {dq (v ++ "_right")} TO {dq ("Δ" ++ v ++ "_R")}"
  for expr in renameExprs do
    let _ ← Adbc.query expr
  -- Re-read table after column renames
  let some adbc ← AdbcTable.requery q total | return none
  -- Pop current view, replace parent with diff result
  let some s' := s.pop | return none
  match View.fromTbl adbc s'.cur.path (grp := allKeys) with
  | some v => return some (s'.setCur { v with disp := "diff", sameHide })
  | none => return none
where
  -- Compile PRQL to SQL for creating temp views
  prepareView (tbl : AdbcTable) (suffix : String) : IO (String × String) := do
    let prql := tbl.query.render
    let some sql ← Prql.compile prql | throw (IO.userError "PRQL compile failed")
    pure (s!"tc_{suffix}", stripSemi sql)

-- | Clear sameHide to reveal same-value columns (one-way toggle)
def showSame (v : View AdbcTable) : View AdbcTable :=
  { v with sameHide := #[] }

end Tc.Diff
