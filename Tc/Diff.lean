/-
  Diff: compare top 2 stack views via FULL OUTER JOIN.
  Auto-keys categorical columns, hides same-value columns, Δ-prefixes diffs.
-/
import Tc.View
import Tc.Data.ADBC.Ops

namespace Tc.Diff

private def isNumeric := isNumericType

-- | DuckDB double-quoted identifier (escapes " → "")
private def quoted (s : String) : String := "\"" ++ s.replace "\"" "\"\"" ++ "\""

-- | Find columns present in both tables with same name and type
private def commonCols (left right : AdbcTable) : Array (String × String) :=
  left.colNames.filterMap fun name =>
    let li := left.colNames.idxOf? name
    let ri := right.colNames.idxOf? name
    match li, ri with
    | some li', some ri' =>
      let lt := left.colTypes.getD li' "?"; let rt := right.colTypes.getD ri' "?"
      if lt == rt then some (name, lt) else none
    | _, _ => none

-- | Deduplicate array preserving insertion order
private def dedup (a : Array String) : Array String :=
  a.foldl (init := #[]) fun acc k => if acc.contains k then acc else acc.push k

-- | FULL OUTER JOIN top 2 stack views on shared categorical columns.
-- Auto-keys non-numeric common columns, suffixes value columns with _left/_right,
-- hides columns with identical values, Δ-prefixes columns that differ.
def run (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  let some parent := s.tl.head? | return none
  let (left, right) := (parent.nav.tbl, s.tbl)
  let common := commonCols left right
  if common.isEmpty then statusMsg "diff: no common columns"; return none
  -- Auto-key: categorical (non-numeric) common columns become join keys
  let existingGrp := parent.nav.grp ++ s.cur.nav.grp
  let autoKeys := common.filterMap fun (name, typ) =>
    if !isNumeric typ && !existingGrp.contains name then some name else none
  let allKeys := dedup (existingGrp ++ autoKeys)
  if allKeys.isEmpty then statusMsg "diff: no key columns (need categorical columns with same name+type)"; return none
  let valCols := common.filterMap fun (n, _) => if allKeys.contains n then none else some n
  -- Build FULL OUTER JOIN SQL with suffixed columns
  let seq ← memTblCounter.modifyGet fun n => (n, n + 1)
  let (lName, lSql) ← prepareView left s!"dl{seq}"
  let (rName, rSql) ← prepareView right s!"dr{seq}"
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP VIEW {lName} AS {lSql}"
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP VIEW {rName} AS {rSql}"
  let q := quoted
  let keySel := allKeys.map fun k => s!"COALESCE(L.{q k}, R.{q k}) AS {q k}"
  let valSel := valCols.map fun v => s!"L.{q v} AS {q (v ++ "_left")}, R.{q v} AS {q (v ++ "_right")}"
  let sideSel side cols := cols.map fun n => s!"{side}.{q n} AS {q (n ++ s!"_{side}")}"
  let leftOnlySel := sideSel "L" (left.colNames.filter fun n => !common.any (·.1 == n) && !allKeys.contains n)
  let rightOnlySel := sideSel "R" (right.colNames.filter fun n => !common.any (·.1 == n) && !allKeys.contains n)
  let joinCond := " AND ".intercalate (allKeys.map fun k => s!"L.{q k} IS NOT DISTINCT FROM R.{q k}").toList
  let tblName := s!"tc_diff_{seq}"
  let sql := s!"CREATE OR REPLACE TEMP TABLE {tblName} AS SELECT {", ".intercalate (keySel ++ valSel ++ leftOnlySel ++ rightOnlySel).toList} FROM {lName} L FULL OUTER JOIN {rName} R ON {joinCond}"
  Log.write "diff-sql" sql
  let _ ← Adbc.query sql
  let query : Prql.Query := { base := s!"from {tblName}" }
  let total ← AdbcTable.queryCount query
  -- Detect same-value columns, rename diffs with Δ prefix
  let mut sameHide : Array String := #[]
  for v in valCols do
    let (leftCol, rightCol) := (v ++ "_left", v ++ "_right")
    let cnt ← Adbc.cellInt (← Adbc.query s!"SELECT COUNT(*) FROM {tblName} WHERE NOT ({q leftCol} IS NOT DISTINCT FROM {q rightCol})") 0 0
    if cnt == 0 then sameHide := sameHide ++ #[leftCol, rightCol]
    else for (old, renamed) in #[(leftCol, "Δ" ++ v ++ "_L"), (rightCol, "Δ" ++ v ++ "_R")] do
      let _ ← Adbc.query s!"ALTER TABLE {tblName} RENAME COLUMN {q old} TO {q renamed}"
  let some adbc ← AdbcTable.requery query total | return none
  let some s' := s.pop | return none
  pure (View.fromTbl adbc s'.cur.path (grp := allKeys) |>.map fun v => s'.setCur { v with disp := "diff", sameHide })
where
  -- | Compile PRQL query to SQL for creating temp views
  prepareView (tbl : AdbcTable) (sfx : String) : IO (String × String) := do
    let some sql ← Prql.compile tbl.query.render | throw (IO.userError "PRQL compile failed")
    pure (s!"tc_{sfx}", stripSemi sql)

-- | Clear sameHide to reveal identical-value columns (toggle)
def showSame (v : View AdbcTable) : View AdbcTable := { v with sameHide := #[] }

end Tc.Diff
