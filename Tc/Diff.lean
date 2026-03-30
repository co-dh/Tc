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
  left.colNames.filterMap fun name => do
    let li ← left.colNames.idxOf? name
    let ri ← right.colNames.idxOf? name
    let lt := left.colTypes.getD li "?"
    let rt := right.colTypes.getD ri "?"
    if lt == rt then pure (name, lt) else none

-- | Deduplicate array preserving insertion order
private def dedup (a : Array String) : Array String :=
  a.foldl (init := #[]) fun acc k => if acc.contains k then acc else acc.push k

-- | Columns in `tbl` that don't appear in `common` or `keys`
private def onlyCols (tbl : AdbcTable) (common : Array (String × String)) (keys : Array String) :=
  tbl.colNames.filter fun n => !common.any (·.1 == n) && !keys.contains n

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
  let autoKeys := common
    |>.filter (fun (_, typ) => !isNumeric typ)
    |>.map (·.1)
    |>.filter (fun n => !existingGrp.contains n)
  let allKeys := existingGrp ++ autoKeys |> dedup
  if allKeys.isEmpty then statusMsg "diff: no key columns (need categorical columns with same name+type)"; return none
  let valCols := common.map (·.1) |>.filter (fun n => !allKeys.contains n)
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
  let leftOnlySel := onlyCols left common allKeys |> sideSel "L"
  let rightOnlySel := onlyCols right common allKeys |> sideSel "R"
  let joinCond := allKeys.map (fun k => s!"L.{q k} IS NOT DISTINCT FROM R.{q k}")
    |>.joinWith " AND "
  let selCols := (keySel ++ valSel ++ leftOnlySel ++ rightOnlySel).joinWith ", "
  let tblName := s!"tc_diff_{seq}"
  let sql := s!"CREATE OR REPLACE TEMP TABLE {tblName} AS SELECT {selCols} FROM {lName} L FULL OUTER JOIN {rName} R ON {joinCond}"
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
  pure (View.fromTbl adbc s'.cur.path (grp := allKeys)
    |>.map fun v => s'.setCur { v with disp := "diff", sameHide })
where
  -- | Compile PRQL query to SQL for creating temp views
  prepareView (tbl : AdbcTable) (sfx : String) : IO (String × String) := do
    let some sql ← Prql.compile tbl.query.render | throw (IO.userError "PRQL compile failed")
    pure (s!"tc_{sfx}", stripSemi sql)

-- | Clear sameHide to reveal identical-value columns (toggle)
def showSame (v : View AdbcTable) : View AdbcTable := { v with sameHide := #[] }

end Tc.Diff
