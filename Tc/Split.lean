/-
  Split: split column by delimiter/regex into multiple new columns.
  Press ':' → fzf prompt with common delimiters → detect part count → derive split columns → push view.
-/
import Tc.Data.ADBC.Ops
import Tc.Fzf
import Tc.View

namespace Tc.Split

-- Common delimiters offered as suggestions (first is used in test mode)
private def suggestions : String := "-\n,\n;\n:\n|\n\\s+\n_\n/"

-- | Find max number of parts when splitting column by pattern.
-- Compiles current PRQL query to SQL, wraps in aggregate to count max split parts.
private def maxParts (q : Prql.Query) (col pat : String) : IO Nat := do
  let some baseSql ← Prql.compile q.render | return 0
  let sql := baseSql.replace ";\n" ""
  let cid := col.replace "\"" "\"\""
  let ep := escSql pat
  let countSql := s!"SELECT COALESCE(max(array_length(string_split_regex(\"{cid}\", '{ep}'))), 0) FROM ({sql})"
  try
    let qr ← Adbc.query countSql
    let v ← Adbc.cellInt qr 0 0
    return min v.toNat 20  -- cap at 20 columns
  catch _ => return 0

-- | Build derive bindings: col_1 = s"string_split_regex({col}, 'pat')[1]", ...
private def splitBindings (col pat : String) (n : Nat) : Array (String × String) :=
  let ep := escSql pat
  let qc := Prql.quote col
  Array.range n |>.map fun i =>
    let idx := i + 1
    (s!"{col}_{idx}", s!"s\"string_split_regex(\{{qc}}, '{ep}')[{idx}]\"")

-- | Prompt for pattern, detect part count, derive split columns, push new view.
-- Returns unchanged stack on cancel or error.
def run (s : ViewStack AdbcTable) : IO (ViewStack AdbcTable) := do
  let names := TblOps.colNames s.tbl
  let curCol := s.cur.nav.curColIdx
  let curName := names.getD curCol ""
  let typ := TblOps.colType s.tbl curCol
  if typ != "str" then return s  -- only split string columns
  let header := s!"Split '{curName}' by delimiter or regex"
  let some raw ← Fzf.fzf #["--print-query", "--prompt=split: ", s!"--header={header}"] suggestions | return s
  let pat := raw.trimAscii.toString
  if pat.isEmpty then return s
  let n ← maxParts s.tbl.query curName pat
  if n <= 1 then return s  -- nothing to split
  let bindings := splitBindings curName pat n
  let q := s.tbl.query.pipe (.derive bindings)
  Log.write "split" q.render
  let some tbl' ← AdbcTable.requery q s.tbl.totalRows | return s
  let nCols := (TblOps.colNames tbl').size
  let firstSplitCol := nCols - n
  let some v := s.cur.rebuild tbl' (col := firstSplitCol) | return s
  return s.push { v with disp := s!":{curName}" }

end Tc.Split
