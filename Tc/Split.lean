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
private def maxParts (q : Prql.Query) (col ep : String) : IO Nat := do
  let some baseSql ← Prql.compile q.render | return 0
  let sql := stripSemi baseSql
  let qid := AdbcTable.quoteId col
  let countSql := s!"SELECT COALESCE(max(array_length(string_split_regex({qid}, '{ep}'))), 0) FROM ({sql})"
  try
    let qr ← Adbc.query countSql
    let v ← Adbc.cellInt qr 0 0
    return min v.toNat 20  -- cap at 20 columns
  catch e => Log.write "split" s!"maxParts: {e.toString}"; return 0

-- | Build derive bindings: col_1 = s"string_split_regex({col}, 'pat')[1]", ...
private def splitBindings (col ep : String) (qc : String) (n : Nat) : Array (String × String) :=
  Array.range n |>.map fun i =>
    let idx := i + 1
    (s!"{col}_{idx}", s!"s\"string_split_regex(\{{qc}}, '{ep}')[{idx}]\"")

-- | Split column by pattern (no fzf). Called by socket/dispatch and by `run` after fzf.
def runWith (s : ViewStack AdbcTable) (pat : String) : IO (ViewStack AdbcTable) := do
  let nav := s.cur.nav
  let curName := nav.curColName; let typ := nav.curColType
  if typ != .str then return s  -- only split string columns
  if pat.isEmpty then return s
  let ep := escSql pat
  let qc := Prql.quote curName
  let n ← maxParts s.tbl.query curName ep
  if n <= 1 then return s  -- nothing to split
  let bindings := splitBindings curName ep qc n
  let q := s.tbl.query.pipe (.derive bindings)
  Log.write "split" q.render
  let some tbl' ← AdbcTable.requery q s.tbl.totalRows | return s
  let nCols := TblOps.colNames tbl' |>.size
  let firstSplitCol := nCols - n
  let some v := s.cur.rebuild tbl' (col := firstSplitCol) | return s
  return s.push { v with disp := s!":{curName}" }

-- | Prompt for pattern via fzf, then split.
def run (s : ViewStack AdbcTable) : IO (ViewStack AdbcTable) := do
  let curName := s.cur.nav.curColName
  let header := s!"Split '{curName}' by delimiter or regex"
  let some raw ← Fzf.fzf #["--print-query", "--prompt=split: ", s!"--header={header}"] suggestions | return s
  runWith s raw.trimAscii.toString

end Tc.Split
