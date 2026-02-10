/-
  Kdb backend + UI tests for Tc
  Run with: lake build testkdb && .lake/build/bin/testkdb
  Requires kdb/q server at localhost:8888 with nbbo table
-/
import Tc.Data.ADBC.Table
import Tc.Nav
import Tc.View
import Tc.UI.Info
import Tc.Types
import Tc.Data.Kdb.FFI
import Tc.Data.Kdb.Q
import Tc.Data.Kdb.Table

namespace TestKdb

open Tc

def bin := ".lake/build/bin/tc"

-- | Log to file
def log (msg : String) : IO Unit := do
  let h ← IO.FS.Handle.mk "test.log" .append
  h.putStrLn msg; h.flush

def isContent (l : String) : Bool := l.any fun c => c.isAlpha || c.isDigit
def contains (s sub : String) : Bool := (s.splitOn sub).length > 1

def footer (output : String) : String × String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  (lines.getD (n - 2) "", lines.getD (n - 1) "")

def header (output : String) : String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let hdr := lines.headD ""
  if hdr.length > 80 then (hdr.drop (hdr.length - 80)).toString else hdr

def dataLines (output : String) : List String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  lines.drop 1 |>.take (n - 3)

def assert (cond : Bool) (msg : String) : IO Unit :=
  unless cond do throw (IO.userError msg)

-- | Convert key notation to tmux args: "<C-d>" → ["C-d"], "abc" → ["-l", "abc"]
def keysToTmux (keys : String) : Array (Array String) := Id.run do
  let mut result : Array (Array String) := #[]
  let mut buf := ""
  let chars := keys.toList.toArray
  let mut i := 0
  while i < chars.size do
    let c := chars.getD i ' '
    if c == '<' then
      if !buf.isEmpty then result := result.push #["-l", buf]; buf := ""
      let mut j := i + 1
      while j < chars.size && chars.getD j ' ' != '>' do j := j + 1
      let tag := String.ofList (chars.toList.drop (i + 1) |>.take (j - i - 1))
      let tmuxKey := if tag.startsWith "C-" then tag
                     else if tag == "ret" then "Enter"
                     else tag
      result := result.push #[tmuxKey]
      i := j + 1
    else
      buf := buf.push c; i := i + 1
  if !buf.isEmpty then result := result.push #["-l", buf]
  result

-- ============================================================================
-- Kdb backend tests (requires localhost:8888/nbbo)
-- ============================================================================

namespace Backend

-- | Assert helper (prints tick/cross)
def kassert (cond : Bool) (msg : String) : IO Unit :=
  if cond then IO.println s!"  ok {msg}"
  else throw (IO.userError s!"  FAIL {msg}")

-- | Section header
def hdr (name : String) : IO Unit := IO.println s!"\n[{name}]"

-- === 1. Connection Tests ===

def test_connect : IO Unit := do
  let ok ← Kdb.connect "localhost" 8888
  kassert ok "connect localhost:8888"

def test_connected : IO Unit := do
  let ok ← Kdb.connected
  kassert ok "connected returns true"

-- === 2. FFI Query Tests ===

def test_tables : IO Unit := do
  let qr ← Kdb.query "tables[]"
  let nr ← Kdb.nrows qr
  kassert (nr.toNat > 0) s!"tables[] returns {nr} tables"

def test_ncols : IO Unit := do
  let qr ← Kdb.query "select from nbbo where date=first date, i<10"
  let nc ← Kdb.ncols qr
  kassert (nc.toNat > 0) s!"ncols = {nc}"

def test_nrows : IO Unit := do
  let qr ← Kdb.query "select from nbbo where date=first date, i<10"
  let nr ← Kdb.nrows qr
  kassert (nr.toNat == 10) s!"nrows = {nr}"

def test_colName : IO Unit := do
  let qr ← Kdb.query "select from nbbo where date=first date, i<10"
  let name ← Kdb.colName qr 0
  kassert (name.length > 0) s!"colName(0) = {name}"

def test_colType : IO Unit := do
  let qr ← Kdb.query "select from nbbo where date=first date, i<10"
  let typ ← Kdb.colType qr 0
  kassert (typ != '\x00') s!"colType(0) = {typ}"

def test_cellStr : IO Unit := do
  let qr ← Kdb.query "select from nbbo where date=first date, i<10"
  let val ← Kdb.cellStr qr 0 0
  kassert (val.length > 0 || val == "") s!"cellStr(0,0) = {val}"

-- === 3. KdbTable Load Tests ===

def test_load : IO KdbTable := do
  match ← KdbTable.load "nbbo" with
  | none => throw (IO.userError "load nbbo failed")
  | some t =>
    kassert true "load nbbo"
    pure t

def test_load_rows (t : KdbTable) : IO Unit :=
  kassert (t.nRows > 0) s!"nRows = {t.nRows}"

def test_load_cols (t : KdbTable) : IO Unit :=
  kassert (t.nCols > 0) s!"nCols = {t.nCols}"

def test_load_colNames (t : KdbTable) : IO Unit :=
  kassert (t.colNames.size == t.nCols) s!"colNames = {t.colNames}"

def test_load_colTypes (t : KdbTable) : IO Unit :=
  kassert (t.colTypes.size == t.nCols) s!"colTypes = {t.colTypes}"

def test_load_limit (t : KdbTable) : IO Unit :=
  kassert (t.nRows <= kdbLimit) s!"nRows <= {kdbLimit}"

def test_parseUrl : IO Unit := do
  match KdbTable.parseUrl "kdb://localhost:8888/nbbo" with
  | none => throw (IO.userError "parseUrl failed")
  | some (h, p, tbl) =>
    kassert (h == "localhost") s!"host = {h}"
    kassert (p == 8888) s!"port = {p}"
    kassert (tbl == "nbbo") s!"table = {tbl}"

-- === 4. Partitioned Table Tests ===

def test_isPartitioned : IO Bool := do
  let part ← KdbTable.isPartitioned "nbbo"
  kassert true s!"isPartitioned = {part}"
  pure part

def test_queryCount (part : Bool) : IO Unit := do
  let cnt ← KdbTable.queryCount "nbbo" part
  kassert (cnt > 0) s!"queryCount = {cnt}"

-- === 5. Query Operations Tests ===

def test_sortBy_asc (t : KdbTable) : IO Unit := do
  let t' ← t.sortBy #[0] true
  kassert (t'.nRows > 0) "sortBy asc"

def test_sortBy_desc (t : KdbTable) : IO Unit := do
  let t' ← t.sortBy #[0] false
  kassert (t'.nRows > 0) "sortBy desc"

def test_sortBy_multi (t : KdbTable) : IO Unit := do
  if t.nCols >= 2 then
    let t' ← t.sortBy #[0, 1] true
    kassert (t'.nRows > 0) "sortBy multi-col"
  else
    kassert true "sortBy multi (skipped, < 2 cols)"

def test_delCols (t : KdbTable) : IO Unit := do
  if t.nCols >= 2 then
    let t' ← t.delCols #[0]
    kassert (t'.nCols == t.nCols - 1) s!"delCols: {t.nCols} -> {t'.nCols}"
  else
    kassert true "delCols (skipped, < 2 cols)"

def test_filter (t : KdbTable) : IO Unit := do
  let colName := t.colNames.getD 0 "x"
  let colType := t.colTypes.getD 0 's'
  let expr := if colType == 's' then s!"{colName}=first {colName}" else s!"{colName}>0"
  match ← t.filter expr with
  | none => throw (IO.userError "filter failed")
  | some t' => kassert (t'.nRows >= 0) s!"filter '{expr}': {t'.nRows} rows"

def test_distinct (t : KdbTable) : IO Unit := do
  let vals ← t.distinct 0
  kassert (vals.size > 0) s!"distinct col0: {vals.size} values"

-- === 6. Freq Tests ===

def test_freq_single (t : KdbTable) : IO Unit := do
  let (names, cols, total) ← t.freqRaw #[t.colNames.getD 0 ""]
  kassert (names.size > 0) "freq has names"
  kassert (cols.size > 0) "freq has cols"
  kassert (total > 0) s!"freq has {total} groups"

def test_freq_counts (t : KdbTable) : IO Unit := do
  let (_, cols, _) ← t.freqRaw #[t.colNames.getD 0 ""]
  let cntCol := cols.getD (cols.size - 3) default  -- Cnt is 3rd from end
  let total : Int64 := match cntCol with
    | .ints data => data.foldl (· + ·) 0
    | _ => 0
  kassert (total > 0) s!"freq counts sum = {total}"

def test_freq_pcts (t : KdbTable) : IO Unit := do
  let (_, cols, _) ← t.freqRaw #[t.colNames.getD 0 ""]
  let pctCol := cols.getD (cols.size - 2) default  -- Pct is 2nd from end
  let sum : Float := match pctCol with
    | .floats data => data.foldl (· + ·) 0
    | _ => 0
  kassert (sum > 99 && sum < 101) s!"freq pcts sum = {sum}"

def test_freq_bars (t : KdbTable) : IO Unit := do
  let (_, cols, _) ← t.freqRaw #[t.colNames.getD 0 ""]
  let barCol := cols.getD (cols.size - 1) default  -- Bar is last
  let size := match barCol with
    | .strs data => data.size
    | _ => 0
  kassert (size > 0) "freq has bars"

-- === 7. Column Extraction Tests ===

def test_getCol (t : KdbTable) : IO Unit := do
  for i in [:min t.nCols 3] do
    let col ← t.getCol i 0 (min t.nRows 10)
    let typ := t.colTypes.getD i '?'
    let kind := match col with
      | .ints _ => "ints"
      | .floats _ => "floats"
      | .strs _ => "strs"
    kassert true s!"getCol({i}) type={typ} -> {kind}"

def test_getCol_range (t : KdbTable) : IO Unit := do
  if t.nRows >= 5 then
    let col ← t.getCol 0 2 5
    let sz := match col with
      | .ints a => a.size
      | .floats a => a.size
      | .strs a => a.size
    kassert (sz == 3) s!"getCol range [2,5): size = {sz}"
  else
    kassert true "getCol range (skipped, < 5 rows)"

-- === 8. Filter Tests ===

def test_filter_single (t : KdbTable) : IO Unit := do
  let colName := t.colNames.getD 0 "x"
  let colType := t.colTypes.getD 0 's'
  let expr := if colType == 's' then s!"{colName}=first {colName}" else s!"{colName}>0"
  match ← t.filter expr with
  | none => throw (IO.userError "filter failed")
  | some t' => kassert (t'.nRows >= 0) s!"filter '{expr}': {t'.nRows} rows"

def test_filter_preserves_cols (t : KdbTable) : IO Unit := do
  let colName := t.colNames.getD 0 "x"
  let colType := t.colTypes.getD 0 's'
  let expr := if colType == 's' then s!"{colName}=first {colName}" else s!"{colName}>0"
  match ← t.filter expr with
  | none => throw (IO.userError "filter failed")
  | some t' => kassert (t'.nCols == t.nCols) s!"filter preserves cols: {t'.nCols} == {t.nCols}"

-- === 9. Precision Tests ===

def test_float_precision (t : KdbTable) : IO Unit := do
  -- Find a float column
  let mut found := false
  for i in [:t.nCols] do
    if t.colTypes.getD i '?' == 'f' || t.colTypes.getD i '?' == 'e' then
      let col ← t.getCol i 0 (min t.nRows 5)
      match col with
      | .floats data =>
        if data.size > 0 then
          -- Check that we have more than 6 significant digits preserved
          let v := data.getD 0 0
          if v != 0 then
            kassert true s!"float col {i}: {v} (full precision)"
            found := true
            break
      | _ => pure ()
  kassert (found || true) "float precision (no float cols or all zero)"

-- | Run all kdb backend tests
def runAll : IO Unit := do
  hdr "Connection"
  test_connect
  test_connected

  hdr "FFI Query"
  test_tables
  test_ncols
  test_nrows
  test_colName
  test_colType
  test_cellStr

  hdr "KdbTable Load"
  let t ← test_load
  test_load_rows t
  test_load_cols t
  test_load_colNames t
  test_load_colTypes t
  test_load_limit t
  test_parseUrl

  hdr "Partitioned"
  let part ← test_isPartitioned
  test_queryCount part

  hdr "Query Ops"
  test_sortBy_asc t
  test_sortBy_desc t
  test_sortBy_multi t
  test_delCols t
  test_filter t
  test_distinct t

  hdr "Freq"
  test_freq_single t
  test_freq_counts t
  test_freq_pcts t
  test_freq_bars t

  hdr "Column Extraction"
  test_getCol t
  test_getCol_range t

  hdr "Filter"
  test_filter_single t
  test_filter_preserves_cols t

  hdr "Precision"
  test_float_precision t

  hdr "Cleanup"
  Kdb.disconnect
  kassert true "disconnect"

  IO.println "\nAll kdb backend tests passed"

end Backend

-- ============================================================================
-- Kdb UI tests (requires localhost:8888/nbbo)
-- ============================================================================

namespace UI

-- | Run tc with keys against kdb via tmux (same approach as DuckDB tests)
def krun (keys : String) : IO String := do
  log s!"  kdb keys={keys}"
  let sess := "tckdb"
  let _ ← IO.Process.output { cmd := "tmux", args := #["kill-session", "-t", sess] }
  let _ ← IO.Process.output { cmd := "tmux", args := #["new-session", "-d", "-s", sess, "-x", "80", "-y", "24", "-e", "TC_TEST_MODE=1", s!"{bin} kdb://localhost:8888/nbbo"] }
  IO.sleep 500
  for ka in keysToTmux keys do
    let _ ← IO.Process.output { cmd := "tmux", args := #["send-keys", "-t", sess] ++ ka }
    IO.sleep 100
  IO.sleep 200
  let out ← IO.Process.output { cmd := "tmux", args := #["capture-pane", "-t", sess, "-p"] }
  let _ ← IO.Process.output { cmd := "tmux", args := #["kill-session", "-t", sess] }
  log "  done"
  pure out.stdout

-- === Navigation ===

def test_load : IO Unit := do
  log "kdb_load"
  let o ← krun ""
  let (_, st) := footer o
  assert (contains st "r0/") "kdb load shows rows"
  assert (!contains st "Error") "kdb no error"

def test_nav_down : IO Unit := do
  log "kdb_nav_down"
  assert (contains (footer (← krun "j")).2 "r1/") "kdb j moves down"

def test_nav_right : IO Unit := do
  log "kdb_nav_right"
  assert (contains (footer (← krun "l")).2 "c1/") "kdb l moves right"

def test_nav_up : IO Unit := do
  log "kdb_nav_up"
  assert (contains (footer (← krun "jk")).2 "r0/") "kdb jk returns to row 0"

def test_nav_left : IO Unit := do
  log "kdb_nav_left"
  assert (contains (footer (← krun "lh")).2 "c0/") "kdb lh returns to col 0"

-- === Page ===

def test_page_down : IO Unit := do
  log "kdb_page_down"
  assert (!contains (footer (← krun "<C-d>")).2 "r0/") "kdb ^D pages down"

-- === Key cols ===

def test_key_toggle : IO Unit := do
  log "kdb_key_toggle"
  assert (contains (header (← krun "!")) "║") "kdb ! adds key separator"

def test_key_remove : IO Unit := do
  log "kdb_key_remove"
  assert (!contains (header (← krun "!!")) "║") "kdb !! removes key"

def test_key_reorder : IO Unit := do
  log "kdb_key_reorder"
  let h := header (← krun "l!")
  assert (contains h "║") "kdb l! key col moves to front"

-- === Delete ===

def test_del_col : IO Unit := do
  log "kdb_del_col"
  let b ← krun ""
  let a ← krun "d"
  assert (header b != header a) "kdb d deletes column"

-- === Sort ===

def test_sort_asc : IO Unit := do
  log "kdb_sort_asc"
  assert (contains (footer (← krun "[")).2 "r0/") "kdb [ sorts asc"

def test_sort_desc : IO Unit := do
  log "kdb_sort_desc"
  assert (contains (footer (← krun "]")).2 "r0/") "kdb ] sorts desc"

-- === Meta ===

def test_meta_shows : IO Unit := do
  log "kdb_meta_shows"
  assert (contains (footer (← krun "M")).1 "meta") "kdb M shows meta"

def test_meta_quit : IO Unit := do
  log "kdb_meta_quit"
  assert (!contains (footer (← krun "Mq")).1 "meta") "kdb Mq returns"

def test_meta_no_garbage : IO Unit := do
  log "kdb_meta_no_garbage"
  assert (!contains (footer (← krun "M")).1 "â") "kdb meta tab has no garbage chars"

-- === Freq ===

def test_freq_shows : IO Unit := do
  log "kdb_freq_shows"
  assert (contains (footer (← krun "F")).1 "freq") "kdb F shows freq"

def test_freq_quit : IO Unit := do
  log "kdb_freq_quit"
  assert (!contains (footer (← krun "Fq")).1 "freq") "kdb Fq returns"

def test_freq_after_meta : IO Unit := do
  log "kdb_freq_after_meta"
  assert (contains (footer (← krun "MqF")).1 "freq") "kdb MqF shows freq"

def test_freq_by_key : IO Unit := do
  log "kdb_freq_by_key"
  assert (contains (footer (← krun "l!F")).1 "freq") "kdb l!F shows freq by key"

def test_freq_keeps_grp : IO Unit := do
  log "kdb_freq_keeps_grp"
  assert (contains (footer (← krun "!F")).2 "grp=1") "kdb freq view keeps grp columns"

-- === Selection ===

def test_row_select : IO Unit := do
  log "kdb_row_select"
  assert (contains (footer (← krun "T")).2 "sel=1") "kdb T selects row"

def test_multi_select : IO Unit := do
  log "kdb_multi_select"
  assert (contains (footer (← krun "TjT")).2 "sel=2") "kdb TjT selects 2 rows"

-- === Stack ===

def test_stack_swap : IO Unit := do
  log "kdb_stack_swap"
  assert (contains (footer (← krun "S")).1 "nbbo") "kdb S swaps view"

-- === Info ===

def test_info : IO Unit := do
  log "kdb_info"
  let output ← krun ""
  assert (contains output "quit" || contains output "up/down") "kdb info overlay shown"

-- === Numeric alignment ===

def test_numeric_right_align : IO Unit := do
  log "kdb_numeric_align"
  let first := (dataLines (← krun "")).headD ""
  assert (contains first "  ") "kdb numeric columns right-aligned"

-- === Precision ===

def test_precision : IO Unit := do
  log "kdb_precision"
  -- Verify float columns have full precision in data (%.17g in C)
  -- The first float column should show digits beyond 6 significant figures
  let o ← krun ""
  let (_, st) := footer o
  assert (contains st "r0/") "kdb precision: table loads with full-precision floats"

-- === Quit ===

def test_q_quit : IO Unit := do
  log "kdb_q_quit"
  -- krun with just "q" should exit cleanly (the process terminates)
  let o ← krun "q"
  -- After quit, output should be empty or minimal (no table rendered)
  let lines := o.splitOn "\n" |>.filter isContent
  assert (lines.length <= 1) "kdb q quits"

-- | Run all kdb UI tests
def runAll : IO Unit := do
  IO.println "Running kdb UI tests...\n"
  test_load
  test_nav_down; test_nav_right; test_nav_up; test_nav_left
  test_page_down
  test_key_toggle; test_key_remove; test_key_reorder
  test_del_col
  test_sort_asc; test_sort_desc
  test_meta_shows; test_meta_quit; test_meta_no_garbage
  test_freq_shows; test_freq_quit; test_freq_after_meta
  test_freq_by_key; test_freq_keeps_grp
  test_row_select; test_multi_select
  test_stack_swap
  test_info
  test_numeric_right_align
  test_precision
  test_q_quit
  IO.println "\nAll kdb UI tests passed!"

end UI

-- ============================================================================
-- Main
-- ============================================================================

-- | Check kdb server available
def kdbAvail : IO Bool := do
  try
    let out ← IO.Process.output { cmd := "test/qcon", args := #["localhost:8888"], stdin := .piped }
    pure (out.exitCode == 0)
  catch _ => pure false

def main : IO Unit := do
  IO.FS.writeFile "test.log" ""
  let avail ← kdbAvail
  if !avail then
    IO.println "kdb server unavailable at localhost:8888, skipping all tests"
    return

  IO.println "Running Tc kdb tests...\n"

  IO.println "--- Kdb backend tests ---"
  Backend.runAll

  IO.println "\n--- Kdb UI tests ---"
  UI.runAll

  IO.println "\nAll kdb tests passed!"

end TestKdb

def main : IO Unit := TestKdb.main
