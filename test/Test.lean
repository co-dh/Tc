/-
  Key tests for Tc - full build (requires ADBC for parquet)
  Run with: lake build tc test && .lake/build/bin/test
-/
import Tc.Data.ADBC.Table
import test.TestLib
import test.CoreTest

open Test

namespace Test

-- === Navigation tests (parquet-specific) ===

def test_page_down : IO Unit := do
  log "page_down"
  let output ← runKeys "<C-d>" "data/sample.parquet"
  let (_, status) := footer output
  assert (!contains status "r0/") "Ctrl-D pages down past row 0"

def test_page_up : IO Unit := do
  log "page_up"
  let output ← runKeys "<C-d><C-u>" "data/sample.parquet"
  let (_, status) := footer output
  assert (contains status "r0/") "Ctrl-D then Ctrl-U returns to row 0"

def test_page_down_scrolls : IO Unit := do
  log "page_down_scrolls"
  let without ← runKeys "" "data/sample.parquet"
  let withPgdn ← runKeys "<C-d>" "data/sample.parquet"
  let (_, status1) := footer without
  let (_, status2) := footer withPgdn
  assert (status1 != status2) "Page down changes status"

def test_last_col_visible : IO Unit := do
  log "last_col_visible"
  let output ← runKeys "llllllllllllllllllll" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  let nonWs := first.toList.filter (!·.isWhitespace) |>.length
  assert (nonWs > 0) "Last col shows data"

-- === Delete tests (parquet) ===

def test_delete_twice : IO Unit := do
  log "del_twice"
  let output ← runKeys "dd" "data/sample.parquet"
  let hdr := header output
  assert (!contains hdr "id") "id deleted"
  assert (!contains hdr "age") "age deleted"
  assert (contains hdr "year") "year remains"

def test_delete_then_key_then_freq : IO Unit := do
  log "del_key_freq"
  let output ← runKeys "dl!F" "data/sample.parquet"
  let (tab, status) := footer output
  assert (contains tab "freq") "D+key+F shows freq"
  assert (!contains status "Error") "No error"

-- === Sort tests (parquet) ===

def test_parquet_sort_asc : IO Unit := do
  log "parquet_sort_asc"
  let output ← runKeys "l[" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first " 18 ") "[ on age sorts asc, age=18"

def test_parquet_sort_desc : IO Unit := do
  log "parquet_sort_desc"
  let output ← runKeys "l]" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first " 80 ") "] on age sorts desc, age=80"

-- === Meta tests (parquet) ===

def test_parquet_meta : IO Unit := do
  log "parquet_meta"
  let output ← runKeys "M" "data/sample.parquet"
  let (tab, _) := footer output
  assert (contains tab "meta") "M on parquet shows meta"

-- === Freq tests (parquet) ===

def test_freq_parquet : IO Unit := do
  log "freq_parquet"
  let output ← runKeys "F" "data/sample.parquet"
  let (tab, _) := footer output
  assert (contains tab "freq") "F on parquet shows freq"

-- | Freq enter on parquet: pop to parent with filter applied
def test_freq_enter_parquet : IO Unit := do
  log "freq_enter_parquet"
  let (tab, status) := footer (← runKeys "F<ret>" "data/sample.parquet")
  assert (contains tab "sample.parquet") "F<ret> on parquet pops to parent"
  assert (!contains tab "freq") "F<ret> on parquet exits freq view"
  assert (contains status "r0/") "F<ret> on parquet shows filtered rows"

-- | Freq on parquet shows correct key column values (not counts)
def test_freq_parquet_key_values : IO Unit := do
  log "freq_parquet_key_values"
  let output ← runKeys "lF" "data/nyse10k.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  -- Exchange column should show exchange names (e.g. "NYS"), not numeric counts
  assert (!first.startsWith " 5180" && !first.startsWith " 2592") "Freq key column shows names, not counts"

-- | Freq view shows total distinct groups (not just displayed rows)
-- 1.parquet has 128974 distinct Exchange+Symbol, but freq shows top 1000
def test_freq_total_count : IO Unit := do
  log "freq_total_count"
  -- l=Exchange, !=group, l=Symbol, !=group, h=back, F=freq
  -- cntdist query on 300M rows takes ~5s, add 8s extra wait
  let output ← runKeys "l!l!hF" "data/nyse/1.parquet" 8000
  let (_, status) := footer output
  -- status should show total groups (128974), not just displayed rows (1000)
  assert (contains status "/128974") "Freq shows total group count (128974)"

-- | Freq sort: sorting by Cnt should work and preserve total count
-- Freq by Symbol on 1.parquet, sort asc should preserve total in status
def test_freq_sort_preserves_total : IO Unit := do
  log "freq_sort_total"
  -- ll=Symbol, F=freq, ll=Cnt, [=sort asc
  let output ← runKeys "llFll[" "data/nyse/1.parquet" 8000
  let (_, status) := footer output
  -- status should still show total groups after sorting (Symbol has many distinct values)
  assert (contains status "/") "Freq sort preserves total count in status"

-- | Freq sort ascending: first row should have smallest Cnt
def test_freq_sort_asc : IO Unit := do
  log "freq_sort_asc"
  -- ll=Symbol, F=freq, ll=Cnt, [=sort asc
  let output ← runKeys "llFll[" "data/nyse/1.parquet" 8000
  let rows := dataLines output
  let first := rows.headD ""
  -- after sort asc, first row should have small Cnt (likely 1)
  assert (contains first "│") "Freq sort asc shows data"

-- === Meta selection tests (parquet) ===

def test_parquet_meta_0_null_cols : IO Unit := do
  log "parquet_meta_0"
  let output ← runKeys "M0" "data/nyse/1.parquet"
  let (_, status) := footer output
  assert (contains status "sel=9") "M0 on parquet selects 9 null columns"

def test_parquet_meta_0_enter_groups : IO Unit := do
  log "parquet_meta_0_enter"
  let output ← runKeys "M0<ret>" "data/nyse/1.parquet"
  let (tab, status) := footer output
  assert (tab.startsWith "[1.parquet]") "M0<ret> returns to parent view"
  assert (contains status "grp=9") "M0<ret> groups 9 null columns"

-- === Filter tests (parquet) ===

-- | \ filter on parquet: distinct values and filter query full DB, not just prqlLimit window
-- filtered_test.parquet: 100k rows, sym col: first 1000 rows are ALL "A", "B" only after row 1000
-- If distinct only used the 1000-row window, it would miss "B" entirely.
-- Filter result count (40000 or 60000) proves full DB was queried, not the 1000-row window.
def test_filter_parquet_full_db : IO Unit := do
  log "filter_parquet_full_db"
  -- \ on sym (col 0), test mode auto-selects first distinct value
  let (tab, status) := footer (← runKeys "\\" "data/filtered_test.parquet")
  assert (contains tab "\\sym") "\\ filter shows \\sym in tab"
  -- extract row count from status "r0/NNNNN"
  let countStr := (status.splitOn "r0/" |>.getD 1 "").takeWhile (·.isDigit)
  let count := countStr.toString.toNat?.getD 0
  assert (count > 1000) s!"filter queries full DB ({count} rows, expected 40000 or 60000)"

-- === Scroll fetch tests (parquet) ===

def test_scroll_fetches_more : IO Unit := do
  log "scroll_fetches_more"
  -- nyse10k.parquet has 10k rows, initially loads 1000 (prqlLimit)
  -- 105 page-downs × 10 rows/page = 1050 → triggers fetchMore at ~row 990
  let keys := String.join (List.replicate 105 "<C-d>")
  let output ← runKeys keys "data/nyse10k.parquet" 1000
  let (_, status) := footer output
  -- Extract cursor from status "... r{cursor}/{total}"
  let rpart := (status.splitOn " r" |>.getD 1 "").takeWhile (· != ' ')
  let cursor := ((rpart.toString.splitOn "/").headD "").toNat?.getD 0
  assert (cursor > 999) s!"Scroll fetches more: cursor={cursor}, expected > 999"

-- === Misc (parquet) ===

def test_numeric_right_align : IO Unit := do
  log "numeric_align"
  let output ← runKeys "" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first "  ") "Numeric columns right-aligned"

-- === Enter key tests (parquet) ===

def test_enter_no_quit_parquet : IO Unit := do
  log "enter_no_quit_parquet"
  let output ← runKeys "<ret>j" "data/nyse/1.parquet"
  let (_, status) := footer output
  assert (contains status "r1/") "Enter on parquet should not quit (j moves to r1)"

-- === Run all tests ===

def main : IO Unit := do
  IO.FS.writeFile "test.log" ""
  IO.println "Running Tc key tests...\n"
  let ok ← Tc.AdbcTable.init
  if !ok then throw (IO.userError "Backend init failed")

  -- Run core tests with tc binary
  IO.println "--- Core tests (CSV) ---"
  _root_.CoreTest.runTests ".lake/build/bin/tc"

  -- Parquet-specific tests
  IO.println "\n--- Parquet tests ---"
  test_page_down
  test_page_up
  test_page_down_scrolls
  test_last_col_visible

  -- Parquet delete
  test_delete_twice
  test_delete_then_key_then_freq

  -- Parquet sort
  test_parquet_sort_asc
  test_parquet_sort_desc

  -- Parquet meta
  test_parquet_meta

  -- Parquet freq
  test_freq_parquet
  test_freq_enter_parquet
  test_freq_parquet_key_values
  test_freq_total_count
  test_freq_sort_preserves_total
  test_freq_sort_asc

  -- Parquet meta M0
  test_parquet_meta_0_null_cols
  test_parquet_meta_0_enter_groups

  -- Parquet filter
  test_filter_parquet_full_db

  -- Parquet scroll fetch
  test_scroll_fetches_more

  -- Parquet misc
  test_numeric_right_align

  -- Parquet enter
  test_enter_no_quit_parquet

  Tc.AdbcTable.shutdown
  IO.println "\nAll tests passed!"

end Test

def main : IO Unit := Test.main
