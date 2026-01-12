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

  -- Parquet meta M0
  test_parquet_meta_0_null_cols
  test_parquet_meta_0_enter_groups

  -- Parquet misc
  test_numeric_right_align

  -- Parquet enter
  test_enter_no_quit_parquet

  Tc.AdbcTable.shutdown
  IO.println "\nAll tests passed!"

end Test

def main : IO Unit := Test.main
