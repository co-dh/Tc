/-
  Core tests - CSV only, no ADBC/parquet
  Run with: lake build tc-core test-core && .lake/build/bin/test-core
-/
import test.TestUtil

namespace Test

-- | Run tc-core
def runCore (keys : String) (file : String := "") : IO String :=
  run ".lake/build/bin/tc-core" keys file

-- === Navigation tests (CSV) ===

def test_navigation_down : IO Unit := do
  log "nav_down"
  let output ← runCore "j" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r1/") "j moves to row 1"

def test_navigation_right : IO Unit := do
  log "nav_right"
  let output ← runCore "l" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "c1/") "l moves to col 1"

def test_navigation_up : IO Unit := do
  log "nav_up"
  let output ← runCore "jk" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r0/") "jk returns to row 0"

def test_navigation_left : IO Unit := do
  log "nav_left"
  let output ← runCore "lh" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "c0/") "lh returns to col 0"

-- === Key column tests ===

def test_toggle_key_column : IO Unit := do
  log "key_col"
  let output ← runCore "!" "data/xkey.csv"
  let hdr := header output
  assert (contains hdr "║") "! adds key separator"

def test_toggle_key_remove : IO Unit := do
  log "key_remove"
  let output ← runCore "!!" "data/xkey.csv"
  let hdr := header output
  assert (!contains hdr "║") "!! removes key separator"

def test_key_col_reorder : IO Unit := do
  log "key_reorder"
  let output ← runCore "l!" "data/basic.csv"
  let hdr := header output
  assert (hdr.take 5 |>.any (· == 'b')) "Key col moves to front"

-- === Delete tests ===

def test_delete_column : IO Unit := do
  log "del_col"
  let output ← runCore "ld" "data/basic.csv"
  let hdr := header output
  assert (contains hdr "a") "Has column a"
  assert (!contains hdr "b") "Column b deleted"

-- === Sort tests ===

def test_sort_asc : IO Unit := do
  log "sort_asc"
  let output ← runCore "[" "data/unsorted.csv"
  let rows := dataLines output
  let first := rows.headD ""
  assert (first.startsWith "1 " || contains first " 1 ") "[ sorts asc, first=1"

def test_sort_desc : IO Unit := do
  log "sort_desc"
  let output ← runCore "]" "data/unsorted.csv"
  let rows := dataLines output
  let first := rows.headD ""
  assert (first.startsWith "3 " || contains first " 3 ") "] sorts desc, first=3"

-- === Meta tests ===

def test_meta_shows : IO Unit := do
  log "meta"
  let output ← runCore "M" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "meta") "M shows meta in tab"

def test_meta_shows_column_info : IO Unit := do
  log "meta_col_info"
  let output ← runCore "M" "data/basic.csv"
  assert (contains output "column" || contains output "name") "Meta shows column info"

def test_meta_tab_no_garbage : IO Unit := do
  log "meta_tab_no_garbage"
  let output ← runCore "M" "data/basic.csv"
  let (tab, _) := footer output
  assert (!contains tab "â") "Meta tab has no garbage chars"

-- === Freq tests ===

def test_freq_shows : IO Unit := do
  log "freq"
  let output ← runCore "F" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "freq") "F shows freq in tab"

def test_freq_after_meta : IO Unit := do
  log "freq_after_meta"
  let output ← runCore "MqF" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "freq") "MqF shows freq"

def test_freq_by_key_column : IO Unit := do
  log "freq_by_key"
  let output ← runCore "l!F" "data/full.csv"
  let (tab, _) := footer output
  assert (contains tab "freq") "l!F shows freq by key"

def test_freq_multi_key : IO Unit := do
  log "freq_multi_key"
  let output ← runCore "!l!F" "data/multi_freq.csv"
  let (tab, _) := footer output
  assert (contains tab "freq") "!l!F shows multi-key freq"

def test_freq_keeps_grp_cols : IO Unit := do
  log "freq_keeps_grp"
  let output ← runCore "!F" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "grp=1") "Freq view keeps grp columns"

-- === Selection tests ===

def test_row_select : IO Unit := do
  log "row_select"
  let output ← runCore "T" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "sel=1") "T selects row"

def test_multi_row_select : IO Unit := do
  log "multi_row_select"
  let output ← runCore "TjT" "data/full.csv"
  let (_, status) := footer output
  assert (contains status "sel=2") "TjT selects 2 rows"

-- === Stack tests ===

def test_stack_swap : IO Unit := do
  log "stack_swap"
  let output ← runCore "S" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "basic.csv") "S swaps/dups view"

def test_meta_then_quit : IO Unit := do
  log "meta_quit"
  let output ← runCore "Mq" "data/basic.csv"
  let (tab, _) := footer output
  assert (!contains tab "meta") "Mq returns from meta"

def test_freq_then_quit : IO Unit := do
  log "freq_quit"
  let output ← runCore "Fq" "data/basic.csv"
  let (tab, _) := footer output
  assert (!contains tab "freq") "Fq returns from freq"

-- === Info overlay ===

def test_info_overlay : IO Unit := do
  log "info"
  let output ← runCore "I" "data/basic.csv"
  assert (contains output "hjkl" || contains output "quit") "I shows info overlay"

-- === Precision/Width adjustment ===

def test_prec_increase : IO Unit := do
  log "prec_inc"
  let output ← runCore "," "data/floats.csv"
  assert (contains output "1.123" || contains output "1.1235") ", prefix works"

def test_prec_decrease : IO Unit := do
  log "prec_dec"
  let output ← runCore "." "data/floats.csv"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first "1.1") ". prefix works"

-- === Meta selection tests (M0/M1) ===

def test_meta_0_select_null_cols : IO Unit := do
  log "meta_0"
  let output ← runCore "M0" "data/null_col.csv"
  let (_, status) := footer output
  assert (contains status "sel=1" || contains status "rows=1") "M0 selects null columns"

def test_meta_1_select_single_val : IO Unit := do
  log "meta_1"
  let output ← runCore "M1" "data/single_val.csv"
  let (_, status) := footer output
  assert (contains status "sel=1" || contains status "rows=1") "M1 selects single-value columns"

def test_meta_0_enter_sets_keycols : IO Unit := do
  log "meta_0_enter"
  let output ← runCore "M0<ret>" "data/null_col.csv"
  let hdr := header output
  assert (contains hdr "║" || contains hdr "|") "M0<ret> sets key cols"

def test_meta_1_enter_sets_keycols : IO Unit := do
  log "meta_1_enter"
  let output ← runCore "M1<ret>" "data/single_val.csv"
  let hdr := header output
  assert (contains hdr "║" || contains hdr "|") "M1<ret> sets key cols"

def test_meta_0_enter_delete : IO Unit := do
  log "meta_0_enter_delete"
  let output ← runCore "M0<ret>d" "data/null_col.csv"
  let (_, status) := footer output
  assert (contains status "c0/1") "M0<ret>d deletes null column"

-- === Stdin parsing tests ===

def test_spaced_header : IO Unit := do
  log "spaced_header"
  let output ← runCore "" "data/spaced_header.txt"
  let (_, status) := footer output
  assert (contains status "c0/3") "Spaced header: 3 columns"

-- === Freq enter tests ===

def test_freq_enter_filters : IO Unit := do
  log "freq_enter"
  let output ← runCore "F<ret>" "data/multi_freq.csv"
  let (tab, status) := footer output
  assert (contains tab "multi_freq") "F<ret> pops to parent"
  assert (contains status "r0/3") "F<ret> filters to 3 rows"

-- === Cursor tracking ===

def test_key_cursor_tracks : IO Unit := do
  log "key_cursor"
  let output ← runCore "l!" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "c0/") "Cursor tracks after key toggle"

-- === No stderr ===

def test_no_stderr : IO Unit := do
  log "no_stderr"
  let out ← IO.Process.output { cmd := "grep", args := #["-r", "eprintln", "Tc/", "--exclude=App.lean", "--exclude=Core.lean"] }
  assert (out.stdout.trim.isEmpty) "No eprintln in Tc/ (except App*.lean)"

-- === Search tests ===

def test_search_jump : IO Unit := do
  log "search_jump"
  let output ← runCore "l/" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r2/") "/ search finds x at row 2"

def test_search_next : IO Unit := do
  log "search_next"
  let output ← runCore "l/n" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r4/") "n finds next x at row 4"

def test_search_prev : IO Unit := do
  log "search_prev"
  let output ← runCore "l/N" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r0/") "N finds prev x (wraps to row 0)"

def test_col_search : IO Unit := do
  log "col_search"
  let output ← runCore "s" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "c0/") "s col search jumps to column"

-- === q quit ===

def test_q_quit_empty_stack : IO Unit := do
  log "q_quit_empty_stack"
  let sess := "tctest"
  let _ ← IO.Process.output { cmd := "tmux", args := #["kill-session", "-t", sess] }
  let _ ← IO.Process.output { cmd := "tmux", args := #["new-session", "-d", "-s", sess, "-x", "80", "-y", "24", "-e", "TC_TEST_MODE=1", ".lake/build/bin/tc-core data/basic.csv"] }
  IO.sleep 200
  let _ ← IO.Process.output { cmd := "tmux", args := #["send-keys", "-t", sess, "-l", "q"] }
  IO.sleep 200
  let check ← IO.Process.output { cmd := "tmux", args := #["has-session", "-t", sess] }
  assert (check.exitCode != 0) "q on empty stack quits (session closed)"

-- === Run all core tests ===

def main : IO Unit := do
  IO.FS.writeFile "test.log" ""
  IO.println "Running Tc core tests (CSV only)...\n"

  -- Navigation
  test_navigation_down
  test_navigation_right
  test_navigation_up
  test_navigation_left

  -- Key column
  test_toggle_key_column
  test_toggle_key_remove
  test_key_col_reorder

  -- Delete
  test_delete_column

  -- Sort
  test_sort_asc
  test_sort_desc

  -- Meta
  test_meta_shows
  test_meta_shows_column_info
  test_meta_tab_no_garbage

  -- Freq
  test_freq_shows
  test_freq_after_meta
  test_freq_by_key_column
  test_freq_multi_key
  test_freq_keeps_grp_cols

  -- Selection
  test_row_select
  test_multi_row_select

  -- Stack
  test_stack_swap
  test_meta_then_quit
  test_freq_then_quit

  -- Info
  test_info_overlay

  -- Precision/Width
  test_prec_increase
  test_prec_decrease

  -- Meta M0/M1
  test_meta_0_select_null_cols
  test_meta_1_select_single_val
  test_meta_0_enter_sets_keycols
  test_meta_1_enter_sets_keycols
  test_meta_0_enter_delete

  -- Freq enter
  test_freq_enter_filters

  -- Stdin parsing
  test_spaced_header

  -- Cursor tracking
  test_key_cursor_tracks

  -- No stderr
  test_no_stderr

  -- Search
  test_search_jump
  test_search_next
  test_search_prev
  test_col_search

  -- Quit
  test_q_quit_empty_stack

  IO.println "\nAll core tests passed!"

end Test

def main : IO Unit := Test.main
