/-
  Core tests for Tc - CSV only (works with tc-core or tc)
  Run with: lake build core-test && .lake/build/bin/core-test
-/
import test.TestLib

open Test

namespace CoreTest

-- | Runner with binary path
def R (bin keys : String) (file : String := "") : IO String := runBin bin keys file

-- === Navigation tests ===

def test_nav_down (bin : String) : IO Unit := do
  log "nav_down"
  let output ← R bin "j" "data/basic.csv"
  assert (contains (footer output).2 "r1/") "j moves to row 1"

def test_nav_right (bin : String) : IO Unit := do
  log "nav_right"
  let output ← R bin "l" "data/basic.csv"
  assert (contains (footer output).2 "c1/") "l moves to col 1"

def test_nav_up (bin : String) : IO Unit := do
  log "nav_up"
  let output ← R bin "jk" "data/basic.csv"
  assert (contains (footer output).2 "r0/") "jk returns to row 0"

def test_nav_left (bin : String) : IO Unit := do
  log "nav_left"
  let output ← R bin "lh" "data/basic.csv"
  assert (contains (footer output).2 "c0/") "lh returns to col 0"

-- === Key column tests ===

def test_key_toggle (bin : String) : IO Unit := do
  log "key_col"
  let output ← R bin "!" "data/xkey.csv"
  assert (contains (header output) "║") "! adds key separator"

def test_key_remove (bin : String) : IO Unit := do
  log "key_remove"
  let output ← R bin "!!" "data/xkey.csv"
  assert (!contains (header output) "║") "!! removes key separator"

def test_key_reorder (bin : String) : IO Unit := do
  log "key_reorder"
  let output ← R bin "l!" "data/basic.csv"
  assert ((header output).take 5 |>.any (· == 'b')) "Key col moves to front"

-- === Delete tests ===

def test_del_col (bin : String) : IO Unit := do
  log "del_col"
  let hdr := header (← R bin "ld" "data/basic.csv")
  assert (contains hdr "a") "Has column a"
  assert (!contains hdr "b") "Column b deleted"

-- === Sort tests ===

def test_sort_asc (bin : String) : IO Unit := do
  log "sort_asc"
  let first := (dataLines (← R bin "[" "data/unsorted.csv")).headD ""
  assert (first.startsWith "1 " || contains first " 1 ") "[ sorts asc, first=1"

def test_sort_desc (bin : String) : IO Unit := do
  log "sort_desc"
  let first := (dataLines (← R bin "]" "data/unsorted.csv")).headD ""
  assert (first.startsWith "3 " || contains first " 3 ") "] sorts desc, first=3"

-- === Meta tests ===

def test_meta_shows (bin : String) : IO Unit := do
  log "meta"
  assert (contains (footer (← R bin "M" "data/basic.csv")).1 "meta") "M shows meta in tab"

def test_meta_col_info (bin : String) : IO Unit := do
  log "meta_col_info"
  let output ← R bin "M" "data/basic.csv"
  assert (contains output "column" || contains output "name") "Meta shows column info"

def test_meta_no_garbage (bin : String) : IO Unit := do
  log "meta_tab_no_garbage"
  assert (!contains (footer (← R bin "M" "data/basic.csv")).1 "â") "Meta tab has no garbage chars"

-- === Freq tests ===

def test_freq_shows (bin : String) : IO Unit := do
  log "freq"
  assert (contains (footer (← R bin "F" "data/basic.csv")).1 "freq") "F shows freq in tab"

def test_freq_after_meta (bin : String) : IO Unit := do
  log "freq_after_meta"
  assert (contains (footer (← R bin "MqF" "data/basic.csv")).1 "freq") "MqF shows freq"

def test_freq_by_key (bin : String) : IO Unit := do
  log "freq_by_key"
  assert (contains (footer (← R bin "l!F" "data/full.csv")).1 "freq") "l!F shows freq by key"

def test_freq_multi_key (bin : String) : IO Unit := do
  log "freq_multi_key"
  assert (contains (footer (← R bin "!l!F" "data/multi_freq.csv")).1 "freq") "!l!F shows multi-key freq"

def test_freq_keeps_grp (bin : String) : IO Unit := do
  log "freq_keeps_grp"
  assert (contains (footer (← R bin "!F" "data/basic.csv")).2 "grp=1") "Freq view keeps grp columns"

-- === Selection tests ===

def test_row_select (bin : String) : IO Unit := do
  log "row_select"
  assert (contains (footer (← R bin "T" "data/basic.csv")).2 "sel=1") "T selects row"

def test_multi_select (bin : String) : IO Unit := do
  log "multi_row_select"
  assert (contains (footer (← R bin "TjT" "data/full.csv")).2 "sel=2") "TjT selects 2 rows"

-- === Stack tests ===

def test_stack_swap (bin : String) : IO Unit := do
  log "stack_swap"
  assert (contains (footer (← R bin "S" "data/basic.csv")).1 "basic.csv") "S swaps/dups view"

def test_meta_quit (bin : String) : IO Unit := do
  log "meta_quit"
  assert (!contains (footer (← R bin "Mq" "data/basic.csv")).1 "meta") "Mq returns from meta"

def test_freq_quit (bin : String) : IO Unit := do
  log "freq_quit"
  assert (!contains (footer (← R bin "Fq" "data/basic.csv")).1 "freq") "Fq returns from freq"

-- === Info overlay ===

def test_info (bin : String) : IO Unit := do
  log "info"
  let output ← R bin "I" "data/basic.csv"
  assert (contains output "hjkl" || contains output "quit") "I shows info overlay"

-- === Precision/Width adjustment ===

def test_prec_inc (bin : String) : IO Unit := do
  log "prec_inc"
  let output ← R bin "," "data/floats.csv"
  assert (contains output "1.123" || contains output "1.1235") ", prefix works"

def test_prec_dec (bin : String) : IO Unit := do
  log "prec_dec"
  let first := (dataLines (← R bin "." "data/floats.csv")).headD ""
  assert (contains first "1.1") ". prefix works"

-- === Meta selection tests (M0/M1) ===

def test_meta_0 (bin : String) : IO Unit := do
  log "meta_0"
  let status := (footer (← R bin "M0" "data/null_col.csv")).2
  assert (contains status "sel=1" || contains status "rows=1") "M0 selects null columns"

def test_meta_1 (bin : String) : IO Unit := do
  log "meta_1"
  let status := (footer (← R bin "M1" "data/single_val.csv")).2
  assert (contains status "sel=1" || contains status "rows=1") "M1 selects single-value columns"

def test_meta_0_enter (bin : String) : IO Unit := do
  log "meta_0_enter"
  let hdr := header (← R bin "M0<ret>" "data/null_col.csv")
  assert (contains hdr "║" || contains hdr "|") "M0<ret> sets key cols"

def test_meta_1_enter (bin : String) : IO Unit := do
  log "meta_1_enter"
  let hdr := header (← R bin "M1<ret>" "data/single_val.csv")
  assert (contains hdr "║" || contains hdr "|") "M1<ret> sets key cols"

def test_meta_0_del (bin : String) : IO Unit := do
  log "meta_0_enter_delete"
  assert (contains (footer (← R bin "M0<ret>d" "data/null_col.csv")).2 "c0/1") "M0<ret>d deletes null column"

-- === Stdin parsing tests ===

def test_spaced_header (bin : String) : IO Unit := do
  log "spaced_header"
  assert (contains (footer (← R bin "" "data/spaced_header.txt")).2 "c0/3") "Spaced header: 3 columns"

-- === Freq enter tests ===

def test_freq_enter (bin : String) : IO Unit := do
  log "freq_enter"
  let (tab, status) := footer (← R bin "F<ret>" "data/multi_freq.csv")
  assert (contains tab "multi_freq") "F<ret> pops to parent"
  assert (contains status "r0/3") "F<ret> filters to 3 rows"

-- === Cursor tracking ===

def test_key_cursor (bin : String) : IO Unit := do
  log "key_cursor"
  assert (contains (footer (← R bin "l!" "data/basic.csv")).2 "c0/") "Cursor tracks after key toggle"

-- === No stderr ===

def test_no_stderr : IO Unit := do
  log "no_stderr"
  let out ← IO.Process.output { cmd := "grep", args := #["-r", "eprintln", "Tc/", "--exclude=App.lean", "--exclude-dir=App"] }
  assert (out.stdout.trim.isEmpty) "No eprintln in Tc/ (except App entry points)"

-- === Search tests ===

def test_search_jump (bin : String) : IO Unit := do
  log "search_jump"
  assert (contains (footer (← R bin "l/" "data/basic.csv")).2 "r2/") "/ search finds x at row 2"

def test_search_next (bin : String) : IO Unit := do
  log "search_next"
  assert (contains (footer (← R bin "l/n" "data/basic.csv")).2 "r4/") "n finds next x at row 4"

def test_search_prev (bin : String) : IO Unit := do
  log "search_prev"
  assert (contains (footer (← R bin "l/N" "data/basic.csv")).2 "r0/") "N finds prev x (wraps to row 0)"

def test_col_search (bin : String) : IO Unit := do
  log "col_search"
  assert (contains (footer (← R bin "s" "data/basic.csv")).2 "c0/") "s col search jumps to column"

-- === Enter key tests ===

def test_q_quit (bin : String) : IO Unit := do
  log "q_quit_empty_stack"
  let sess := "tctest"
  let _ ← IO.Process.output { cmd := "tmux", args := #["kill-session", "-t", sess] }
  let _ ← IO.Process.output { cmd := "tmux", args := #["new-session", "-d", "-s", sess, "-x", "80", "-y", "24", "-e", "TC_TEST_MODE=1", s!"{bin} data/basic.csv"] }
  IO.sleep 200
  let _ ← IO.Process.output { cmd := "tmux", args := #["send-keys", "-t", sess, "-l", "q"] }
  IO.sleep 200
  let check ← IO.Process.output { cmd := "tmux", args := #["has-session", "-t", sess] }
  assert (check.exitCode != 0) "q on empty stack quits (session closed)"

-- === Folder tests ===

def test_folder_no_args (bin : String) : IO Unit := do
  log "folder_no_args"
  let output ← R bin ""
  assert (contains output "[/") "No-args shows folder view with absolute path"
  assert (contains output "path") "Folder view has path column"

def test_folder_D (bin : String) : IO Unit := do
  log "folder_D_key"
  assert (contains (← R bin "D" "data/basic.csv") "[/") "D pushes folder view with absolute path"

def test_folder_tab (bin : String) : IO Unit := do
  log "folder_tab_path"
  let (tab, _) := footer (← R bin "")
  assert (contains tab "[/") "Folder tab shows absolute path (starts with /)"
  assert (contains tab "/Tc]") "Folder tab shows absolute path (ends with /Tc])"

def test_folder_enter (bin : String) : IO Unit := do
  log "folder_enter_dir"
  let (tab, status) := footer (← R bin "<ret>")
  assert (contains tab "[/") "Enter on dir pushes new folder view"
  assert (contains status "r0/") "Entered directory has rows"

def test_folder_relative (bin : String) : IO Unit := do
  log "folder_path_relative"
  let output ← R bin ""
  assert (contains output "..") "Path shows entry name"
  assert (not (contains output "/home/dh/repo/Tc/..")) "Path column is relative"

def test_folder_pop (bin : String) : IO Unit := do
  log "folder_pop"
  assert (contains (← R bin "jjj<ret>q") "[/") "q pops back to parent folder"

def test_folder_prefix (bin : String) : IO Unit := do
  log "folder_prefix"
  let (_, s1) := footer (← R bin "")
  let (_, s2) := footer (← R bin ",")
  let r1 := s1.splitOn "r0/" |>.getD 1 "" |>.takeWhile (·.isDigit)
  let r2 := s2.splitOn "r0/" |>.getD 1 "" |>.takeWhile (·.isDigit)
  assert (r2.toNat?.getD 0 >= r1.toNat?.getD 0) ", prefix works in folder"

def test_folder_del (bin : String) : IO Unit := do
  log "folder_del"
  let (_, s1) := footer (← R bin "")
  let (_, s2) := footer (← R bin "d")
  assert (s1 == s2) "d in folder view (test mode) keeps view unchanged"

-- === Run all core tests with specified binary ===

def runTests (bin : String) : IO Unit := do
  -- Navigation
  test_nav_down bin; test_nav_right bin; test_nav_up bin; test_nav_left bin
  -- Key column
  test_key_toggle bin; test_key_remove bin; test_key_reorder bin
  -- Delete
  test_del_col bin
  -- Sort
  test_sort_asc bin; test_sort_desc bin
  -- Meta
  test_meta_shows bin; test_meta_col_info bin; test_meta_no_garbage bin
  -- Freq
  test_freq_shows bin; test_freq_after_meta bin; test_freq_by_key bin
  test_freq_multi_key bin; test_freq_keeps_grp bin
  -- Selection
  test_row_select bin; test_multi_select bin
  -- Stack
  test_stack_swap bin; test_meta_quit bin; test_freq_quit bin
  -- Info
  test_info bin
  -- Precision/Width
  test_prec_inc bin; test_prec_dec bin
  -- Meta M0/M1
  test_meta_0 bin; test_meta_1 bin; test_meta_0_enter bin
  test_meta_1_enter bin; test_meta_0_del bin
  -- Freq enter
  test_freq_enter bin
  -- Stdin parsing
  test_spaced_header bin
  -- Cursor tracking
  test_key_cursor bin
  -- No stderr
  test_no_stderr
  -- Search
  test_search_jump bin; test_search_next bin; test_search_prev bin; test_col_search bin
  -- Enter key
  test_q_quit bin
  -- Folder
  test_folder_no_args bin; test_folder_D bin; test_folder_tab bin
  test_folder_enter bin; test_folder_relative bin; test_folder_pop bin
  test_folder_prefix bin; test_folder_del bin

end CoreTest
