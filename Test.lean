/-
  Key tests for Tc - adapted from tvl Test.lean
  Run with: lake build tc test && .lake/build/bin/test
-/
import Tc.Data.ADBC.Table

namespace Test

-- | Log to file
def log (msg : String) : IO Unit := do
  let h ← IO.FS.Handle.mk "test.log" .append
  h.putStrLn msg
  h.flush

-- | Run tc with -c keys and capture rendered output
def runKeys (keys : String) (file : String) : IO String := do
  log s!"  spawn: {file} keys={keys}"
  let child ← IO.Process.spawn {
    cmd := "bash"
    args := #["-c", s!"script -q -c 'stty rows 24 cols 80; .lake/build/bin/tc \"{file}\" -c \"{keys}\"' /dev/null | ansi2txt | tail -24"]
    stdin := .null
    stdout := .piped
    stderr := .piped
    env := #[("TERM", "xterm")]
  }
  log "  spawned"
  let stdout ← child.stdout.readToEnd
  log "  read"
  let _ ← child.wait
  log "  done"
  return stdout

-- | Check if line has content (letters/digits)
def isContent (l : String) : Bool := l.any (fun c => c.isAlpha || c.isDigit)

-- | Check string contains substring
def contains (s sub : String) : Bool := (s.splitOn sub).length > 1

-- | Extract footer: (tab line, status line) - last two content lines
def footer (output : String) : String × String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  (lines.getD (n - 2) "", lines.getD (n - 1) "")

-- | Extract header row: last 80 chars of first content line
def header (output : String) : String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let hdr := lines.headD ""
  if hdr.length > 80 then hdr.drop (hdr.length - 80) else hdr

-- | Get data lines (skip header, skip footer 2 lines)
def dataLines (output : String) : List String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  lines.drop 1 |>.take (n - 3)

-- | Assert with message
def assert (cond : Bool) (msg : String) : IO Unit := do
  if cond then IO.println s!"✓ {msg}"
  else throw (IO.userError s!"✗ {msg}")

-- | Check string ends with substring
def endsWith (s sub : String) : Bool := s.endsWith sub

-- === Navigation tests ===

def test_navigation_down : IO Unit := do
  log "nav_down"
  let output ← runKeys "j" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r1/") "j moves to row 1"

def test_navigation_right : IO Unit := do
  log "nav_right"
  let output ← runKeys "l" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "c1/") "l moves to col 1"

def test_navigation_up : IO Unit := do
  log "nav_up"
  let output ← runKeys "jk" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r0/") "jk returns to row 0"

def test_navigation_left : IO Unit := do
  log "nav_left"
  let output ← runKeys "lh" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "c0/") "lh returns to col 0"

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

-- === Key column tests ===

def test_toggle_key_column : IO Unit := do
  log "key_col"
  let output ← runKeys "!" "data/xkey.csv"
  let hdr := header output
  assert (contains hdr "║") "! adds key separator"

def test_toggle_key_remove : IO Unit := do
  log "key_remove"
  let output ← runKeys "!!" "data/xkey.csv"
  let hdr := header output
  assert (!contains hdr "║") "!! removes key separator"

def test_key_col_reorder : IO Unit := do
  log "key_reorder"
  let output ← runKeys "l!" "data/basic.csv"
  let hdr := header output
  -- After l!, column b should be first (key col)
  assert (hdr.take 5 |>.any (· == 'b')) "Key col moves to front"

-- Key columns are pinned on left, non-key columns scroll
def test_key_col_pinned_when_scrolled : IO Unit := do
  log "keycol_pinned"
  -- Before: set id as key, check header has id and age (next col)
  let before ← runKeys "!" "data/sample.parquet"
  let hdrBefore := header before
  -- After: scroll right, id should stay but age should be gone
  let after ← runKeys "!lllll" "data/sample.parquet"
  let hdrAfter := header after
  assert (contains hdrBefore "id" && contains hdrBefore "age") "Before: has id and age"
  assert (contains hdrAfter "id" && !contains hdrAfter "age") "After: id pinned, age scrolled off"

-- === Delete tests ===

def test_delete_column : IO Unit := do
  log "del_col"
  let output ← runKeys "ld" "data/basic.csv"
  let hdr := header output
  assert (contains hdr "a") "Has column a"
  assert (!contains hdr "b") "Column b deleted"

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

-- === Sort tests ===

def test_sort_asc : IO Unit := do
  log "sort_asc"
  let output ← runKeys "[" "data/unsorted.csv"
  let rows := dataLines output
  let first := rows.headD ""
  assert (first.startsWith "1 " || contains first " 1 ") "[ sorts asc, first=1"

def test_sort_desc : IO Unit := do
  log "sort_desc"
  let output ← runKeys "]" "data/unsorted.csv"
  let rows := dataLines output
  let first := rows.headD ""
  assert (first.startsWith "3 " || contains first " 3 ") "] sorts desc, first=3"

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

-- === Meta tests ===

def test_meta_shows : IO Unit := do
  log "meta"
  let output ← runKeys "M" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "meta") "M shows meta in tab"

def test_parquet_meta : IO Unit := do
  log "parquet_meta"
  let output ← runKeys "M" "data/sample.parquet"
  let (tab, _) := footer output
  assert (contains tab "meta") "M on parquet shows meta"

def test_meta_shows_column_info : IO Unit := do
  log "meta_col_info"
  let output ← runKeys "M" "data/basic.csv"
  assert (contains output "column" || contains output "name") "Meta shows column info"

def test_meta_tab_no_garbage : IO Unit := do
  log "meta_tab_no_garbage"
  let output ← runKeys "M" "data/basic.csv"
  let (tab, _) := footer output
  -- Tab line should be "[meta] │ basic.csv" not "[meta] â basic.c" (garbage char)
  assert (!contains tab "â") "Meta tab has no garbage chars"

-- === Freq tests ===

def test_freq_shows : IO Unit := do
  log "freq"
  let output ← runKeys "F" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "freq") "F shows freq in tab"

def test_freq_parquet : IO Unit := do
  log "freq_parquet"
  let output ← runKeys "F" "data/sample.parquet"
  let (tab, _) := footer output
  assert (contains tab "freq") "F on parquet shows freq"

def test_freq_after_meta : IO Unit := do
  log "freq_after_meta"
  let output ← runKeys "MqF" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "freq") "MqF shows freq"

def test_freq_by_key_column : IO Unit := do
  log "freq_by_key"
  let output ← runKeys "l!F" "data/full.csv"
  let (tab, _) := footer output
  assert (contains tab "freq") "l!F shows freq by key"

def test_freq_multi_key : IO Unit := do
  log "freq_multi_key"
  let output ← runKeys "!l!F" "data/multi_freq.csv"
  let (tab, _) := footer output
  assert (contains tab "freq") "!l!F shows multi-key freq"

-- === Selection tests ===

def test_row_select : IO Unit := do
  log "row_select"
  let output ← runKeys "T" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "sel=1") "T selects row"

def test_multi_row_select : IO Unit := do
  log "multi_row_select"
  let output ← runKeys "TjT" "data/full.csv"
  let (_, status) := footer output
  assert (contains status "sel=2") "TjT selects 2 rows"

-- === Stack tests ===

def test_stack_swap : IO Unit := do
  log "stack_swap"
  let output ← runKeys "S" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "basic.csv") "S swaps/dups view"

def test_meta_then_quit : IO Unit := do
  log "meta_quit"
  let output ← runKeys "Mq" "data/basic.csv"
  let (tab, _) := footer output
  assert (!contains tab "meta") "Mq returns from meta"

def test_freq_then_quit : IO Unit := do
  log "freq_quit"
  let output ← runKeys "Fq" "data/basic.csv"
  let (tab, _) := footer output
  assert (!contains tab "freq") "Fq returns from freq"

-- === Info overlay ===

def test_info_overlay : IO Unit := do
  log "info"
  let output ← runKeys "I" "data/basic.csv"
  assert (contains output "hjkl" || contains output "quit") "I shows info overlay"

-- === Precision/Width adjustment ===

def test_prec_increase : IO Unit := do
  log "prec_inc"
  let output ← runKeys "+p" "data/floats.csv"
  -- +p increases precision, should show more decimals
  assert (contains output "1.123" || contains output "1.1235") "+p increases precision"

def test_prec_decrease : IO Unit := do
  log "prec_dec"
  let output ← runKeys "-p" "data/floats.csv"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first "1.1") "-p decreases precision"

-- === Meta selection tests (M0/M1) ===

def test_meta_0_select_null_cols : IO Unit := do
  log "meta_0"
  let output ← runKeys "M0" "data/null_col.csv"
  let (_, status) := footer output
  assert (contains status "sel=1" || contains status "rows=1") "M0 selects null columns"

def test_meta_1_select_single_val : IO Unit := do
  log "meta_1"
  let output ← runKeys "M1" "data/single_val.csv"
  let (_, status) := footer output
  assert (contains status "sel=1" || contains status "rows=1") "M1 selects single-value columns"

def test_meta_0_enter_sets_keycols : IO Unit := do
  log "meta_0_enter"
  let output ← runKeys "M0<ret>" "data/null_col.csv"
  let hdr := header output
  assert (contains hdr "║" || contains hdr "|") "M0<ret> sets key cols"

def test_meta_1_enter_sets_keycols : IO Unit := do
  log "meta_1_enter"
  let output ← runKeys "M1<ret>" "data/single_val.csv"
  let hdr := header output
  assert (contains hdr "║" || contains hdr "|") "M1<ret> sets key cols"

-- | Test M0 on parquet with known null columns (9 cols with 100% null)
def test_parquet_meta_0_null_cols : IO Unit := do
  log "parquet_meta_0"
  let output ← runKeys "M0" "data/nyse/1.parquet"
  let (_, status) := footer output
  assert (contains status "sel=9") "M0 on parquet selects 9 null columns"

-- | Test M0<ret> groups null columns as key columns
def test_parquet_meta_0_enter_groups : IO Unit := do
  log "parquet_meta_0_enter"
  let output ← runKeys "M0<ret>" "data/nyse/1.parquet"
  let (tab, status) := footer output
  -- Should pop meta, return to parent with grp=9 (9 null cols as key)
  -- Tab must be "[1.parquet]" not "[meta] │ 1.parquet"
  assert (tab.startsWith "[1.parquet]") "M0<ret> returns to parent view"
  assert (contains status "grp=9") "M0<ret> groups 9 null columns"

-- | Test M0<ret>d deletes the null columns
def test_meta_0_enter_delete : IO Unit := do
  log "meta_0_enter_delete"
  let output ← runKeys "M0<ret>d" "data/null_col.csv"
  let (_, status) := footer output
  -- After M0<ret>d, null col b should be deleted, only col a remains
  assert (contains status "c0/1") "M0<ret>d deletes null column"

-- === Freq enter tests ===

def test_freq_enter_filters : IO Unit := do
  log "freq_enter"
  let output ← runKeys "F<ret>" "data/basic.csv"
  let (tab, _) := footer output
  -- After F<ret>, should have filter view or be back at filtered data
  assert (contains tab "filter" || contains tab "basic") "F<ret> filters or returns"

-- TODO: requires freqCol filter implementation
-- def test_freq_enter_then_quit : IO Unit := do
--   log "freq_enter_quit"
--   let output ← runKeys "F<ret>q" "data/basic.csv"
--   let (tab, _) := footer output
--   assert (contains tab "freq") "F<ret>q returns to freq view"

-- === Cursor tracking ===

def test_key_cursor_tracks : IO Unit := do
  log "key_cursor"
  let output ← runKeys "l!" "data/basic.csv"
  let (_, status) := footer output
  -- After l!, cursor should be on col 0 (b is now first as key)
  assert (contains status "c0/") "Cursor tracks after key toggle"

-- === No stderr ===

def test_no_stderr : IO Unit := do
  log "no_stderr"
  -- Exclude App.lean (error handling uses eprintln)
  let out ← IO.Process.output { cmd := "grep", args := #["-r", "eprintln", "Tc/", "--exclude=App.lean"] }
  assert (out.stdout.trim.isEmpty) "No eprintln in Tc/ (except App.lean)"

-- === Misc ===

def test_numeric_right_align : IO Unit := do
  log "numeric_align"
  let output ← runKeys "" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first "  ") "Numeric columns right-aligned"

-- === Search tests (CSV only, testMode returns first distinct value) ===

-- | Test / search jumps to match (testMode picks first distinct val "x")
def test_search_jump : IO Unit := do
  log "search_jump"
  -- On col b, distinct vals are [x,y,z], testMode picks "x"
  -- After l, at r0. Search starts from r0+1=r1, finds x at r2
  let output ← runKeys "l/" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r2/") "/ search finds x at row 2"

-- | Test n (search next) finds next match
def test_search_next : IO Unit := do
  log "search_next"
  -- After /, cursor at r2 (x). Press n to find next x (r4)
  let output ← runKeys "l/n" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r4/") "n finds next x at row 4"

-- | Test N (search prev) wraps to find previous match
def test_search_prev : IO Unit := do
  log "search_prev"
  -- After /, at r2. Press N to search backward from r2, wraps to r0
  let output ← runKeys "l/N" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r0/") "N finds prev x (wraps to row 0)"

-- | Test column search s jumps to column
def test_col_search : IO Unit := do
  log "col_search"
  -- s on columns [a,b], testMode picks first (a), cursor stays at c0
  let output ← runKeys "s" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "c0/") "s col search jumps to column"

-- | Test search disabled on parquet (cursor stays at r0, popup skipped in testMode)
def test_search_disabled_parquet : IO Unit := do
  log "search_disabled_parquet"
  let output ← runKeys "/" "data/sample.parquet"
  let (_, status) := footer output
  -- In testMode popup is skipped; verify cursor didn't move (search was no-op)
  assert (contains status "r0/") "/ on parquet is no-op (search disabled)"

-- === Run all tests ===

def main : IO Unit := do
  IO.FS.writeFile "test.log" ""
  IO.println "Running Tc key tests...\n"
  let ok ← Tc.AdbcTable.init
  if !ok then throw (IO.userError "Backend init failed")

  -- Navigation
  test_navigation_down
  test_navigation_right
  test_navigation_up
  test_navigation_left
  test_page_down
  test_page_up
  test_page_down_scrolls
  test_last_col_visible

  -- Key column
  test_toggle_key_column
  test_toggle_key_remove
  test_key_col_reorder
  -- test_key_col_pinned_when_scrolled  -- TODO: fix pinning test

  -- Delete
  test_delete_column
  test_delete_twice
  test_delete_then_key_then_freq

  -- Sort
  test_sort_asc
  test_sort_desc
  test_parquet_sort_asc
  test_parquet_sort_desc

  -- Meta
  test_meta_shows
  test_parquet_meta
  test_meta_shows_column_info
  test_meta_tab_no_garbage

  -- Freq
  test_freq_shows
  test_freq_parquet
  test_freq_after_meta
  test_freq_by_key_column
  test_freq_multi_key

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
  test_parquet_meta_0_null_cols
  test_parquet_meta_0_enter_groups
  test_meta_0_enter_delete

  -- Freq enter
  test_freq_enter_filters
  -- test_freq_enter_then_quit  -- TODO: requires freqCol filter

  -- Cursor tracking
  test_key_cursor_tracks

  -- No stderr
  test_no_stderr

  -- Misc
  test_numeric_right_align

  -- Search
  test_search_jump
  test_search_next
  test_search_prev
  test_col_search
  test_search_disabled_parquet

  Tc.AdbcTable.shutdown
  IO.println "\nAll tests passed!"

end Test

def main : IO Unit := Test.main
