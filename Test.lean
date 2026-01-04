/-
  Key tests for Tc - run with: lake build && .lake/build/bin/test
  Adapted from tvl Test.lean
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
  assert (contains status "r1/") "j should move to row 1"

def test_navigation_right : IO Unit := do
  log "nav_right"
  let output ← runKeys "l" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "c1/") "l should move to col 1"

def test_page_down : IO Unit := do
  log "page_down"
  let output ← runKeys "<C-d>" "data/sample.parquet"
  let (_, status) := footer output
  assert (!contains status "r0/") "Ctrl-D should page down past row 0"

def test_page_down_scrolls : IO Unit := do
  log "page_down_scrolls"
  let without ← runKeys "" "data/sample.parquet"
  let withPgdn ← runKeys "<C-d>" "data/sample.parquet"
  let (_, status1) := footer without
  let (_, status2) := footer withPgdn
  assert (status1 != status2) s!"Page down should scroll: before={status1} after={status2}"

def test_last_col_visible : IO Unit := do
  log "last_col_visible"
  let output ← runKeys "llllllllllllllllllll" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  let nonWs := first.toList.filter (!·.isWhitespace) |>.length
  assert (nonWs > 0) s!"Last col should show data: {first}"

-- === Column operations ===

def test_toggle_key_column : IO Unit := do
  log "key_col"
  let output ← runKeys "!" "data/xkey.csv"
  let hdr := header output
  assert (contains hdr "║") "! should add key column separator (║)"

def test_toggle_key_remove : IO Unit := do
  log "key_remove"
  let output ← runKeys "!!" "data/xkey.csv"
  let hdr := header output
  assert (!contains hdr "║") "!! should remove key column separator"

def test_delete_column : IO Unit := do
  log "del_col"
  let output ← runKeys "ld" "data/basic.csv"
  let hdr := header output
  assert (contains hdr "a") "Should have column a"
  assert (!contains hdr "b") "Should not have column b"

def test_delete_twice : IO Unit := do
  log "del_twice"
  let output ← runKeys "dd" "data/sample.parquet"
  let hdr := header output
  assert (!contains hdr "id") "id should be deleted"
  assert (!contains hdr "age") "age should be deleted"
  assert (contains hdr "year") "year should remain"

-- === Sort tests ===

def test_sort_asc : IO Unit := do
  log "sort_asc"
  let output ← runKeys "[" "data/unsorted.csv"
  let rows := dataLines output
  let first := rows.headD ""
  assert (first.startsWith "1 " || contains first " 1 ") "[ should sort asc, first=1"

def test_sort_desc : IO Unit := do
  log "sort_desc"
  let output ← runKeys "]" "data/unsorted.csv"
  let rows := dataLines output
  let first := rows.headD ""
  assert (first.startsWith "3 " || contains first " 3 ") "] should sort desc, first=3"

def test_parquet_sort_asc : IO Unit := do
  log "parquet_sort_asc"
  let output ← runKeys "l[" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first " 18 ") "[ on age should sort asc, age=18"

def test_parquet_sort_desc : IO Unit := do
  log "parquet_sort_desc"
  let output ← runKeys "l]" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first " 80 ") "] on age should sort desc, age=80"

-- === Meta view ===

def test_meta_shows : IO Unit := do
  log "meta"
  let output ← runKeys "M" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "meta") "M should show meta in tab"

def test_parquet_meta : IO Unit := do
  log "parquet_meta"
  let output ← runKeys "M" "data/sample.parquet"
  let (tab, _) := footer output
  assert (contains tab "meta") "M should show meta"

-- === Freq view ===

def test_freq_in_tab : IO Unit := do
  log "freq_csv"
  let output ← runKeys "F" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "freq") "F should show freq in tab"

def test_freq_parquet : IO Unit := do
  log "freq_parquet"
  let output ← runKeys "F" "data/sample.parquet"
  let (tab, _) := footer output
  assert (contains tab "freq") "F on parquet should show freq"

def test_freq_after_meta : IO Unit := do
  log "freq_after_meta"
  let output ← runKeys "MqF" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "freq") "M then q then F should show freq"

-- === Selection tests ===

def test_row_select : IO Unit := do
  log "row_select"
  let output ← runKeys "T" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "sel=1") "T should select row (show sel=1)"

def test_multi_row_select : IO Unit := do
  log "multi_row_select"
  let output ← runKeys "TjT" "data/full.csv"
  let (_, status) := footer output
  assert (contains status "sel=2") "TjT should show sel=2"

-- === Info overlay ===

def test_info_overlay : IO Unit := do
  log "info"
  let output ← runKeys "I" "data/basic.csv"
  assert (contains output "hjkl" || contains output "quit") "I should show info overlay"

-- === Stack operations ===

def test_stack_dup : IO Unit := do
  log "stack_dup"
  let output ← runKeys "S" "data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "basic.csv") "S should swap/dup view"

def test_separator_not_shown_when_keycol_scrolled : IO Unit := do
  log "keycol_scroll"
  let output ← runKeys "!llllllllllllllllll" "data/sample.parquet"
  let hdr := header output
  -- Separator should NOT be in header since key col is scrolled off
  assert (!contains hdr "║") "Separator should not show when key col scrolled off"

def test_numeric_right_align : IO Unit := do
  log "numeric_align"
  let output ← runKeys "" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  -- Values should have leading spaces (right-aligned)
  assert (contains first "  ") "Should have spacing for alignment"

-- === Run all tests ===

def main : IO Unit := do
  IO.FS.writeFile "test.log" ""  -- clear log
  IO.println "Running Tc key tests...\n"
  let ok ← Tc.AdbcTable.init
  if !ok then throw (IO.userError "Backend init failed")

  -- Navigation
  test_navigation_down
  test_navigation_right
  test_page_down
  test_page_down_scrolls
  test_last_col_visible

  -- Column operations
  test_toggle_key_column
  test_toggle_key_remove
  test_delete_column
  test_delete_twice

  -- Sort
  test_sort_asc
  test_sort_desc
  test_parquet_sort_asc
  test_parquet_sort_desc

  -- Meta
  test_meta_shows
  test_parquet_meta

  -- Freq
  test_freq_in_tab
  test_freq_parquet
  test_freq_after_meta

  -- Selection
  test_row_select
  test_multi_row_select

  -- Info
  test_info_overlay

  -- Stack & misc
  test_stack_dup
  test_separator_not_shown_when_keycol_scrolled
  test_numeric_right_align

  Tc.AdbcTable.shutdown
  IO.println "\nAll tests passed!"

end Test

def main : IO Unit := Test.main
