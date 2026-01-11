/-
  ADBC tests - parquet files, folder view, full tc features
  Run with: lake build tc test-adbc && .lake/build/bin/test-adbc
-/
import Tc.Data.ADBC.Table
import test.TestUtil

namespace Test

-- | Run full tc
def runFull (keys : String) (file : String := "") : IO String :=
  run ".lake/build/bin/tc" keys file

-- | Run folder mode (no file arg)
def runFolder (keys : String) : IO String := runFull keys

-- === Navigation tests (parquet) ===

def test_page_down : IO Unit := do
  log "page_down"
  let output ← runFull "<C-d>" "data/sample.parquet"
  let (_, status) := footer output
  assert (!contains status "r0/") "Ctrl-D pages down past row 0"

def test_page_up : IO Unit := do
  log "page_up"
  let output ← runFull "<C-d><C-u>" "data/sample.parquet"
  let (_, status) := footer output
  assert (contains status "r0/") "Ctrl-D then Ctrl-U returns to row 0"

def test_page_down_scrolls : IO Unit := do
  log "page_down_scrolls"
  let without ← runFull "" "data/sample.parquet"
  let withPgdn ← runFull "<C-d>" "data/sample.parquet"
  let (_, status1) := footer without
  let (_, status2) := footer withPgdn
  assert (status1 != status2) "Page down changes status"

def test_last_col_visible : IO Unit := do
  log "last_col_visible"
  let output ← runFull "llllllllllllllllllll" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  let nonWs := first.toList.filter (!·.isWhitespace) |>.length
  assert (nonWs > 0) "Last col shows data"

-- === Delete tests (parquet) ===

def test_delete_twice : IO Unit := do
  log "del_twice"
  let output ← runFull "dd" "data/sample.parquet"
  let hdr := header output
  assert (!contains hdr "id") "id deleted"
  assert (!contains hdr "age") "age deleted"
  assert (contains hdr "year") "year remains"

def test_delete_then_key_then_freq : IO Unit := do
  log "del_key_freq"
  let output ← runFull "dl!F" "data/sample.parquet"
  let (tab, status) := footer output
  assert (contains tab "freq") "D+key+F shows freq"
  assert (!contains status "Error") "No error"

-- === Sort tests (parquet) ===

def test_parquet_sort_asc : IO Unit := do
  log "parquet_sort_asc"
  let output ← runFull "l[" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first " 18 ") "[ on age sorts asc, age=18"

def test_parquet_sort_desc : IO Unit := do
  log "parquet_sort_desc"
  let output ← runFull "l]" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first " 80 ") "] on age sorts desc, age=80"

-- === Meta tests (parquet) ===

def test_parquet_meta : IO Unit := do
  log "parquet_meta"
  let output ← runFull "M" "data/sample.parquet"
  let (tab, _) := footer output
  assert (contains tab "meta") "M on parquet shows meta"

def test_parquet_meta_0_null_cols : IO Unit := do
  log "parquet_meta_0"
  let output ← runFull "M0" "data/nyse/1.parquet"
  let (_, status) := footer output
  assert (contains status "sel=9") "M0 on parquet selects 9 null columns"

def test_parquet_meta_0_enter_groups : IO Unit := do
  log "parquet_meta_0_enter"
  let output ← runFull "M0<ret>" "data/nyse/1.parquet"
  let (tab, status) := footer output
  assert (tab.startsWith "[1.parquet]") "M0<ret> returns to parent view"
  assert (contains status "grp=9") "M0<ret> groups 9 null columns"

-- === Freq tests (parquet) ===

def test_freq_parquet : IO Unit := do
  log "freq_parquet"
  let output ← runFull "F" "data/sample.parquet"
  let (tab, _) := footer output
  assert (contains tab "freq") "F on parquet shows freq"

-- === Misc (parquet) ===

def test_numeric_right_align : IO Unit := do
  log "numeric_align"
  let output ← runFull "" "data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first "  ") "Numeric columns right-aligned"

-- === Enter key ===

def test_enter_no_quit_parquet : IO Unit := do
  log "enter_no_quit_parquet"
  let output ← runFull "<ret>j" "data/nyse/1.parquet"
  let (_, status) := footer output
  assert (contains status "r1/") "Enter on parquet should not quit (j moves to r1)"

-- === Folder tests ===

def test_folder_no_args : IO Unit := do
  log "folder_no_args"
  let output ← runFolder ""
  assert (contains output "[/") "No-args shows folder view with absolute path"
  assert (contains output "path") "Folder view has path column"

def test_folder_D_key : IO Unit := do
  log "folder_D_key"
  let output ← runFull "D" "data/basic.csv"
  assert (contains output "[/") "D pushes folder view with absolute path"

def test_folder_tab_path : IO Unit := do
  log "folder_tab_path"
  let output ← runFolder ""
  let (tab, _) := footer output
  assert (contains tab "[/") "Folder tab shows absolute path (starts with /)"
  assert (contains tab "/Tc]") "Folder tab shows absolute path (ends with /Tc])"

def test_folder_enter_dir : IO Unit := do
  log "folder_enter_dir"
  let output ← runFolder "<ret>"
  let (tab, status) := footer output
  assert (contains tab "[/") "Enter on dir pushes new folder view"
  assert (contains status "r0/") "Entered directory has rows"

def test_folder_path_relative : IO Unit := do
  log "folder_path_relative"
  let output ← runFolder ""
  assert (contains output "..") "Path shows entry name"
  assert (not (contains output "/home/dh/repo/Tc/..")) "Path column is relative"

def test_folder_pop : IO Unit := do
  log "folder_pop"
  let output ← runFolder "jjj<ret>q"
  assert (contains output "[/") "q pops back to parent folder"

def test_folder_prefix : IO Unit := do
  log "folder_prefix"
  let before ← runFolder ""
  let after ← runFolder ","
  let (_, status1) := footer before
  let (_, status2) := footer after
  let r1 := status1.splitOn "r0/" |>.getD 1 "" |>.takeWhile (·.isDigit)
  let r2 := status2.splitOn "r0/" |>.getD 1 "" |>.takeWhile (·.isDigit)
  assert (r2.toNat?.getD 0 >= r1.toNat?.getD 0) ", prefix works in folder"

def test_folder_del : IO Unit := do
  log "folder_del"
  let before ← runFolder ""
  let after ← runFolder "d"
  let (_, s1) := footer before
  let (_, s2) := footer after
  assert (s1 == s2) "d in folder view (test mode) keeps view unchanged"

-- === Run all ADBC tests ===

def main : IO Unit := do
  IO.FS.writeFile "test.log" ""
  IO.println "Running Tc ADBC tests (parquet/folder)...\n"
  let ok ← Tc.AdbcTable.init
  if !ok then throw (IO.userError "Backend init failed")

  -- Navigation (parquet)
  test_page_down
  test_page_up
  test_page_down_scrolls
  test_last_col_visible

  -- Delete (parquet)
  test_delete_twice
  test_delete_then_key_then_freq

  -- Sort (parquet)
  test_parquet_sort_asc
  test_parquet_sort_desc

  -- Meta (parquet)
  test_parquet_meta
  test_parquet_meta_0_null_cols
  test_parquet_meta_0_enter_groups

  -- Freq (parquet)
  test_freq_parquet

  -- Misc
  test_numeric_right_align

  -- Enter key
  test_enter_no_quit_parquet

  -- Folder
  test_folder_no_args
  test_folder_D_key
  test_folder_tab_path
  test_folder_enter_dir
  test_folder_path_relative
  test_folder_pop
  test_folder_prefix
  test_folder_del

  Tc.AdbcTable.shutdown
  IO.println "\nAll ADBC tests passed!"

end Test

def main : IO Unit := Test.main
