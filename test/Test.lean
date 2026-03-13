/-
  Key tests for Tc
  Run with: lake run runtest
  Kdb tests: lake build testkdb && .lake/build/bin/testkdb
-/
import Tc.Data.ADBC.Table
import Tc.SourceConfig
import Tc.Nav
import Tc.View
import Tc.UI.Info
import Tc.Types
import Tc.Remote
import Tc.Data.Text
import test.TestPure

-- ============================================================================
-- Runtime UI tests
-- ============================================================================

namespace Test

open Tc

def bin := ".lake/build/bin/tc"

-- | Log to file
def log (msg : String) : IO Unit := do
  let h ← IO.FS.Handle.mk "test.log" .append
  h.putStrLn msg; h.flush

-- | Run tc with -c flag, no tmux. Headless mode renders to internal buffer.
def run (keys : String) (file : String := "") : IO String := do
  log s!"  run: {file} keys={keys}"
  let args := if file.isEmpty then #["-c", keys] else #[file, "-c", keys]
  let out ← IO.Process.output { cmd := bin, args }
  if !out.stderr.isEmpty then log s!"  stderr: {out.stderr.trimAscii.toString}"
  if out.exitCode != 0 then log s!"  exit: {out.exitCode}"
  log "  done"
  pure out.stdout

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

-- | Check if a file/directory exists (for skipping tests with untracked data)
def hasFile (path : String) : IO Bool :=
  try let _ ← IO.FS.Handle.mk path .read; pure true
  catch _ => pure false

-- | Skip guard for data/nyse directory
def needsNyse : IO Bool := hasFile "data/nyse/1.parquet"

-- | Skip guard for data/nyse10k.parquet
def needsNyse10k : IO Bool := hasFile "data/nyse10k.parquet"

-- | Skip guard for data/sample.parquet
def needsSample : IO Bool := hasFile "data/sample.parquet"

-- === Width stability tests ===

def test_width_stable_after_info : IO Unit := do
  log "width_stable_after_info"
  -- pac.csv has wide columns (deps, desc). Pressing I (info toggle) triggers re-render.
  -- Bug: Lean cached widths with min(maxColWidth=50), so re-render shrinks wide columns.
  let hdr1 := header (← run "" "data/pac.csv")
  let hdr2 := header (← run "I" "data/pac.csv")   -- toggle info on
  let hdr3 := header (← run "II" "data/pac.csv")   -- toggle info on then off
  assert (hdr1 == hdr2) s!"Info toggle must not change column widths: [{hdr1}] vs [{hdr2}]"
  assert (hdr1 == hdr3) s!"Info double-toggle must not change column widths"

-- === Sort tests (CSV) ===

def test_sort_asc : IO Unit := do
  log "sort_asc"
  let first := (dataLines (← run "[" "data/unsorted.csv")).headD ""
  assert (first.startsWith "1 " || contains first " 1 ") "[ sorts asc, first=1"

def test_sort_desc : IO Unit := do
  log "sort_desc"
  let first := (dataLines (← run "]" "data/unsorted.csv")).headD ""
  assert (first.startsWith "3 " || contains first " 3 ") "] sorts desc, first=3"

-- === Meta tests (CSV) ===

def test_meta_shows : IO Unit := do
  log "meta"
  assert (contains (footer (← run "M" "data/basic.csv")).1 "meta") "M shows meta in tab"

def test_meta_col_info : IO Unit := do
  log "meta_col_info"
  assert (contains (← run "M" "data/basic.csv") "column" || contains (← run "M" "data/basic.csv") "name") "Meta shows column info"

def test_meta_no_garbage : IO Unit := do
  log "meta_tab_no_garbage"
  assert (!contains (footer (← run "M" "data/basic.csv")).1 "â") "Meta tab has no garbage chars"

-- === Freq tests (CSV) ===

def test_freq_shows : IO Unit := do
  log "freq"
  assert (contains (footer (← run "F" "data/basic.csv")).1 "freq") "F shows freq in tab"

def test_freq_after_meta : IO Unit := do
  log "freq_after_meta"
  assert (contains (footer (← run "MqF" "data/basic.csv")).1 "freq") "MqF shows freq"

def test_freq_by_key : IO Unit := do
  log "freq_by_key"
  assert (contains (footer (← run "l!F" "data/full.csv")).1 "freq") "l!F shows freq by key"

def test_freq_multi_key : IO Unit := do
  log "freq_multi_key"
  assert (contains (footer (← run "!l!F" "data/multi_freq.csv")).1 "freq") "!l!F shows multi-key freq"

def test_freq_keeps_grp : IO Unit := do
  log "freq_keeps_grp"
  assert (contains (footer (← run "!F" "data/basic.csv")).2 "grp=1") "Freq view keeps grp columns"

-- === Precision/Width adjustment ===

def test_prec_inc : IO Unit := do
  log "prec_inc"
  assert (contains (← run "," "data/floats.csv") "1.123" || contains (← run "," "data/floats.csv") "1.1235") ", prefix works"

def test_prec_dec : IO Unit := do
  log "prec_dec"
  let first := (dataLines (← run "." "data/floats.csv")).headD ""
  assert (contains first "1.1") ". prefix works"

-- === Meta selection tests (M0/M1) ===

def test_meta_0 : IO Unit := do
  log "meta_0"
  let status := (footer (← run "M0" "data/null_col.csv")).2
  assert (contains status "sel=1" || contains status "rows=1") "M0 selects null columns"

def test_meta_1 : IO Unit := do
  log "meta_1"
  let status := (footer (← run "M1" "data/single_val.csv")).2
  assert (contains status "sel=1" || contains status "rows=1") "M1 selects single-value columns"

def test_meta_0_enter : IO Unit := do
  log "meta_0_enter"
  let hdr := header (← run "M0<ret>" "data/null_col.csv")
  assert (contains hdr "║" || contains hdr "|") "M0<ret> sets key cols"

def test_meta_1_enter : IO Unit := do
  log "meta_1_enter"
  let hdr := header (← run "M1<ret>" "data/single_val.csv")
  assert (contains hdr "║" || contains hdr "|") "M1<ret> sets key cols"


-- === Stdin parsing tests ===

def test_spaced_header : IO Unit := do
  log "spaced_header"
  assert (contains (footer (← run "" "data/spaced_header.txt")).2 "c0/3") "Spaced header: 3 columns"

-- === Freq enter tests ===

def test_freq_enter : IO Unit := do
  log "freq_enter"
  let (tab, status) := footer (← run "F<ret>" "data/multi_freq.csv")
  assert (contains tab "multi_freq") "F<ret> pops to parent"
  assert (contains status "r0/3") "F<ret> filters to 3 rows"

-- === No stderr ===

def test_no_stderr : IO Unit := do
  log "no_stderr"
  let out ← IO.Process.output { cmd := "grep", args := #["-r", "eprintln", "Tc/", "--exclude=App.lean", "--exclude-dir=App"] }
  assert (out.stdout.trimAscii.toString.isEmpty) "No eprintln in Tc/ (except App entry points)"

-- === Search tests ===

def test_search_jump : IO Unit := do
  log "search_jump"
  assert (contains (footer (← run "l/" "data/basic.csv")).2 "r2/") "/ search finds x at row 2"

def test_search_next : IO Unit := do
  log "search_next"
  assert (contains (footer (← run "l/n" "data/basic.csv")).2 "r4/") "n finds next x at row 4"

def test_search_prev : IO Unit := do
  log "search_prev"
  assert (contains (footer (← run "l/N" "data/basic.csv")).2 "r0/") "N finds prev x (wraps to row 0)"

def test_search_after_sort : IO Unit := do
  log "search_after_sort"
  -- sort desc on a (]) then move to col b (l) then search (/)
  -- sorted: 5,x 4,z 3,x 2,y 1,x → cursor at r0, search starts from r1, finds x at r2
  assert (contains (footer (← run "]l/" "data/basic.csv")).2 "r2/") "/ search after sort finds row 2"

def test_col_search : IO Unit := do
  log "col_search"
  assert (contains (footer (← run "s" "data/basic.csv")).2 "c0/") "s col search jumps to column"

-- === Folder tests ===

def test_folder_no_args : IO Unit := do
  log "folder_no_args"
  assert (contains (← run "") "[/") "No-args shows folder view with absolute path"

def test_folder_D : IO Unit := do
  log "folder_D_key"
  assert (contains (← run "D" "data/basic.csv") "[/") "D pushes folder view with absolute path"

def test_folder_tab : IO Unit := do
  log "folder_tab_path"
  let (tab, _) := footer (← run "")
  assert (contains tab "[/") "Folder tab shows absolute path (starts with /)"
  assert (contains tab "/Tc]") "Folder tab shows absolute path (ends with /Tc])"

def test_folder_enter : IO Unit := do
  log "folder_enter_dir"
  let (tab, status) := footer (← run "<ret>")
  assert (contains tab "[/") "Enter on dir pushes new folder view"
  assert (contains status "r0/") "Entered directory has rows"

def test_folder_relative : IO Unit := do
  log "folder_path_relative"
  let output ← run ""
  assert (contains output "..") "Path shows entry name"
  assert (not (contains output "/home/dh/repo/Tc/..")) "Path column is relative"

def test_folder_pop : IO Unit := do
  log "folder_pop"
  -- fixture dir: [sort, jjj → row 3 = subdir (dir), enter, q pops back
  assert (contains (← run "[jjj<ret>q" "data/test_folder") "[/") "q pops back to parent folder"

def test_folder_enter_symlink : IO Unit := do
  log "folder_enter_symlink"
  -- fixture dir: [sort, jj → row 2 = link_to_subdir (symlink to dir), enter it
  let output ← run "[jj<ret>" "data/test_folder"
  assert (contains output "file2") "Enter on symlink dir shows its contents"
  let (_, status) := footer output
  assert (contains status "r0/") "Entered symlink dir has rows"

def test_duckdb_list : IO Unit := do
  log "duckdb_list"
  let output ← run "" "data/nu_help.duckdb"
  assert (contains output "commands") "DuckDB file lists tables"
  assert (contains output "params") "DuckDB file lists params table"
  let (_, status) := footer output
  assert (contains status "r0/3") "DuckDB has 3 tables"

def test_duckdb_enter : IO Unit := do
  log "duckdb_enter"
  let output ← run "<ret>" "data/nu_help.duckdb"
  let (_, status) := footer output
  assert (contains status "r0/") "Enter on DuckDB table opens it"
  assert (not (contains status "r0/3")) "Entered table has different row count"

def test_duckdb_primary_key : IO Unit := do
  log "duckdb_primary_key"
  let output ← run "<ret>" "data/nu_help.duckdb"
  let (_, status) := footer output
  assert (contains status "grp=1") "DuckDB primary key is keyed (grp=1)"

def test_folder_prefix : IO Unit := do
  log "folder_prefix"
  let (_, s1) := footer (← run "")
  let (_, s2) := footer (← run ",")
  let r1 := s1.splitOn "r0/" |>.getD 1 "" |>.takeWhile (·.isDigit)
  let r2 := s2.splitOn "r0/" |>.getD 1 "" |>.takeWhile (·.isDigit)
  assert (r2.toNat?.getD 0 >= r1.toNat?.getD 0) ", prefix works in folder"


-- === Navigation tests (parquet) ===

def test_page_down : IO Unit := do
  log "page_down"
  unless (← needsSample) do log "  skip (no sample.parquet)"; return
  let (_, status) := footer (← run "<C-d>" "data/sample.parquet")
  assert (!contains status "r0/") "Ctrl-D pages down past row 0"

def test_page_up : IO Unit := do
  log "page_up"
  unless (← needsSample) do log "  skip (no sample.parquet)"; return
  let (_, status) := footer (← run "<C-d><C-u>" "data/sample.parquet")
  assert (contains status "r0/") "Ctrl-D then Ctrl-U returns to row 0"

def test_page_down_scrolls : IO Unit := do
  log "page_down_scrolls"
  unless (← needsSample) do log "  skip (no sample.parquet)"; return
  let (_, status1) := footer (← run "" "data/sample.parquet")
  let (_, status2) := footer (← run "<C-d>" "data/sample.parquet")
  assert (status1 != status2) "Page down changes status"

def test_last_col_visible : IO Unit := do
  log "last_col_visible"
  unless (← needsSample) do log "  skip (no sample.parquet)"; return
  let first := (dataLines (← run "llllllllllllllllllll" "data/sample.parquet")).headD ""
  let nonWs := first.toList.filter (!·.isWhitespace) |>.length
  assert (nonWs > 0) "Last col shows data"


-- === Sort tests (parquet) ===

def test_parquet_sort_asc : IO Unit := do
  log "parquet_sort_asc"
  unless (← needsSample) do log "  skip (no sample.parquet)"; return
  let first := (dataLines (← run "l[" "data/sample.parquet")).headD ""
  assert (contains first " 18 ") "[ on age sorts asc, age=18"

def test_parquet_sort_desc : IO Unit := do
  log "parquet_sort_desc"
  unless (← needsSample) do log "  skip (no sample.parquet)"; return
  let first := (dataLines (← run "l]" "data/sample.parquet")).headD ""
  assert (contains first " 80 ") "] on age sorts desc, age=80"

-- Sort by non-first column: l moves to val, [ sorts asc → val=1 first
def test_sort_excludes_key : IO Unit := do
  log "sort_excludes_key"
  let first := (dataLines (← run "Il[" "data/grp_sort.csv")).headD ""
  assert (contains first " 1 ") "sort by val: val=1 first"

-- Sort on group column is no-op: ! groups grp, cursor stays on grp, [ → no sort
def test_sort_selected_not_key : IO Unit := do
  log "sort_on_key_noop"
  let lines := dataLines (← run "I![" "data/grp_sort.csv")
  let first := lines.headD ""
  -- within filtered group, original order preserved (val=3 first for A, val=6 for B)
  assert (contains first " 3 " || contains first " 6 ") "sort on key col is no-op"

-- === Meta tests (parquet) ===

def test_parquet_meta : IO Unit := do
  log "parquet_meta"
  unless (← needsSample) do log "  skip (no sample.parquet)"; return
  assert (contains (footer (← run "M" "data/sample.parquet")).1 "meta") "M on parquet shows meta"

-- === Freq tests (parquet) ===

def test_freq_parquet : IO Unit := do
  log "freq_parquet"
  unless (← needsSample) do log "  skip (no sample.parquet)"; return
  assert (contains (footer (← run "F" "data/sample.parquet")).1 "freq") "F on parquet shows freq"

def test_freq_enter_parquet : IO Unit := do
  log "freq_enter_parquet"
  unless (← needsSample) do log "  skip (no sample.parquet)"; return
  let (tab, status) := footer (← run "F<ret>" "data/sample.parquet")
  assert (contains tab "sample.parquet") "F<ret> on parquet pops to parent"
  assert (!contains tab "freq") "F<ret> on parquet exits freq view"
  assert (contains status "r0/") "F<ret> on parquet shows filtered rows"

def test_freq_parquet_key_values : IO Unit := do
  log "freq_parquet_key_values"
  unless (← needsNyse10k) do log "  skip (no nyse10k.parquet)"; return
  let first := (dataLines (← run "lF" "data/nyse10k.parquet")).headD ""
  assert (!first.startsWith " 5180" && !first.startsWith " 2592") "Freq key column shows names, not counts"

def test_freq_total_count : IO Unit := do
  log "freq_total_count"
  unless (← needsNyse) do log "  skip (no nyse)"; return
  let (_, status) := footer (← run "l!l!hF" "data/nyse/1.parquet")
  assert (contains status "/128974") "Freq shows total group count (128974)"

def test_freq_sort_preserves_total : IO Unit := do
  log "freq_sort_total"
  unless (← needsNyse) do log "  skip (no nyse)"; return
  let (_, status) := footer (← run "llFll[" "data/nyse/1.parquet")
  assert (contains status "/") "Freq sort preserves total count in status"

def test_freq_sort_asc_parquet : IO Unit := do
  log "freq_sort_asc"
  unless (← needsNyse) do log "  skip (no nyse)"; return
  let first := (dataLines (← run "llFll[" "data/nyse/1.parquet")).headD ""
  assert (contains first "│") "Freq sort asc shows data"

-- === Meta selection tests (parquet) ===

def test_parquet_meta_0_null_cols : IO Unit := do
  log "parquet_meta_0"
  unless (← needsNyse) do log "  skip (no nyse)"; return
  let (_, status) := footer (← run "M0" "data/nyse/1.parquet")
  assert (contains status "sel=9") "M0 on parquet selects 9 null columns"

def test_parquet_meta_0_enter_groups : IO Unit := do
  log "parquet_meta_0_enter"
  unless (← needsNyse) do log "  skip (no nyse)"; return
  let (tab, status) := footer (← run "M0<ret>" "data/nyse/1.parquet")
  assert (tab.startsWith "[1.parquet]") "M0<ret> returns to parent view"
  assert (contains status "grp=9") "M0<ret> groups 9 null columns"

-- === Filter tests (parquet) ===

def test_filter_parquet_full_db : IO Unit := do
  log "filter_parquet_full_db"
  let (tab, status) := footer (← run "\\" "data/filtered_test.parquet")
  assert (contains tab "\\sym") "\\ filter shows \\sym in tab"
  let countStr := (status.splitOn "r0/" |>.getD 1 "").takeWhile (·.isDigit)
  let count := countStr.toString.toNat?.getD 0
  assert (count > 1000) s!"filter queries full DB ({count} rows, expected 40000 or 60000)"

-- === Scroll fetch tests (parquet) ===

def test_scroll_fetches_more : IO Unit := do
  log "scroll_fetches_more"
  unless (← needsNyse10k) do log "  skip (no nyse10k.parquet)"; return
  let keys := String.join (List.replicate 105 "<C-d>")
  let (_, status) := footer (← run keys "data/nyse10k.parquet")
  let rpart := (status.splitOn " r" |>.getD 1 "").takeWhile (· != ' ')
  let cursor := ((rpart.toString.splitOn "/").headD "").toNat?.getD 0
  assert (cursor > 999) s!"Scroll fetches more: cursor={cursor}, expected > 999"

-- === Misc ===

def test_numeric_right_align : IO Unit := do
  log "numeric_align"
  unless (← needsSample) do log "  skip (no sample.parquet)"; return
  let first := (dataLines (← run "" "data/sample.parquet")).headD ""
  assert (contains first "  ") "Numeric columns right-aligned"

def test_enter_no_quit_parquet : IO Unit := do
  log "enter_no_quit_parquet"
  unless (← needsNyse) do log "  skip (no nyse)"; return
  let (_, status) := footer (← run "<ret>j" "data/nyse/1.parquet")
  assert (contains status "r1/") "Enter on parquet should not quit (j moves to r1)"

-- === SQLite tests ===

-- test_sqlite_list: opening a .sqlite file lists its tables
def test_sqlite_list : IO Unit := do
  log "sqlite_list"
  let output ← run "" "data/test.sqlite"
  assert (contains output "items") "SQLite file lists 'items' table"

-- test_sqlite_enter: entering a SQLite table shows its data
def test_sqlite_enter : IO Unit := do
  log "sqlite_enter"
  let output ← run "<ret>" "data/test.sqlite"
  assert (contains output "alpha") "SQLite table shows 'alpha' row"
  assert (contains output "gamma") "SQLite table shows 'gamma' row"
  let (_, status) := footer output
  assert (contains status "r0/3") "SQLite table has 3 rows"

-- === JSONL tests ===

-- test_jsonl_open: opening a .jsonl file shows its data
def test_jsonl_open : IO Unit := do
  log "jsonl_open"
  let output ← run "" "data/test.jsonl"
  assert (contains output "alpha") "JSONL shows 'alpha' row"
  assert (contains output "beta") "JSONL shows 'beta' row"
  let (_, status) := footer output
  assert (contains status "r0/3") "JSONL has 3 rows"

-- test_jsonl_sort: sorting works on JSONL data
def test_jsonl_sort : IO Unit := do
  log "jsonl_sort"
  let output ← run "[" "data/test.jsonl"
  let first := (dataLines output).headD ""
  assert (contains first "alpha") "JSONL sort asc: first row is alpha"

-- === Arrow/Feather tests ===

-- test_arrow_open: opening a .arrow file shows its data
def test_arrow_open : IO Unit := do
  log "arrow_open"
  let output ← run "" "data/test.arrow"
  assert (contains output "alpha") "Arrow shows 'alpha' row"
  assert (contains output "gamma") "Arrow shows 'gamma' row"
  let (_, status) := footer output
  assert (contains status "r0/3") "Arrow has 3 rows"

-- test_feather_open: opening a .feather file shows its data
def test_feather_open : IO Unit := do
  log "feather_open"
  let output ← run "" "data/test.feather"
  assert (contains output "alpha") "Feather shows 'alpha' row"
  assert (contains output "beta") "Feather shows 'beta' row"
  let (_, status) := footer output
  assert (contains status "r0/3") "Feather has 3 rows"

-- === Excel tests ===

-- test_xlsx_open: opening a .xlsx file shows its data
def test_xlsx_open : IO Unit := do
  log "xlsx_open"
  let output ← run "" "data/test.xlsx"
  assert (contains output "alpha") "Excel shows 'alpha' value"
  assert (contains output "gamma") "Excel shows 'gamma' value"
  let (_, status) := footer output
  assert (contains status "r0/3") "Excel has 3 rows"

-- === Avro tests ===

-- test_avro_open: opening a .avro file shows its data
def test_avro_open : IO Unit := do
  log "avro_open"
  let output ← run "" "data/test.avro"
  assert (contains output "alpha") "Avro shows 'alpha' row"
  assert (contains output "gamma") "Avro shows 'gamma' row"
  let (_, status) := footer output
  assert (contains status "r0/3") "Avro has 3 rows"

-- === Osquery tests ===

def hasOsquery : IO Bool := do
  let r ← IO.Process.output { cmd := "which", args := #["osqueryi"] }
  pure (r.exitCode == 0)

def test_osquery_list : IO Unit := do
  log "osquery_list"
  unless (← hasOsquery) do log "  skip (no osqueryi)"; return
  let output ← run "" "osquery://"
  assert (contains output "name") "osquery:// shows name column"
  assert (contains output "safety") "osquery:// shows safety column"

def test_osquery_enter : IO Unit := do
  log "osquery_enter"
  unless (← hasOsquery) do log "  skip (no osqueryi)"; return
  let output ← run "<ret>" "osquery://"
  let (tab, _) := footer output
  assert (contains tab "acpi_tables") "Enter on safe table opens it"

def test_osquery_scroll_no_hide : IO Unit := do
  log "osquery_scroll_no_hide"
  unless (← hasOsquery) do log "  skip (no osqueryi)"; return
  -- name column (keyed) should stay visible when scrolling right
  let output0 ← run "" "osquery://"
  assert (contains output0 "name") "col 0: name visible"
  let output1 ← run "l" "osquery://"
  assert (contains output1 "name") "col 1: name still visible after moving right"

def test_osquery_back : IO Unit := do
  log "osquery_back"
  unless (← hasOsquery) do log "  skip (no osqueryi)"; return
  let output ← run "<ret>q" "osquery://"
  assert (contains output "name") "q pops back to osquery table list"

def test_osquery_meta_description : IO Unit := do
  log "osquery_meta_description"
  unless (← hasOsquery) do log "  skip (no osqueryi)"; return
  -- Enter first safe table, then press M for meta view
  let output ← run "<ret>M" "osquery://"
  assert (contains output "description") "Meta view on osquery table shows description column"

def test_osquery_direct_table : IO Unit := do
  log "osquery_direct_table"
  unless (← hasOsquery) do log "  skip (no osqueryi)"; return
  let output ← run "" "osquery://groups"
  -- Should open groups table directly, not the listing
  assert (contains output "gid") "osquery://groups shows gid column"
  assert (!(contains output "safety")) "osquery://groups is not the listing"

-- | Verify osquery columns are typed (gid is numeric, not VARCHAR).
--   osquery JSON quotes all values as strings; enter_types_sql casts them
--   using osq schema views. Numeric columns show '#' type indicator.
def test_osquery_typed_columns : IO Unit := do
  log "osquery_typed_columns"
  unless (← hasOsquery) do log "  skip (no osqueryi)"; return
  -- Direct table: gid should be numeric (#) not string (no indicator)
  let output ← run "" "osquery://groups"
  assert (contains output "gid") "osquery://groups has gid column"
  assert (contains output "#") "osquery://groups gid is numeric (# indicator)"
  -- Via folder enter: first safe table should open
  let output2 ← run "<ret>" "osquery://"
  assert (!(contains output2 "safety")) "osquery enter table is not listing"

def test_osquery_sort_enter : IO Unit := do
  log "osquery_sort_enter"
  unless (← hasOsquery) do log "  skip (no osqueryi)"; return
  -- Sort by rows (col 3) desc, then enter top row → should open a table, not crash
  let output ← run "lll]<ret>" "osquery://"
  let (tab, _) := footer output
  -- After enter, tab bar should show both osquery listing and the opened table
  assert (contains tab "osquery://") "sort+enter: osquery tab still visible"
  assert (!(contains output "safety")) "sort+enter: opened table, not listing"

def test_last_col_no_stretch : IO Unit := do
  log "last_col_no_stretch"
  let output ← run "" "data/basic.csv"
  let hdr := header output
  -- basic.csv has 2 narrow columns (a, b); header should be short, not padded to 80
  assert (hdr.length < 30) s!"last col should not stretch to 80: got {hdr.length} chars"
  -- trailing separator marks the end of the table
  assert (contains hdr "│") "last col should have trailing separator"

def test_width_grows_on_scroll : IO Unit := do
  log "width_grows_on_scroll"
  -- wide_scroll.csv: first 22 rows have status=ok, last 5 have status=input-required
  -- Scroll down with many j presses so "input-required" rows are visible
  let keys := String.mk (List.replicate 26 'j')
  let output ← run keys "data/wide_scroll.csv"
  let lines := dataLines output
  assert (lines.any (contains · "input-required")) "scrolled data should show full 'input-required'"

-- === HF tests ===

-- | Check if HuggingFace API is reachable (cached to avoid repeated 3s timeouts)
initialize hfAccessCache : IO.Ref (Option Bool) ← IO.mkRef none

def hasHfAccess : IO Bool := do
  match ← hfAccessCache.get with
  | some v => pure v
  | none =>
    let r ← IO.Process.output { cmd := "curl", args := #["-sf", "--max-time", "3", "https://huggingface.co/api/datasets/openai/gsm8k"] }
    let ok := r.exitCode == 0
    hfAccessCache.set (some ok)
    pure ok

def test_hf_readme : IO Unit := do
  log "hf_readme"
  unless (← hasHfAccess) do log "  skip (no HF access)"; return
  -- Enter README.md from openai/gsm8k (row 5: .., main, socratic, .gitattributes, README.md)
  let output ← run "jjjjj<ret>" "hf://datasets/openai/gsm8k"
  assert (contains output "GSM8K") "HF README shows dataset name"
  assert (contains output "math" || contains output "arithmetic" || contains output "word problems")
    "HF README shows description content"

def test_hf_enter_parquet : IO Unit := do
  log "hf_enter_parquet"
  unless (← hasHfAccess) do log "  skip (no HF access)"; return
  -- Enter main/ dir then first parquet file
  let output ← run "jj<ret>j<ret>" "hf://datasets/openai/gsm8k"
  assert (contains output "question") "HF parquet has question column"
  assert (contains output "answer") "HF parquet has answer column"

-- === Script mode (-p) tests ===

-- | -p with implicit from: filter rows where a > 2
def test_script_filter : IO Unit := do
  log "script_filter"
  let out ← IO.Process.output { cmd := bin, args := #["data/basic.csv", "-p", "filter a > 2"] }
  assert (out.exitCode == 0) s!"script_filter exit code: {out.exitCode}"
  assert (contains out.stdout "3") "script_filter: row a=3 present"
  assert (contains out.stdout "5") "script_filter: row a=5 present"
  assert (!contains out.stdout "\n1\t") "script_filter: row a=1 excluded"

-- | -p join: join two CSV files on shared column
def test_script_join : IO Unit := do
  log "script_join"
  IO.FS.writeFile "/tmp/tc_test/left.csv" "k,val\n1,a\n2,b\n3,c\n"
  IO.FS.writeFile "/tmp/tc_test/right.csv" "k,score\n1,10\n3,30\n"
  let prql := "from x | join (from `/tmp/tc_test/right.csv`) (==k)"
  let out ← IO.Process.output { cmd := bin, args := #["/tmp/tc_test/left.csv", "-p", prql] }
  assert (out.exitCode == 0) s!"script_join exit code: {out.exitCode}"
  assert (contains out.stdout "10") "script_join: score=10 present"
  assert (contains out.stdout "30") "script_join: score=30 present"
  assert (!contains out.stdout "\tb\t") "script_join: val=b excluded (k=2 not in right)"

-- | -p append: union two files
def test_script_append : IO Unit := do
  log "script_append"
  IO.FS.writeFile "/tmp/tc_test/a1.csv" "x\n1\n2\n"
  IO.FS.writeFile "/tmp/tc_test/a2.csv" "x\n3\n4\n"
  let prql := "from x | append (from `/tmp/tc_test/a2.csv`)"
  let out ← IO.Process.output { cmd := bin, args := #["/tmp/tc_test/a1.csv", "-p", prql] }
  assert (out.exitCode == 0) s!"script_append exit code: {out.exitCode}"
  let lines := out.stdout.splitOn "\n" |>.filter (· != "")
  -- header + 4 data rows (2 from each file)
  assert (lines.length == 5) s!"script_append: expected 5 lines, got {lines.length}"

-- | -p with explicit from: verify full PRQL passthrough
def test_script_from : IO Unit := do
  log "script_from"
  let out ← IO.Process.output { cmd := bin, args := #["data/basic.csv", "-p", "from `data/basic.csv` | take 2"] }
  assert (out.exitCode == 0) s!"script_from exit code: {out.exitCode}"
  let lines := out.stdout.splitOn "\n" |>.filter (· != "")
  -- header + 2 data rows
  assert (lines.length == 3) s!"script_from: expected 3 lines, got {lines.length}"
  assert (contains out.stdout "a\tb") "script_from: header present"

-- === Run all tests ===

-- | All tests as (name, action) pairs
def tests : Array (String × IO Unit) := #[
  -- CSV tests (nav/key/hide/select/stack/info/quit moved to TestScreen.lean)
  ("width_stable_after_info", test_width_stable_after_info),
  ("sort_asc", test_sort_asc), ("sort_desc", test_sort_desc),
  ("meta_shows", test_meta_shows), ("meta_col_info", test_meta_col_info),
  ("meta_no_garbage", test_meta_no_garbage),
  ("freq_shows", test_freq_shows), ("freq_after_meta", test_freq_after_meta),
  ("freq_by_key", test_freq_by_key), ("freq_multi_key", test_freq_multi_key),
  ("freq_keeps_grp", test_freq_keeps_grp),
  ("prec_inc", test_prec_inc), ("prec_dec", test_prec_dec),
  ("meta_0", test_meta_0), ("meta_1", test_meta_1),
  ("meta_0_enter", test_meta_0_enter), ("meta_1_enter", test_meta_1_enter),
  ("freq_enter", test_freq_enter),
  ("spaced_header", test_spaced_header),
  ("no_stderr", test_no_stderr),
  ("search_jump", test_search_jump), ("search_next", test_search_next),
  ("search_prev", test_search_prev), ("search_after_sort", test_search_after_sort),
  ("col_search", test_col_search),
  ("folder_no_args", test_folder_no_args), ("folder_D", test_folder_D),
  ("folder_tab", test_folder_tab), ("folder_enter", test_folder_enter),
  ("folder_relative", test_folder_relative), ("folder_pop", test_folder_pop),
  ("folder_enter_symlink", test_folder_enter_symlink),
  ("duckdb_list", test_duckdb_list), ("duckdb_enter", test_duckdb_enter),
  ("duckdb_primary_key", test_duckdb_primary_key),
  ("sqlite_list", test_sqlite_list), ("sqlite_enter", test_sqlite_enter),
  ("jsonl_open", test_jsonl_open), ("jsonl_sort", test_jsonl_sort),
  ("arrow_open", test_arrow_open), ("feather_open", test_feather_open),
  ("xlsx_open", test_xlsx_open), ("avro_open", test_avro_open),
  ("folder_prefix", test_folder_prefix),
  -- Parquet tests
  ("page_down", test_page_down), ("page_up", test_page_up),
  ("page_down_scrolls", test_page_down_scrolls),
  ("last_col_visible", test_last_col_visible),
  ("parquet_sort_asc", test_parquet_sort_asc),
  ("parquet_sort_desc", test_parquet_sort_desc),
  ("sort_excludes_key", test_sort_excludes_key),
  ("sort_selected_not_key", test_sort_selected_not_key),
  ("parquet_meta", test_parquet_meta),
  ("freq_parquet", test_freq_parquet),
  ("freq_enter_parquet", test_freq_enter_parquet),
  ("freq_parquet_key_values", test_freq_parquet_key_values),
  ("freq_total_count", test_freq_total_count),
  ("freq_sort_preserves_total", test_freq_sort_preserves_total),
  ("freq_sort_asc_parquet", test_freq_sort_asc_parquet),
  ("parquet_meta_0_null_cols", test_parquet_meta_0_null_cols),
  ("parquet_meta_0_enter_groups", test_parquet_meta_0_enter_groups),
  ("filter_parquet_full_db", test_filter_parquet_full_db),
  ("scroll_fetches_more", test_scroll_fetches_more),
  ("numeric_right_align", test_numeric_right_align),
  ("enter_no_quit_parquet", test_enter_no_quit_parquet),
  -- Osquery tests
  ("osquery_list", test_osquery_list), ("osquery_enter", test_osquery_enter),
  ("osquery_scroll_no_hide", test_osquery_scroll_no_hide),
  ("osquery_back", test_osquery_back),
  ("osquery_meta_description", test_osquery_meta_description),
  ("osquery_direct_table", test_osquery_direct_table),
  ("osquery_typed_columns", test_osquery_typed_columns),
  ("osquery_sort_enter", test_osquery_sort_enter),
  -- HF tests
  ("hf_readme", test_hf_readme),
  ("hf_enter_parquet", test_hf_enter_parquet),
  -- Rendering tests
  ("last_col_no_stretch", test_last_col_no_stretch),
  ("width_grows_on_scroll", test_width_grows_on_scroll),
  -- Script mode (-p) tests
  ("script_filter", test_script_filter),
  ("script_join", test_script_join),
  ("script_append", test_script_append),
  ("script_from", test_script_from)
]

def main (args : List String) : IO Unit := do
  IO.FS.writeFile "test.log" ""
  IO.FS.createDirAll "/tmp/tc_test"
  let ok ← Tc.AdbcTable.init
  if !ok then throw (IO.userError "Backend init failed")
  try Tc.SourceConfig.attachDb catch _ => pure ()
  let filter := args.head?
  let selected := match filter with
    | none => tests
    | some f => tests.filter fun (name, _) => (name.splitOn f).length > 1
  if selected.isEmpty then
    IO.eprintln s!"No tests matching '{filter.getD ""}'"
    return
  IO.println s!"Running {selected.size} test(s)...\n"
  for (_, action) in selected do action
  Tc.AdbcTable.shutdown
  IO.println "\nAll tests passed!"

end Test

def main (args : List String) : IO Unit := Test.main args
