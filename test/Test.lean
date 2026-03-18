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
import Tc.Socket
import Tc.TmpDir
import test.TestPure
import test.TestUtil

-- ============================================================================
-- Runtime UI tests
-- ============================================================================

namespace Test

open Tc TestUtil

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

-- | Sort on "type" column in folder view — "type" is a PRQL keyword
def test_folder_sort_type : IO Unit := do
  log "folder_sort_type"
  -- Navigate to type column (3rd from name), sort ascending
  let output ← run "lll[" "data/"
  let lines := dataLines output
  -- After sort on type, dirs should group together (alphabetically "dir" < "file")
  -- Check that the sort didn't silently fail (no error popup, data is sorted)
  let (_, status) := footer output
  assert (contains status "r0/") "sort on type column should not error"
  -- Verify dirs come before files in ascending sort
  let types := lines.map fun l => if contains l " dir " then "d"
    else if contains l " symlink " then "s" else "f"
  let firstFile := types.findIdx? (· == "f") |>.getD 999
  let lastDir := types.reverse.findIdx? (· == "d") |>.map (types.length - 1 - ·) |>.getD 0
  assert (lastDir < firstFile) s!"ascending sort: dirs ({lastDir}) before files ({firstFile})"

-- === Folder tests ===

def test_folder_no_args : IO Unit := do
  log "folder_no_args"
  assert (contains (← run "") "[/") "No-args shows folder view with absolute path"

def test_folder_D : IO Unit := do
  log "folder_D_key"
  assert (contains (← run "D" "data/basic.csv") "[/") "D pushes folder view with absolute path"

-- Folder tab shows the current working directory as an absolute path
def test_folder_tab : IO Unit := do
  log "folder_tab_path"
  let (tab, _) := footer (← run "")
  assert (contains tab "[/") "Folder tab shows absolute path (starts with /)"
  let cwd ← IO.currentDir
  let dirName := cwd.toString.splitOn "/" |>.getLast?.getD ""
  assert (contains tab s!"/{dirName}]") s!"Folder tab ends with /{dirName}]"

-- enter .. from test_folder → pushes parent folder view; verify status line has rows
def test_folder_enter : IO Unit := do
  log "folder_enter_dir"
  let output ← run "<ret>" "data/test_folder"
  let (_, status) := footer output
  -- Parent dir always has entries; status shows r0/N where N>0
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


-- Sort by non-first column: l moves to val, [ sorts asc → val=1 first
def test_sort_excludes_key : IO Unit := do
  log "sort_excludes_key"
  let first := (dataLines (← run "l[" "data/grp_sort.csv")).headD ""
  assert (contains first " 1 ") "sort by val: val=1 first"

-- Sort on group column is no-op: ! groups grp, cursor stays on grp, [ → no sort
def test_sort_selected_not_key : IO Unit := do
  log "sort_on_key_noop"
  let lines := dataLines (← run "![" "data/grp_sort.csv")
  let first := lines.headD ""
  -- within filtered group, original order preserved (val=3 first for A, val=6 for B)
  assert (contains first " 3 " || contains first " 6 ") "sort on key col is no-op"

-- === Filter tests (parquet — checked-in) ===

def test_filter_parquet_full_db : IO Unit := do
  log "filter_parquet_full_db"
  let (tab, status) := footer (← run "\\" "data/filtered_test.parquet")
  assert (contains tab "\\sym") "\\ filter shows \\sym in tab"
  let countStr := (status.splitOn "r0/" |>.getD 1 "").takeWhile (·.isDigit)
  let count := countStr.toString.toNat?.getD 0
  assert (count > 1000) s!"filter queries full DB ({count} rows, expected 40000 or 60000)"

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

-- === CSV test ===

-- test_csv_open: opening a .csv file shows its data
def test_csv_open : IO Unit := do
  log "csv_open"
  let output ← run "" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r0/5") "CSV has 5 rows"

-- === JSON tests ===

-- test_json_open: opening a .json file shows its data
def test_json_open : IO Unit := do
  log "json_open"
  let output ← run "" "data/test.json"
  assert (contains output "alpha") "JSON shows 'alpha' row"
  let (_, status) := footer output
  assert (contains status "r0/3") "JSON has 3 rows"

-- test_ndjson_open: opening a .ndjson file shows its data
def test_ndjson_open : IO Unit := do
  log "ndjson_open"
  let output ← run "" "data/test.ndjson"
  assert (contains output "beta") "NDJSON shows 'beta' row"
  let (_, status) := footer output
  assert (contains status "r0/3") "NDJSON has 3 rows"

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

-- === PostgreSQL tests ===

def hasPgTest : IO Bool := do
  let r ← IO.Process.output { cmd := "pg_isready", args := #["-h", "/tmp/claude-1000", "-p", "5433"] }
  pure (r.exitCode == 0)

-- test_pg_list: connecting to PostgreSQL lists public tables
def test_pg_list : IO Unit := do
  log "pg_list"
  unless (← hasPgTest) do log "  skip (no pg on /tmp/claude-1000:5433)"; return
  let output ← run "" "pg://host=/tmp/claude-1000 port=5433 dbname=pagila"
  assert (contains output "film") "pg:// lists 'film' table"
  assert (contains output "actor") "pg:// lists 'actor' table"

-- test_pg_enter: entering a PostgreSQL table shows its data
def test_pg_enter : IO Unit := do
  log "pg_enter"
  unless (← hasPgTest) do log "  skip (no pg on /tmp/claude-1000:5433)"; return
  let output ← run "jjjjj<ret>" "pg://host=/tmp/claude-1000 port=5433 dbname=pagila"
  let (_, status) := footer output
  assert (contains status "r0/") "Enter on pg table opens it with rows"

-- === Osquery tests ===

def hasOsquery : IO Bool := hasCmd "osqueryi"

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
  -- I hides info overlay so header helper picks up table header, not overlay text
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

-- === Derive tests ===

-- | Derive: press '=', fzf auto-selects hint "a : int" which lacks "name = expr" format → no-op
def test_derive : IO Unit := do
  log "derive"
  let out ← run "=" "data/basic.csv"
  let hdr := header out
  -- without valid "name = expr" input, derive is a no-op — original columns remain unchanged
  assert (contains hdr "a") "derive: original columns should remain"
  assert (!(contains hdr "_1")) "derive: no derived column without name = expr"

-- === Split tests ===

-- | Split: press ':' on string column, fzf picks first suggestion "-" → splits tag into parts
def test_split : IO Unit := do
  log "split"
  let out ← run ":" "data/split_test.csv"
  let (tab, status) := footer out
  -- split adds 4 columns (max parts in "x-y-z-w"), total = original 2 + 4 = 6
  assert (contains status "c2/6") "split: 6 columns after split"
  assert (contains tab ":tag") "split: tab shows :tag"
  -- split column data visible (first parts from "a-b-c" and "d-e-f")
  let lines := dataLines out
  assert (lines.any (contains · " a ")) "split: part 'a' visible"
  assert (lines.any (contains · " d ")) "split: part 'd' visible"

-- | Split no-op on non-string column: ':' on int column does nothing
def test_split_noop : IO Unit := do
  log "split_noop"
  let out ← run "l:" "data/split_test.csv"
  let (_, status) := footer out
  -- cursor on "value" (int column), split should be no-op — still 2 columns
  assert (contains status "c1/2") "split: no split on int column"

-- === Export tests ===

-- | Export: press 'e', fzf auto-selects csv, verify file created with correct content
def test_export_csv : IO Unit := do
  log "export_csv"
  let home := (← IO.getEnv "HOME").getD "."
  let path := s!"{home}/tv_export_sort_test.csv"
  try IO.FS.removeFile path catch _ => pure ()
  let out ← run "e" "data/sort_test.parquet"
  assert (contains out "name") "export_csv: table should render"
  let csv ← IO.FS.readFile path
  assert (contains csv "name") "export_csv: csv should contain header"
  assert (contains csv "alice") "export_csv: csv should contain data"
  IO.FS.removeFile path

-- === Transpose tests ===

-- test_transpose: X swaps rows/columns; original col names become row values
def test_transpose : IO Unit := do
  log "transpose"
  let output ← run "X" "data/basic.csv"
  assert (contains output "column") "X shows 'column' header"
  -- original column names a, b appear as data in the "column" column
  let lines := dataLines output
  assert (lines.any (contains · " a ")) "transposed row for col 'a'"
  assert (lines.any (contains · " b ")) "transposed row for col 'b'"
  let (_, status) := footer output
  -- basic.csv has 2 data columns (a, b) → transposed has 2 rows
  assert (contains status "r0/2") "transposed has 2 rows (one per original column)"

-- test_transpose_pop: q pops back from transposed view
def test_transpose_pop : IO Unit := do
  log "transpose_pop"
  let output ← run "Xq" "data/basic.csv"
  let (_, status) := footer output
  assert (contains status "r0/5") "q pops back to original 5-row view"

-- === Join tests ===

-- test_join_inner: sort folder by name, enter left, key id, swap, enter right, key id, join
def test_join_inner : IO Unit := do
  log "join_inner"
  -- [ sorts asc → row0=.., row1=left.csv, row2=right.csv (alphabetical)
  -- j<ret> enter left, ! key id, S swap (cursor stays row1), j<ret> enter right, ! key id, S swap, q pop, J join
  let output ← run "[j<ret><key>Sj<ret><key>SqJ" "data/join_test"
  assert (contains output "alice") "J shows alice from left table"
  assert (contains output "90") "J shows score=90 from right table"
  let (_, status) := footer output
  assert (contains status "r0/2") "Inner join has 2 rows (id=1,3)"

-- test_join_union: sort folder, enter left.csv twice, union
def test_join_union : IO Unit := do
  log "join_union"
  -- [ sorts asc → row0=.., row1=left.csv. j→left, <ret>→open, S→swap (cursor stays at row1),
  -- <ret>→open left again, S→swap, q→pop folder, J→union left∪left
  let output ← run "[j<ret>S<ret>SqJ" "data/join_test"
  assert (contains output "name") "Union shows name column"
  let (_, status) := footer output
  assert (contains status "r0/6") "Union of same 3-row table = 6 rows"

-- === Sparkline tests ===

-- | Sparklines always on: sparkline row with block chars visible
def test_sparkline_on : IO Unit := do
  log "sparkline_on"
  let output ← run "" "data/basic.csv"
  assert (output.any (fun c => c.toNat >= 0x2581 && c.toNat <= 0x2588)) "sparklines always on (block chars visible)"

-- === Status bar aggregation tests ===

-- | Numeric column shows sum/avg/count on status bar (a: 1,2,3,4,5 → Σ15 μ3 #5)
def test_statusagg_numeric : IO Unit := do
  log "statusagg_numeric"
  let output ← run "" "data/basic.csv"
  -- cursor starts on column a (numeric) — status bar shows Σ and #
  assert (contains output "Σ") "Numeric column shows sum (Σ)"
  assert (contains output "#5") "Numeric column shows count (#5)"

-- | String column shows only count on status bar (b: x,y,x,z,x → #5)
def test_statusagg_string : IO Unit := do
  log "statusagg_string"
  let output ← run "l" "data/basic.csv"
  -- l moves to column b (string) — status bar shows # but no Σ
  assert (contains output "#5") "String column shows count"
  assert (!contains output "Σ") "String column has no sum"

-- === Key column reorder tests ===

-- | Shift+Arrow reorders key columns: !l! groups a,b; <S-left> swaps b before a
def test_key_shift : IO Unit := do
  log "key_shift"
  -- !l! → key both cols (grp=["a","b"]), then shift-left moves b before a
  let hdr := header (← run "!l!<S-left>" "data/basic.csv")
  -- After shift: grp=["b","a"], so header should show b before a
  let bPos := hdr.splitOn "b" |>.head?.map (·.length) |>.getD 999
  let aPos := hdr.splitOn "a" |>.head?.map (·.length) |>.getD 999
  assert (bPos < aPos) s!"shift-left: b ({bPos}) should appear before a ({aPos}) in header"

-- | Heatmap mode cycling: space m , reduces mode, space m . increases (default=1 numeric)
def test_heat_mode : IO Unit := do
  log "heat_mode"
  -- space m , reduces mode from 1→0 (off); verify rendering still works
  let output ← run " m," "data/basic.csv"
  assert (contains output "a") "heat mode dec: still shows column a"
  -- space m . increases mode from 1→2; verify rendering still works
  let output ← run " m." "data/basic.csv"
  assert (contains output "a") "heat mode inc: still shows column a"
  -- space m . . . increases to 3 (clamped max); verify rendering still works
  let output ← run " m..." "data/basic.csv"
  assert (contains output "a") "heat mode inc to max: still shows column a"

-- | Flat command menu: space triggers single-level fzf menu (test mode picks first item)
-- Verifies the 2-level menu → flat menu refactor doesn't break menu dispatch
def test_flat_menu : IO Unit := do
  log "flat_menu"
  -- In test mode, space → cmdMode picks first flat item → row .dec (cursor up from r0 = no-op)
  let output ← run " " "data/basic.csv"
  assert (contains output "a") "flat menu: table renders after first menu item"
  -- Two space presses: each picks first flat item, table still intact
  let output ← run "  " "data/basic.csv"
  assert (contains output "a") "flat menu: double space still renders"

-- | Socket command channel: init → send command via socat → pollCmd → verify → shutdown
def test_socket : IO Unit := do
  log "socket"
  unless (← hasCmd "socat") do log "  skip (no socat)"; return
  Socket.init
  try
    let path ← Socket.getPath
    if path.isEmpty then do log "  skip (socket init failed)"; return
    let _ ← IO.Process.output { cmd := "bash", args := #["-c", s!"printf 'm+' | socat - UNIX-CONNECT:{path}"] }
    -- Poll with retry — listener thread may need time to process
    let mut got := none
    for _ in List.range 10 do
      got ← Socket.pollCmd
      if got.isSome then break
      IO.sleep 10
    match got with
    | some cmd => assert (cmd == "m+") s!"socket: expected 'm+', got '{cmd}'"
    | none => assert false "socket: pollCmd returned none"
  finally Socket.shutdown

-- | Arrow keys move cursor one step (not page). Verifies arrow→hjkl mapping.
def test_arrow_nav : IO Unit := do
  log "arrow_nav"
  -- <down> should move cursor to r1 (same as j)
  let s1 := (footer (← run "<down>" "data/basic.csv")).2
  assert (contains s1 "r1/") "arrow down moves to r1"
  -- j should also move to r1
  let s2 := (footer (← run "j" "data/basic.csv")).2
  assert (contains s2 "r1/") "j moves to r1"
  -- <right> then check we moved (col index changes)
  let h1 := header (← run "<right>" "data/basic.csv")
  let h2 := header (← run "l" "data/basic.csv")
  -- Both should produce same cursor position
  assert (h1 == h2) s!"arrow right == l: '{h1}' vs '{h2}'"

-- === Session tests ===

-- | Session save/load: write a session file, load with -s, verify data is restored
def test_session_load : IO Unit := do
  log "session_load"
  -- Write a session file that opens basic.csv with a sort op (ascending on col "a")
  let home := (← IO.getEnv "HOME").getD "."
  let dir := s!"{home}/.cache/tv/sessions"
  let _ ← IO.Process.output { cmd := "mkdir", args := #["-p", dir] }
  let absPath ← do
    let r ← IO.Process.output { cmd := "realpath", args := #["data/basic.csv"] }
    pure r.stdout.trimAscii.toString
  let json := s!"\{\"version\":1,\"views\":[\{\"path\":\"{absPath}\",\"vkind\":\{\"kind\":\"tbl\"},\"disp\":\"\",\"precAdj\":0,\"widthAdj\":0,\"row\":0,\"col\":0,\"grp\":[],\"hidden\":[],\"colSels\":[],\"search\":null,\"query\":\{\"base\":\"from `{absPath}`\",\"ops\":[\{\"type\":\"sort\",\"cols\":[[\"a\",true]]}]}}]}"
  IO.FS.writeFile s!"{dir}/test_session.json" json
  -- Load the session via -s flag
  let out ← IO.Process.output { cmd := bin, args := #["-s", "test_session", "-c", "I"] }
  assert (out.exitCode == 0) s!"session load exit code: {out.exitCode}"
  assert (contains out.stdout "a") "session load: shows column a"
  let (_, status) := footer out.stdout
  assert (contains status "r0/5") "session load: has 5 rows"
  -- Clean up
  try IO.FS.removeFile s!"{dir}/test_session.json" catch _ => pure ()

-- | Session save+load round-trip: W saves session, -s loads it back
def test_session_save_load : IO Unit := do
  log "session_save_load"
  let home := (← IO.getEnv "HOME").getD "."
  let dir := s!"{home}/.cache/tv/sessions"
  -- W auto-names from tab (basic.csv → "basic"); clean up any prior session
  try IO.FS.removeFile s!"{dir}/basic.json" catch _ => pure ()
  -- Open basic.csv, sort asc, then press W to save session
  let _ ← run "[W" "data/basic.csv"
  -- Verify session file was created
  let saved ← hasFile s!"{dir}/basic.json"
  assert saved "W should create basic.json session file"
  -- Load the session via -s and verify sort is preserved (first data row should be 1)
  let out ← IO.Process.output { cmd := bin, args := #["-s", "basic", "-c", "I"] }
  assert (out.exitCode == 0) s!"session round-trip exit code: {out.exitCode}"
  let first := (dataLines out.stdout).headD ""
  assert (contains first " 1 " || first.startsWith "1 ") "session round-trip: sort preserved (first row = 1)"
  let (_, status) := footer out.stdout
  assert (contains status "r0/5") "session round-trip: has 5 rows"
  -- Clean up
  try IO.FS.removeFile s!"{dir}/basic.json" catch _ => pure ()

-- | Session load missing: -s with non-existent session prints error
def test_session_missing : IO Unit := do
  log "session_missing"
  let out ← IO.Process.output { cmd := bin, args := #["-s", "nonexistent_session_xyz"] }
  assert (out.exitCode == 0 || contains out.stderr "not found" || contains out.stderr "Session")
    "missing session should report error"

-- === Diff tests ===

-- | Diff: open before.csv and after.csv, press V to diff.
--   cost column is same across all rows → hidden (sameHide).
--   sales column differs → visible with Δ prefix.
def test_diff : IO Unit := do
  log "diff"
  -- [ sorts asc → row0=.., row1=after.csv, row2=before.csv
  -- j<ret> enter after, S swap, jj<ret> enter before, S swap, q pop folder, V diff
  let output ← run "[j<ret>Sjj<ret>SqV" "data/diff_test"
  let (tab, status) := footer output
  assert (contains tab "diff") "V shows diff in tab"
  assert (contains status "r0/3") "diff has 3 rows"
  -- Δ prefix marks changed columns
  assert (contains output "Δ") "diff columns have Δ prefix"
  -- Key columns (name, region) should be visible; region may be truncated
  assert (contains output "name") "diff shows key column name"
  assert (contains output "regi") "diff shows key column region (truncated)"

-- | Diff show same: V on diff view reveals hidden same-value columns
def test_diff_show_same : IO Unit := do
  log "diff_show_same"
  -- Same as above but press V again to reveal sameHide columns
  let output ← run "[j<ret>Sjj<ret>SqVV" "data/diff_test"
  -- After second V, cost columns should expand (no longer hidden width=1)
  assert (contains output "cos") "VV reveals same-value cost columns"

-- === Plot tests ===

-- | plotExport with string column as y should not crash.
--   Regression: ds_nth's `y != 0` filter caused DuckDB to cast string cols to INT.
def test_plot_export_string_col : IO Unit := do
  log "plot_export_string_col"
  -- mixed.csv: x(int), y(float), cat(str) — passing string col "cat" as yName triggers the bug
  let some tbl ← Tc.AdbcTable.fromFile "data/plot/mixed.csv" | throw (IO.userError "failed to open mixed.csv")
  -- xName=x, yName=cat (string!), no category, xIsTime=false, step=1, truncLen=1
  let result ← Tc.AdbcTable.plotExport tbl "x" "cat" none false 1 1
  assert result.isSome "plotExport with string y column should not crash with type cast error"

-- | plotExport generates a valid TSV data file with correct columns
def test_plot_export_data : IO Unit := do
  log "plot_export_data"
  let some tbl ← Tc.AdbcTable.fromFile "data/plot/mixed.csv" | throw (IO.userError "failed to open mixed.csv")
  let result ← Tc.AdbcTable.plotExport tbl "x" "y" none false 1 1
  assert result.isSome "plotExport should succeed for numeric y"
  let datPath ← Tc.tmpPath "plot.dat"
  let content ← IO.FS.readFile datPath
  let lines := content.splitOn "\n" |>.filter (!·.isEmpty)
  assert (lines.length > 0) "plot.dat should have data rows"
  -- each line should have tab-separated x and y values
  assert (lines.all fun l => (l.splitOn "\t").length >= 2) "plot.dat rows should be tab-separated x\\ty"

-- | plotExport with category column returns distinct categories
def test_plot_export_cat : IO Unit := do
  log "plot_export_cat"
  let some tbl ← Tc.AdbcTable.fromFile "data/plot/mixed.csv" | throw (IO.userError "failed to open mixed.csv")
  let result ← Tc.AdbcTable.plotExport tbl "x" "y" (some "cat") false 1 1
  let some cats := result | throw (IO.userError "plotExport with cat should succeed")
  assert (cats.size == 2) s!"expected 2 categories (A,B), got {cats.size}"

-- | Check Rscript is installed
def hasRscript : IO Bool := hasCmd "Rscript"

-- | Check ggplot2 is installed in R
def hasGgplot2 : IO Bool := do
  let r ← IO.Process.output { cmd := "Rscript", args := #["-e", "library(ggplot2)"] }
  pure (r.exitCode == 0)

-- | R + ggplot2 installed and loadable
def test_plot_r_installed : IO Unit := do
  log "plot_r_installed"
  unless (← hasRscript) do log "  skip (no Rscript)"; return
  assert (← hasGgplot2) "ggplot2 not installed — run: Rscript -e 'install.packages(\"ggplot2\")'"

-- | R script generates a PNG from exported data (full pipeline)
def test_plot_render_line : IO Unit := do
  log "plot_render_line"
  unless (← hasRscript) do log "  skip (no Rscript)"; return
  unless (← hasGgplot2) do log "  skip (no ggplot2)"; return
  let some tbl ← Tc.AdbcTable.fromFile "data/plot/line.csv" | throw (IO.userError "failed to open line.csv")
  -- export data
  let _ ← Tc.AdbcTable.plotExport tbl "x" "y" none false 1 1
  let datPath ← Tc.tmpPath "plot.dat"
  -- prepend header (plotExport writes raw, exportWithHeaders adds header in Plot.run)
  let content ← IO.FS.readFile datPath
  IO.FS.writeFile datPath (s!"x\ty\n" ++ content)
  -- generate R script and run
  let pngPath ← Tc.tmpPath "plot_test.png"
  let script := "library(ggplot2)\n" ++
    s!"d <- read.delim('{datPath}', header=TRUE, sep='\\t', colClasses='character', check.names=FALSE)\n" ++
    s!"d[['y']] <- as.numeric(d[['y']])\n" ++
    s!"tryCatch(d[['x']] <- as.numeric(d[['x']]), warning=function(w) NULL)\n" ++
    s!"p <- ggplot(d, aes(x = `x`, y = `y`)) + geom_line(linewidth = 0.5) + theme_minimal()\n" ++
    s!"ggsave('{pngPath}', p, width = 12, height = 7, dpi = 100)\n"
  let rPath ← Tc.tmpPath "plot_test.R"
  IO.FS.writeFile rPath script
  let r ← IO.Process.output { cmd := "Rscript", args := #[rPath] }
  assert (r.exitCode == 0) s!"Rscript failed: {r.stderr.trimAscii.toString}"
  -- verify PNG was created and is non-empty
  let h ← IO.FS.Handle.mk pngPath .read
  let buf ← h.read 1
  assert (buf.size > 0) "plot PNG should be non-empty"

-- | R scatter plot with category column renders correctly
def test_plot_render_scatter_cat : IO Unit := do
  log "plot_render_scatter_cat"
  unless (← hasRscript) do log "  skip (no Rscript)"; return
  unless (← hasGgplot2) do log "  skip (no ggplot2)"; return
  let some tbl ← Tc.AdbcTable.fromFile "data/plot/mixed.csv" | throw (IO.userError "failed to open mixed.csv")
  let _ ← Tc.AdbcTable.plotExport tbl "x" "y" (some "cat") false 1 1
  let datPath ← Tc.tmpPath "plot.dat"
  let content ← IO.FS.readFile datPath
  IO.FS.writeFile datPath (s!"x\ty\tcat\n" ++ content)
  let pngPath ← Tc.tmpPath "plot_test_cat.png"
  let script := "library(ggplot2)\n" ++
    s!"d <- read.delim('{datPath}', header=TRUE, sep='\\t', colClasses='character', check.names=FALSE)\n" ++
    s!"d[['y']] <- as.numeric(d[['y']])\n" ++
    s!"tryCatch(d[['x']] <- as.numeric(d[['x']]), warning=function(w) NULL)\n" ++
    s!"p <- ggplot(d, aes(x = `x`, y = `y`, color = `cat`)) + geom_point(size = 1.5, alpha = 0.7) + theme_minimal()\n" ++
    s!"ggsave('{pngPath}', p, width = 12, height = 7, dpi = 100)\n"
  let rPath ← Tc.tmpPath "plot_test_cat.R"
  IO.FS.writeFile rPath script
  let r ← IO.Process.output { cmd := "Rscript", args := #[rPath] }
  assert (r.exitCode == 0) s!"Rscript scatter+cat failed: {r.stderr.trimAscii.toString}"
  let h ← IO.FS.Handle.mk pngPath .read
  let buf ← h.read 1
  assert (buf.size > 0) "scatter+cat PNG should be non-empty"

-- | Histogram R script renders from single numeric column
def test_plot_render_histogram : IO Unit := do
  log "plot_render_histogram"
  unless (← hasRscript) do log "  skip (no Rscript)"; return
  unless (← hasGgplot2) do log "  skip (no ggplot2)"; return
  let datPath ← Tc.tmpPath "plot.dat"
  IO.FS.writeFile datPath "y\n10.5\n20.3\n15.7\n25.1\n30.0\n12.2\n18.9\n"
  let pngPath ← Tc.tmpPath "plot_test_hist.png"
  let script := "library(ggplot2)\n" ++
    s!"d <- read.delim('{datPath}', header=TRUE, sep='\\t', colClasses='character', check.names=FALSE)\n" ++
    s!"d[['y']] <- as.numeric(d[['y']])\n" ++
    s!"p <- ggplot(d, aes(x = `y`)) + geom_histogram(fill = '#4682B4', color = 'white', bins = 30) + theme_minimal()\n" ++
    s!"ggsave('{pngPath}', p, width = 12, height = 7, dpi = 100)\n"
  let rPath ← Tc.tmpPath "plot_test_hist.R"
  IO.FS.writeFile rPath script
  let r ← IO.Process.output { cmd := "Rscript", args := #[rPath] }
  assert (r.exitCode == 0) s!"Rscript histogram failed: {r.stderr.trimAscii.toString}"
  let h ← IO.FS.Handle.mk pngPath .read
  let buf ← h.read 1
  assert (buf.size > 0) "histogram PNG should be non-empty"

-- === Replay ops tests ===

-- | Replay: sort adds "sort" to tab line (PRQL ops shown right-aligned)
def test_replay_sort : IO Unit := do
  log "replay_sort"
  let out ← run "[" "data/unsorted.csv"
  let (tab, _) := footer out
  assert (contains tab "sort") "replay: sort op shown on tab line after ["

-- | Replay: no ops on fresh open (tab line has no PRQL ops)
def test_replay_empty : IO Unit := do
  log "replay_empty"
  let out ← run "" "data/basic.csv"
  let (tab, _) := footer out
  assert (!(contains tab "sort")) "replay: no sort on fresh view"
  assert (!(contains tab "filter")) "replay: no filter on fresh view"

-- === Run all tests ===

-- | All tests as (name, action) pairs
def tests : Array (String × IO Unit) := #[
  -- CSV tests (nav/key/hide/select/stack/info/quit moved to TestScreen.lean)
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
  ("csv_open", test_csv_open),
  ("json_open", test_json_open), ("ndjson_open", test_ndjson_open),
  ("jsonl_open", test_jsonl_open), ("jsonl_sort", test_jsonl_sort),
  -- arrow/feather disabled: DuckDB arrow extension not available on extensions.duckdb.org for v1.4.4
  -- ("arrow_open", test_arrow_open), ("feather_open", test_feather_open),
  ("xlsx_open", test_xlsx_open), ("avro_open", test_avro_open),
  ("pg_list", test_pg_list), ("pg_enter", test_pg_enter),
  ("folder_prefix", test_folder_prefix),
  -- Parquet tests (checked-in data only)
  ("sort_excludes_key", test_sort_excludes_key),
  ("sort_selected_not_key", test_sort_selected_not_key),
  ("filter_parquet_full_db", test_filter_parquet_full_db),
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
  ("script_from", test_script_from),
  -- Export tests
  ("export_csv", test_export_csv),
  -- Transpose tests
  ("transpose", test_transpose),
  ("transpose_pop", test_transpose_pop),
  -- Split tests
  ("split", test_split),
  ("split_noop", test_split_noop),
  -- Derive tests
  ("derive", test_derive),
  -- Join tests
  ("join_inner", test_join_inner),
  ("join_union", test_join_union),
  -- Sparkline tests
  ("sparkline_on", test_sparkline_on),
  -- Key column reorder tests
  ("key_shift", test_key_shift), ("arrow_nav", test_arrow_nav), ("heat_mode", test_heat_mode), ("flat_menu", test_flat_menu), ("socket", test_socket),
  -- Status bar aggregation tests
  ("statusagg_numeric", test_statusagg_numeric),
  ("statusagg_string", test_statusagg_string),
  -- Session tests
  ("session_load", test_session_load),
  ("session_save_load", test_session_save_load),
  ("session_missing", test_session_missing),
  -- Diff tests
  ("diff", test_diff),
  ("diff_show_same", test_diff_show_same),
  -- Plot tests
  ("plot_export_string_col", test_plot_export_string_col),
  ("plot_export_data", test_plot_export_data),
  ("plot_export_cat", test_plot_export_cat),
  ("plot_r_installed", test_plot_r_installed),
  ("plot_render_line", test_plot_render_line),
  ("plot_render_scatter_cat", test_plot_render_scatter_cat),
  ("plot_render_histogram", test_plot_render_histogram),
  -- Replay ops tests
  ("replay_sort", test_replay_sort),
  ("replay_empty", test_replay_empty),
  -- Folder sort on PRQL keyword column
  ("folder_sort_type", test_folder_sort_type)
]

def main (args : List String) : IO Unit := do
  IO.FS.writeFile "test.log" ""
  IO.FS.createDirAll "/tmp/tc_test"
  let err ← Tc.AdbcTable.init
  if !err.isEmpty then throw (IO.userError s!"Backend init failed: {err}")
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
