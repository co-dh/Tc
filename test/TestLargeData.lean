/-
  Tests requiring large/untracked data files (sample.parquet, nyse, pac.csv, etc.)
  Run locally with: lake run runlarge
  Skipped in CI since these files are gitignored.
-/
import Tc.Data.ADBC.Table
import Tc.SourceConfig

namespace TestLargeData

open Tc

def bin := ".lake/build/bin/tc"

def log (msg : String) : IO Unit := do
  let h ← IO.FS.Handle.mk "test.log" .append
  h.putStrLn msg; h.flush

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

def hasFile (path : String) : IO Bool :=
  try let _ ← IO.FS.Handle.mk path .read; pure true
  catch _ => pure false

-- === Width stability (pac.csv — gitignored) ===

-- Verify column widths don't change after toggling info panel
def test_width_stable_after_info : IO Unit := do
  log "width_stable_after_info"
  let hdr1 := header (← run "" "data/pac.csv")
  let hdr2 := header (← run "I" "data/pac.csv")
  let hdr3 := header (← run "II" "data/pac.csv")
  assert (hdr1 == hdr2) s!"Info toggle must not change column widths: [{hdr1}] vs [{hdr2}]"
  assert (hdr1 == hdr3) s!"Info double-toggle must not change column widths"

-- === Navigation tests (sample.parquet — gitignored) ===

-- Ctrl-D pages down past row 0
def test_page_down : IO Unit := do
  log "page_down"
  let (_, status) := footer (← run "<C-d>" "data/sample.parquet")
  assert (!contains status "r0/") "Ctrl-D pages down past row 0"

-- Ctrl-D then Ctrl-U returns to row 0
def test_page_up : IO Unit := do
  log "page_up"
  let (_, status) := footer (← run "<C-d><C-u>" "data/sample.parquet")
  assert (contains status "r0/") "Ctrl-D then Ctrl-U returns to row 0"

-- Page down changes status
def test_page_down_scrolls : IO Unit := do
  log "page_down_scrolls"
  let (_, status1) := footer (← run "" "data/sample.parquet")
  let (_, status2) := footer (← run "<C-d>" "data/sample.parquet")
  assert (status1 != status2) "Page down changes status"

-- Last column shows data when scrolled far right
def test_last_col_visible : IO Unit := do
  log "last_col_visible"
  let first := (dataLines (← run "llllllllllllllllllll" "data/sample.parquet")).headD ""
  let nonWs := first.toList.filter (!·.isWhitespace) |>.length
  assert (nonWs > 0) "Last col shows data"

-- === Sort tests (sample.parquet) ===

-- Sort asc on age column gives age=18 first
def test_parquet_sort_asc : IO Unit := do
  log "parquet_sort_asc"
  let first := (dataLines (← run "l[" "data/sample.parquet")).headD ""
  assert (contains first " 18 ") "[ on age sorts asc, age=18"

-- Sort desc on age column gives age=80 first
def test_parquet_sort_desc : IO Unit := do
  log "parquet_sort_desc"
  let first := (dataLines (← run "l]" "data/sample.parquet")).headD ""
  assert (contains first " 80 ") "] on age sorts desc, age=80"

-- === Meta tests (sample.parquet) ===

-- M on parquet shows meta view
def test_parquet_meta : IO Unit := do
  log "parquet_meta"
  assert (contains (footer (← run "M" "data/sample.parquet")).1 "meta") "M on parquet shows meta"

-- === Freq tests (sample.parquet) ===

-- F on parquet shows freq view
def test_freq_parquet : IO Unit := do
  log "freq_parquet"
  assert (contains (footer (← run "F" "data/sample.parquet")).1 "freq") "F on parquet shows freq"

-- F<ret> on parquet exits freq and filters
def test_freq_enter_parquet : IO Unit := do
  log "freq_enter_parquet"
  let (tab, status) := footer (← run "F<ret>" "data/sample.parquet")
  assert (contains tab "sample.parquet") "F<ret> on parquet pops to parent"
  assert (!contains tab "freq") "F<ret> on parquet exits freq view"
  assert (contains status "r0/") "F<ret> on parquet shows filtered rows"

-- === Freq tests (nyse10k.parquet — gitignored) ===

-- Freq key column shows names, not counts
def test_freq_parquet_key_values : IO Unit := do
  log "freq_parquet_key_values"
  let first := (dataLines (← run "lF" "data/nyse10k.parquet")).headD ""
  assert (!first.startsWith " 5180" && !first.startsWith " 2592") "Freq key column shows names, not counts"

-- === Freq tests (nyse — gitignored) ===

-- Freq shows total group count
def test_freq_total_count : IO Unit := do
  log "freq_total_count"
  let (_, status) := footer (← run "l!l!hF" "data/nyse/1.parquet")
  assert (contains status "/128974") "Freq shows total group count (128974)"

-- Freq sort preserves total count
def test_freq_sort_preserves_total : IO Unit := do
  log "freq_sort_total"
  let (_, status) := footer (← run "llFll[" "data/nyse/1.parquet")
  assert (contains status "/") "Freq sort preserves total count in status"

-- Freq sort asc shows data
def test_freq_sort_asc_parquet : IO Unit := do
  log "freq_sort_asc"
  let first := (dataLines (← run "llFll[" "data/nyse/1.parquet")).headD ""
  assert (contains first "│") "Freq sort asc shows data"

-- === Meta selection tests (nyse) ===

-- M0 selects null columns in parquet
def test_parquet_meta_0_null_cols : IO Unit := do
  log "parquet_meta_0"
  let (_, status) := footer (← run "M0" "data/nyse/1.parquet")
  assert (contains status "sel=9") "M0 on parquet selects 9 null columns"

-- M0<ret> groups null columns
def test_parquet_meta_0_enter_groups : IO Unit := do
  log "parquet_meta_0_enter"
  let (tab, status) := footer (← run "M0<ret>" "data/nyse/1.parquet")
  assert (tab.startsWith "[1.parquet]") "M0<ret> returns to parent view"
  assert (contains status "grp=9") "M0<ret> groups 9 null columns"

-- === Scroll fetch tests (nyse10k.parquet) ===

-- Scrolling down fetches more rows
def test_scroll_fetches_more : IO Unit := do
  log "scroll_fetches_more"
  let keys := String.join (List.replicate 105 "<C-d>")
  let (_, status) := footer (← run keys "data/nyse10k.parquet")
  let rpart := (status.splitOn " r" |>.getD 1 "").takeWhile (· != ' ')
  let cursor := ((rpart.toString.splitOn "/").headD "").toNat?.getD 0
  assert (cursor > 999) s!"Scroll fetches more: cursor={cursor}, expected > 999"

-- === Misc (sample.parquet, nyse) ===

-- Numeric columns are right-aligned
def test_numeric_right_align : IO Unit := do
  log "numeric_align"
  let first := (dataLines (← run "" "data/sample.parquet")).headD ""
  assert (contains first "  ") "Numeric columns right-aligned"

-- Enter on parquet should not quit
def test_enter_no_quit_parquet : IO Unit := do
  log "enter_no_quit_parquet"
  let (_, status) := footer (← run "<ret>j" "data/nyse/1.parquet")
  assert (contains status "r1/") "Enter on parquet should not quit (j moves to r1)"

-- === Run all large-data tests ===

def tests : Array (String × IO Unit) := #[
  ("width_stable_after_info", test_width_stable_after_info),
  ("page_down", test_page_down), ("page_up", test_page_up),
  ("page_down_scrolls", test_page_down_scrolls),
  ("last_col_visible", test_last_col_visible),
  ("parquet_sort_asc", test_parquet_sort_asc),
  ("parquet_sort_desc", test_parquet_sort_desc),
  ("parquet_meta", test_parquet_meta),
  ("freq_parquet", test_freq_parquet),
  ("freq_enter_parquet", test_freq_enter_parquet),
  ("freq_parquet_key_values", test_freq_parquet_key_values),
  ("freq_total_count", test_freq_total_count),
  ("freq_sort_preserves_total", test_freq_sort_preserves_total),
  ("freq_sort_asc_parquet", test_freq_sort_asc_parquet),
  ("parquet_meta_0_null_cols", test_parquet_meta_0_null_cols),
  ("parquet_meta_0_enter_groups", test_parquet_meta_0_enter_groups),
  ("scroll_fetches_more", test_scroll_fetches_more),
  ("numeric_right_align", test_numeric_right_align),
  ("enter_no_quit_parquet", test_enter_no_quit_parquet)
]

def main (args : List String) : IO Unit := do
  IO.FS.writeFile "test.log" ""
  let ok ← Tc.AdbcTable.init
  if !ok then throw (IO.userError "Backend init failed")
  let filter := args.head?
  let selected := match filter with
    | none => tests
    | some f => tests.filter fun (name, _) => (name.splitOn f).length > 1
  if selected.isEmpty then
    IO.eprintln s!"No tests matching '{filter.getD ""}'"
    return
  IO.println s!"Running {selected.size} large-data test(s)...\n"
  for (_, action) in selected do action
  Tc.AdbcTable.shutdown
  IO.println "\nAll large-data tests passed!"

end TestLargeData

def main (args : List String) : IO Unit := TestLargeData.main args
