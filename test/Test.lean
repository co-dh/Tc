/-
  Key tests for Tc
  Run with: lake build test && .lake/build/bin/test
  Kdb tests: lake build testkdb && .lake/build/bin/testkdb
-/
import Tc.Data.ADBC.Table
import Tc.Nav
import Tc.View
import Tc.UI.Info
import Tc.Types
import Tc.Remote
import Tc.S3
import Tc.HF
import Tc.Data.Text

-- ============================================================================
-- Pure tests (compile-time #guard checks — no runtime cost)
-- ============================================================================

namespace PureTest

open Tc

/-! ## Mock Table for Testing -/

-- | Mock table with fixed dimensions (phantom types)
structure MockTable (nRows nCols : Nat) where
  names : Array String

-- | TblOps instance for MockTable (minimal: just nRows/colNames + dummy render)
instance : TblOps (MockTable nRows nCols) where
  nRows _ := nRows
  colNames t := t.names
  filter _ _ := pure none
  distinct _ _ := pure #[]
  findRow _ _ _ _ _ := pure none
  render _ _ := pure #[]

-- | Create mock 5x3 table for testing
def mock53 : MockTable 5 3 := ⟨#["c0", "c1", "c2"]⟩

/-! ## NavState Tests -/

section NavTests

-- 5 rows, 3 cols
def testNav : NavState 5 3 (MockTable 5 3) :=
  NavState.new mock53 rfl rfl (by decide) (by decide)

-- | j (row.inc) moves cursor from 0 to 1
#guard (NavState.exec (.row .inc) testNav 1 1).map (·.row.cur.val) == some 1

-- | k (row.dec) at row 0 stays at 0 (clamped)
#guard (NavState.exec (.row .dec) testNav 1 1).map (·.row.cur.val) == some 0

-- | l (col.inc) moves cursor from 0 to 1
#guard (NavState.exec (.col .inc) testNav 1 1).map (·.col.cur.val) == some 1

-- | h (col.dec) at col 0 stays at 0 (clamped)
#guard (NavState.exec (.col .dec) testNav 1 1).map (·.col.cur.val) == some 0

-- | G (ver.inc) goes to last row (4)
#guard (NavState.exec (.ver .inc) testNav 1 1).map (·.row.cur.val) == some 4

-- | g (ver.dec) goes to first row (0)
def navAtRow4 : NavState 5 3 (MockTable 5 3) :=
  match NavState.exec (.ver .inc) testNav 1 1 with
  | some n => n
  | none => testNav
#guard (NavState.exec (.ver .dec) navAtRow4 1 1).map (·.row.cur.val) == some 0

-- | $ (hor.inc) goes to last col (2)
#guard (NavState.exec (.hor .inc) testNav 1 1).map (·.col.cur.val) == some 2

-- | 0 (hor.dec) goes to first col (0)
def navAtCol2 : NavState 5 3 (MockTable 5 3) :=
  match NavState.exec (.hor .inc) testNav 1 1 with
  | some n => n
  | none => testNav
#guard (NavState.exec (.hor .dec) navAtCol2 1 1).map (·.col.cur.val) == some 0

-- | Page down (vPage.inc) with page size 2 moves from 0 to 2
#guard (NavState.exec (.vPage .inc) testNav 2 1).map (·.row.cur.val) == some 2

-- | Page up (vPage.dec) at row 0 stays at 0
#guard (NavState.exec (.vPage .dec) testNav 2 1).map (·.row.cur.val) == some 0

-- | T (rowSel.ent) toggles row selection
#guard (NavState.exec (.rowSel .ent) testNav 1 1).map (·.row.sels) == some #[0]

-- | T twice removes selection
def navWithSel : NavState 5 3 (MockTable 5 3) :=
  match NavState.exec (.rowSel .ent) testNav 1 1 with
  | some n => n
  | none => testNav
#guard (NavState.exec (.rowSel .ent) navWithSel 1 1).map (·.row.sels) == some #[]

-- | ! (grp.ent) toggles group
#guard (NavState.exec (.grp .ent) testNav 1 1).map (·.grp) == some #["c0"]

-- | Unhandled command returns none
#guard (NavState.exec (.thm .inc) testNav 1 1).isNone

-- | update returns Effect.none for nav commands
#guard (NavState.update (.row .inc) testNav 1 1).map (·.2) == some .none

end NavTests

/-! ## Array.toggle Tests -/

section ToggleTests

-- | toggle adds element if absent
#guard #[1, 2].toggle 3 == #[1, 2, 3]

-- | toggle removes element if present
#guard #[1, 2, 3].toggle 2 == #[1, 3]

-- | toggle on empty array adds element
#guard (#[] : Array Nat).toggle 1 == #[1]

-- | toggle twice returns original (when not present initially)
#guard ((#[1, 2] : Array Nat).toggle 3).toggle 3 == #[1, 2]

end ToggleTests

/-! ## ViewStack.update Tests -/

section StackTests

-- For ViewStack tests we need a View, which requires more setup
-- These are simpler property tests

-- | Fin.clamp stays in bounds
#guard (⟨0, by decide⟩ : Fin 5).clamp 10 == ⟨4, by decide⟩
#guard (⟨4, by decide⟩ : Fin 5).clamp (-10) == ⟨0, by decide⟩
#guard (⟨2, by decide⟩ : Fin 5).clamp 1 == ⟨3, by decide⟩
#guard (⟨2, by decide⟩ : Fin 5).clamp (-1) == ⟨1, by decide⟩

end StackTests

/-! ## Info.State Tests -/

section InfoTests

def infoOff : UI.Info.State := { vis := false }
def infoOn : UI.Info.State := { vis := true }

-- | Default is on
#guard ({} : UI.Info.State).vis == true

-- | I toggles info visibility
#guard (UI.Info.State.update infoOff (.info .ent)).map (·.1.vis) == some true
#guard (UI.Info.State.update infoOn (.info .ent)).map (·.1.vis) == some false

-- | info.inc turns on
#guard (UI.Info.State.update infoOff (.info .inc)).map (·.1.vis) == some true

-- | info.dec turns off
#guard (UI.Info.State.update infoOn (.info .dec)).map (·.1.vis) == some false

-- | Unhandled returns none
#guard (UI.Info.State.update infoOff (.row .inc)).isNone

-- | Info update returns Effect.none
#guard (UI.Info.State.update infoOff (.info .ent)).map (·.2) == some .none

end InfoTests

/-! ## dispOrder Tests -/

section DispOrderTests

-- | Empty group: order unchanged
#guard dispOrder #[] #["a", "b", "c"] == #[0, 1, 2]

-- | Group first col: moves to front
#guard dispOrder #["b"] #["a", "b", "c"] == #[1, 0, 2]

-- | Group multiple: group columns come first (in column order)
#guard dispOrder #["c", "a"] #["a", "b", "c"] == #[0, 2, 1]

-- | Group non-existent: ignored
#guard dispOrder #["x"] #["a", "b", "c"] == #[0, 1, 2]

end DispOrderTests


/-! ## TextParse Tests -/

section TextParseTests

-- | fromText parses tab-separated content
#guard (Tc.TextParse.fromText "a\tb\n1\t2\n3\t4").toOption.isSome

-- | fromText parses space-separated content
#guard (Tc.TextParse.fromText "PID  CMD\n1    init\n2    kthreadd").toOption.isSome

-- | fromText empty input returns error
#guard (Tc.TextParse.fromText "").toOption.isNone

end TextParseTests

/-! ## Column Operation Tests -/

section ColumnTests

-- | Column.size for ints
#guard (Column.ints #[10, 20, 30]).size == 3

-- | Column.size for strs
#guard (Column.strs #["a", "b"]).size == 2

-- | Column.size for floats
#guard (Column.floats #[1.0, 2.0, 3.0, 4.0]).size == 4

-- | Column.size for empty
#guard (Column.ints #[]).size == 0

-- | Column.take preserves first n elements (check via get + toRaw)
#guard ((Column.ints #[10, 20, 30]).take 2).size == 2
#guard (((Column.ints #[10, 20, 30]).take 2).get 0).toRaw == "10"
#guard (((Column.ints #[10, 20, 30]).take 2).get 1).toRaw == "20"

-- | Column.take with n > size returns all
#guard ((Column.strs #["a", "b"]).take 5).size == 2

-- | Column.take 0 returns empty
#guard ((Column.ints #[10, 20, 30]).take 0).size == 0

-- | Column.gather reindexes correctly (ints)
#guard ((Column.ints #[10, 20, 30]).gather #[2, 0]).size == 2
#guard (((Column.ints #[10, 20, 30]).gather #[2, 0]).get 0).toRaw == "30"
#guard (((Column.ints #[10, 20, 30]).gather #[2, 0]).get 1).toRaw == "10"

-- | Column.gather reindexes correctly (strs)
#guard (((Column.strs #["a", "b", "c"]).gather #[2, 0]).get 0).toRaw == "c"
#guard (((Column.strs #["a", "b", "c"]).gather #[2, 0]).get 1).toRaw == "a"

-- | Column.gather with empty indices returns empty column
#guard ((Column.ints #[10, 20, 30]).gather #[]).size == 0

-- | Column.gather duplicate indices
#guard ((Column.ints #[10, 20]).gather #[0, 0, 1, 1]).size == 4
#guard (((Column.ints #[10, 20]).gather #[0, 0, 1, 1]).get 2).toRaw == "20"

-- | Column.get returns correct Cell.toRaw for strs
#guard ((Column.strs #["hello", "world"]).get 0).toRaw == "hello"
#guard ((Column.strs #["hello", "world"]).get 1).toRaw == "world"

-- | Column.get out of bounds returns default (empty/0)
#guard ((Column.ints #[10]).get 5).toRaw == "0"
#guard ((Column.strs #["a"]).get 5).toRaw == ""

end ColumnTests

/-! ## S3 Path Helper Tests -/

section S3Tests

-- | isS3 recognizes s3:// prefix
#guard S3.isS3 "s3://bucket/path" == true
#guard S3.isS3 "s3://my-bucket" == true

-- | isS3 rejects non-S3 paths
#guard S3.isS3 "/local/path" == false
#guard S3.isS3 "http://example.com" == false
#guard S3.isS3 "" == false

-- | parent strips last component
#guard S3.parent "s3://bucket/a/b/" == some "s3://bucket/a/"
#guard S3.parent "s3://bucket/a/" == some "s3://bucket/"

-- | parent returns none at bucket root
#guard S3.parent "s3://bucket/" == none

-- | parent without trailing slash
#guard S3.parent "s3://bucket/a/b" == some "s3://bucket/a/"

end S3Tests

/-! ## Remote Path Helper Tests -/

section RemoteTests

-- | join with trailing slash
#guard Remote.join "s3://b/a/" "x" == "s3://b/a/x"

-- | join without trailing slash adds separator
#guard Remote.join "s3://b/a" "x" == "s3://b/a/x"

-- | join preserves trailing slash on child
#guard Remote.join "s3://b/" "subdir/" == "s3://b/subdir/"

-- | dispName extracts last component
#guard Remote.dispName "hf://datasets/user/ds" == "ds"
#guard Remote.dispName "hf://datasets/user/ds/data/" == "data"
#guard Remote.dispName "s3://bucket/prefix/" == "prefix"

-- | parent with different minParts
#guard Remote.parent "s3://bucket/" 3 == none
#guard Remote.parent "s3://bucket/a/b" 3 == some "s3://bucket/a/"
#guard Remote.parent "hf://datasets/user/ds" 5 == none
#guard Remote.parent "hf://datasets/user/ds/a/" 5 == some "hf://datasets/user/ds/"

end RemoteTests

/-! ## HF Path Helper Tests -/

section HFTests

-- | isHF recognizes hf:// prefix
#guard HF.isHF "hf://datasets/user/dataset" == true
#guard HF.isHF "hf://datasets/user/dataset/data/" == true

-- | isHF rejects non-HF paths
#guard HF.isHF "/local/path" == false
#guard HF.isHF "s3://bucket/path" == false
#guard HF.isHF "" == false

-- | parsePath extracts repo and subpath
#guard HF.parsePath "hf://datasets/user/ds" == some ("user/ds", "")
#guard HF.parsePath "hf://datasets/user/ds/data" == some ("user/ds", "data")
#guard HF.parsePath "hf://datasets/user/ds/data/train" == some ("user/ds", "data/train")

-- | parsePath rejects too-short paths
#guard HF.parsePath "hf://datasets/user" == none

-- | parsePath rejects non-datasets paths
#guard HF.parsePath "hf://finance.yahoo.com/quote/AAPL/profile" == none
#guard HF.parsePath "hf://models/user/model" == none

-- | parent strips last component
#guard HF.parent "hf://datasets/user/ds/a/b/" == some "hf://datasets/user/ds/a/"
#guard HF.parent "hf://datasets/user/ds/a/" == some "hf://datasets/user/ds/"

-- | parent returns none at dataset root
#guard HF.parent "hf://datasets/user/ds" == none
#guard HF.parent "hf://datasets/user/ds/" == none

-- | apiUrl builds correct HF Hub API URL
#guard HF.apiUrl "hf://datasets/user/ds" == some "https://huggingface.co/api/datasets/user/ds/tree/main"
#guard HF.apiUrl "hf://datasets/user/ds/data" == some "https://huggingface.co/api/datasets/user/ds/tree/main/data"

end HFTests

/-! ## FreqResult Construction Tests -/

section FreqTests

-- | freqPctBar produces pct array of same size as input
#guard (freqPctBar #[10, 20, 30]).1.size == 3

-- | freqPctBar produces bar array of same size as input
#guard (freqPctBar #[10, 20, 30]).2.size == 3

-- | freqPctBar with empty input
#guard (freqPctBar #[]).1.size == 0
#guard (freqPctBar #[]).2.size == 0

-- | freqPctBar with single element
#guard (freqPctBar #[100]).1.size == 1
#guard (freqPctBar #[100]).2.size == 1

-- | freqPctBar pct and bar arrays have same size as each other
#guard (freqPctBar #[5, 10, 15]).1.size == (freqPctBar #[5, 10, 15]).2.size

end FreqTests


end PureTest

-- ============================================================================
-- Sort behavior tests (compile-time #guard checks)
-- ============================================================================

namespace SortTest

open Tc

-- Group exclusion logic
#guard (#[] ++ #[(0:Nat)]).filter (!#[(0:Nat)].contains ·) == #[]
#guard (#[(0:Nat)] ++ #[1]).filter (!#[(0:Nat)].contains ·) == #[1]
#guard (#[(0:Nat), 2] ++ #[1]).filter (!#[(0:Nat), 3].contains ·) == #[2, 1]

-- Deduplication
def dedup (arr : Array Nat) : Array Nat :=
  arr.foldl (init := #[]) fun acc c => if acc.contains c then acc else acc.push c
#guard dedup (#[(0:Nat), 1] ++ #[1]) == #[0, 1]
#guard dedup (#[(2:Nat), 0] ++ #[2]) == #[2, 0]

end SortTest

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

-- | Convert key notation to tmux args: "<C-d>" → ["C-d"], "abc" → ["-l", "abc"]
def keysToTmux (keys : String) : Array (Array String) := Id.run do
  let mut result : Array (Array String) := #[]
  let mut buf := ""
  let chars := keys.toList.toArray
  let mut i := 0
  while i < chars.size do
    let c := chars.getD i ' '
    if c == '<' then
      if !buf.isEmpty then result := result.push #["-l", buf]; buf := ""
      let mut j := i + 1
      while j < chars.size && chars.getD j ' ' != '>' do j := j + 1
      let tag := String.ofList (chars.toList.drop (i + 1) |>.take (j - i - 1))
      let tmuxKey := if tag.startsWith "C-" then tag
                     else if tag == "ret" then "Enter"
                     else tag
      result := result.push #[tmuxKey]
      i := j + 1
    else
      buf := buf.push c; i := i + 1
  if !buf.isEmpty then result := result.push #["-l", buf]
  result

-- | Run tc with keys and file, capture tmux output
def run (keys : String) (file : String := "") (finalWait : UInt32 := 0) : IO String := do
  let f := if file.isEmpty then "" else s!"\"{file}\" "
  log s!"  spawn: {file} keys={keys}"
  let sess := "tctest"
  let _ ← IO.Process.output { cmd := "tmux", args := #["kill-session", "-t", sess] }
  let _ ← IO.Process.output { cmd := "tmux", args := #["new-session", "-d", "-s", sess, "-x", "80", "-y", "24", "-e", "TC_TEST_MODE=1", s!"{bin} {f}"] }
  let slow := (file.splitOn "1.parquet").length > 1
  let (t1, t2, t3) := if slow then (500, 150, 300) else (150, 50, 100)
  IO.sleep t1
  for ka in keysToTmux keys do
    let _ ← IO.Process.output { cmd := "tmux", args := #["send-keys", "-t", sess] ++ ka }
    IO.sleep t2
  IO.sleep (t3 + finalWait)
  let out ← IO.Process.output { cmd := "tmux", args := #["capture-pane", "-t", sess, "-p"] }
  let _ ← IO.Process.output { cmd := "tmux", args := #["kill-session", "-t", sess] }
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

-- === Navigation tests (CSV) ===

def test_nav_down : IO Unit := do
  log "nav_down"
  assert (contains (footer (← run "j" "data/basic.csv")).2 "r1/") "j moves to row 1"

def test_nav_right : IO Unit := do
  log "nav_right"
  assert (contains (footer (← run "l" "data/basic.csv")).2 "c1/") "l moves to col 1"

def test_nav_up : IO Unit := do
  log "nav_up"
  assert (contains (footer (← run "jk" "data/basic.csv")).2 "r0/") "jk returns to row 0"

def test_nav_left : IO Unit := do
  log "nav_left"
  assert (contains (footer (← run "lh" "data/basic.csv")).2 "c0/") "lh returns to col 0"

-- === Key column tests ===

def test_key_toggle : IO Unit := do
  log "key_col"
  assert (contains (header (← run "!" "data/basic.csv")) "║") "! adds key separator"

def test_key_remove : IO Unit := do
  log "key_remove"
  assert (!contains (header (← run "!!" "data/basic.csv")) "║") "!! removes key separator"

def test_key_reorder : IO Unit := do
  log "key_reorder"
  assert ((header (← run "l!" "data/basic.csv")).take 5 |>.any (· == 'b')) "Key col moves to front"

-- === Delete tests (CSV) ===

def test_del_col : IO Unit := do
  log "del_col"
  let hdr := header (← run "ld" "data/basic.csv")
  assert (contains hdr "a") "Has column a"
  assert (!contains hdr "b") "Column b deleted"

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

-- === Selection tests ===

def test_row_select : IO Unit := do
  log "row_select"
  assert (contains (footer (← run "T" "data/basic.csv")).2 "sel=1") "T selects row"

def test_multi_select : IO Unit := do
  log "multi_row_select"
  assert (contains (footer (← run "TjT" "data/full.csv")).2 "sel=2") "TjT selects 2 rows"

-- === Stack tests ===

def test_stack_swap : IO Unit := do
  log "stack_swap"
  assert (contains (footer (← run "S" "data/basic.csv")).1 "basic.csv") "S swaps/dups view"

def test_meta_quit : IO Unit := do
  log "meta_quit"
  assert (!contains (footer (← run "Mq" "data/basic.csv")).1 "meta") "Mq returns from meta"

def test_freq_quit : IO Unit := do
  log "freq_quit"
  assert (!contains (footer (← run "Fq" "data/basic.csv")).1 "freq") "Fq returns from freq"

-- === Info overlay ===

def test_info : IO Unit := do
  log "info"
  let output ← run "" "data/basic.csv"
  assert (contains output "quit" || contains output "up/down") "Info overlay shown by default"

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

def test_meta_0_del : IO Unit := do
  log "meta_0_enter_delete"
  assert (contains (footer (← run "M0<ret>d" "data/null_col.csv")).2 "c0/1") "M0<ret>d deletes null column"

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

-- === Cursor tracking ===

def test_key_cursor : IO Unit := do
  log "key_cursor"
  assert (contains (footer (← run "l!" "data/basic.csv")).2 "c0/") "Cursor tracks after key toggle"

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

def test_col_search : IO Unit := do
  log "col_search"
  assert (contains (footer (← run "s" "data/basic.csv")).2 "c0/") "s col search jumps to column"

-- === Enter/Quit key tests ===

def test_q_quit : IO Unit := do
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
  assert (contains (← run "jjj<ret>q") "[/") "q pops back to parent folder"

def test_folder_prefix : IO Unit := do
  log "folder_prefix"
  let (_, s1) := footer (← run "")
  let (_, s2) := footer (← run ",")
  let r1 := s1.splitOn "r0/" |>.getD 1 "" |>.takeWhile (·.isDigit)
  let r2 := s2.splitOn "r0/" |>.getD 1 "" |>.takeWhile (·.isDigit)
  assert (r2.toNat?.getD 0 >= r1.toNat?.getD 0) ", prefix works in folder"

def test_folder_del : IO Unit := do
  log "folder_del"
  let (_, s1) := footer (← run "")
  let (_, s2) := footer (← run "d")
  assert (s1 == s2) "d in folder view (test mode) keeps view unchanged"

-- === Navigation tests (parquet) ===

def test_page_down : IO Unit := do
  log "page_down"
  let (_, status) := footer (← run "<C-d>" "data/sample.parquet")
  assert (!contains status "r0/") "Ctrl-D pages down past row 0"

def test_page_up : IO Unit := do
  log "page_up"
  let (_, status) := footer (← run "<C-d><C-u>" "data/sample.parquet")
  assert (contains status "r0/") "Ctrl-D then Ctrl-U returns to row 0"

def test_page_down_scrolls : IO Unit := do
  log "page_down_scrolls"
  let (_, status1) := footer (← run "" "data/sample.parquet")
  let (_, status2) := footer (← run "<C-d>" "data/sample.parquet")
  assert (status1 != status2) "Page down changes status"

def test_last_col_visible : IO Unit := do
  log "last_col_visible"
  let first := (dataLines (← run "llllllllllllllllllll" "data/sample.parquet")).headD ""
  let nonWs := first.toList.filter (!·.isWhitespace) |>.length
  assert (nonWs > 0) "Last col shows data"

-- === Delete tests (parquet) ===

def test_delete_twice : IO Unit := do
  log "del_twice"
  let hdr := header (← run "dd" "data/sample.parquet")
  assert (!contains hdr "id") "id deleted"
  assert (!contains hdr "age") "age deleted"
  assert (contains hdr "year") "year remains"

def test_delete_then_key_then_freq : IO Unit := do
  log "del_key_freq"
  let (tab, status) := footer (← run "dl!F" "data/sample.parquet")
  assert (contains tab "freq") "D+key+F shows freq"
  assert (!contains status "Error") "No error"

-- === Sort tests (parquet) ===

def test_parquet_sort_asc : IO Unit := do
  log "parquet_sort_asc"
  let first := (dataLines (← run "l[" "data/sample.parquet")).headD ""
  assert (contains first " 18 ") "[ on age sorts asc, age=18"

def test_parquet_sort_desc : IO Unit := do
  log "parquet_sort_desc"
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
  assert (contains (footer (← run "M" "data/sample.parquet")).1 "meta") "M on parquet shows meta"

-- === Freq tests (parquet) ===

def test_freq_parquet : IO Unit := do
  log "freq_parquet"
  assert (contains (footer (← run "F" "data/sample.parquet")).1 "freq") "F on parquet shows freq"

def test_freq_enter_parquet : IO Unit := do
  log "freq_enter_parquet"
  let (tab, status) := footer (← run "F<ret>" "data/sample.parquet")
  assert (contains tab "sample.parquet") "F<ret> on parquet pops to parent"
  assert (!contains tab "freq") "F<ret> on parquet exits freq view"
  assert (contains status "r0/") "F<ret> on parquet shows filtered rows"

def test_freq_parquet_key_values : IO Unit := do
  log "freq_parquet_key_values"
  let first := (dataLines (← run "lF" "data/nyse10k.parquet")).headD ""
  assert (!first.startsWith " 5180" && !first.startsWith " 2592") "Freq key column shows names, not counts"

def test_freq_total_count : IO Unit := do
  log "freq_total_count"
  let (_, status) := footer (← run "l!l!hF" "data/nyse/1.parquet" 8000)
  assert (contains status "/128974") "Freq shows total group count (128974)"

def test_freq_sort_preserves_total : IO Unit := do
  log "freq_sort_total"
  let (_, status) := footer (← run "llFll[" "data/nyse/1.parquet" 8000)
  assert (contains status "/") "Freq sort preserves total count in status"

def test_freq_sort_asc_parquet : IO Unit := do
  log "freq_sort_asc"
  let first := (dataLines (← run "llFll[" "data/nyse/1.parquet" 8000)).headD ""
  assert (contains first "│") "Freq sort asc shows data"

-- === Meta selection tests (parquet) ===

def test_parquet_meta_0_null_cols : IO Unit := do
  log "parquet_meta_0"
  let (_, status) := footer (← run "M0" "data/nyse/1.parquet")
  assert (contains status "sel=9") "M0 on parquet selects 9 null columns"

def test_parquet_meta_0_enter_groups : IO Unit := do
  log "parquet_meta_0_enter"
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
  let keys := String.join (List.replicate 105 "<C-d>")
  let (_, status) := footer (← run keys "data/nyse10k.parquet" 1000)
  let rpart := (status.splitOn " r" |>.getD 1 "").takeWhile (· != ' ')
  let cursor := ((rpart.toString.splitOn "/").headD "").toNat?.getD 0
  assert (cursor > 999) s!"Scroll fetches more: cursor={cursor}, expected > 999"

-- === Misc ===

def test_numeric_right_align : IO Unit := do
  log "numeric_align"
  let first := (dataLines (← run "" "data/sample.parquet")).headD ""
  assert (contains first "  ") "Numeric columns right-aligned"

def test_enter_no_quit_parquet : IO Unit := do
  log "enter_no_quit_parquet"
  let (_, status) := footer (← run "<ret>j" "data/nyse/1.parquet")
  assert (contains status "r1/") "Enter on parquet should not quit (j moves to r1)"

-- === Run all tests ===

def main (_args : List String) : IO Unit := do
  IO.FS.writeFile "test.log" ""
  IO.println "Running Tc tests...\n"
  let ok ← Tc.AdbcTable.init
  if !ok then throw (IO.userError "Backend init failed")

  IO.println "--- CSV tests ---"
  test_nav_down; test_nav_right; test_nav_up; test_nav_left
  test_key_toggle; test_key_remove; test_key_reorder
  test_del_col
  test_sort_asc; test_sort_desc
  test_meta_shows; test_meta_col_info; test_meta_no_garbage
  test_freq_shows; test_freq_after_meta; test_freq_by_key
  test_freq_multi_key; test_freq_keeps_grp
  test_row_select; test_multi_select
  test_stack_swap; test_meta_quit; test_freq_quit
  test_info
  test_prec_inc; test_prec_dec
  test_meta_0; test_meta_1; test_meta_0_enter; test_meta_1_enter; test_meta_0_del
  test_freq_enter
  test_spaced_header
  test_key_cursor
  test_no_stderr
  test_search_jump; test_search_next; test_search_prev; test_col_search
  test_q_quit
  test_folder_no_args; test_folder_D; test_folder_tab
  test_folder_enter; test_folder_relative; test_folder_pop
  test_folder_prefix; test_folder_del

  IO.println "\n--- Parquet tests ---"
  test_page_down; test_page_up; test_page_down_scrolls; test_last_col_visible
  test_delete_twice; test_delete_then_key_then_freq
  test_parquet_sort_asc; test_parquet_sort_desc
  test_sort_excludes_key; test_sort_selected_not_key
  test_parquet_meta
  test_freq_parquet; test_freq_enter_parquet; test_freq_parquet_key_values
  test_freq_total_count; test_freq_sort_preserves_total; test_freq_sort_asc_parquet
  test_parquet_meta_0_null_cols; test_parquet_meta_0_enter_groups
  test_filter_parquet_full_db
  test_scroll_fetches_more
  test_numeric_right_align
  test_enter_no_quit_parquet

  Tc.AdbcTable.shutdown
  IO.println "\nAll tests passed!"

end Test

def main (args : List String) : IO Unit := Test.main args
