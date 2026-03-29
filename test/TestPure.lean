/-
  Pure core tests derived from screen-based tests.
  Theorems (native_decide) replace #guard where they capture algebraic invariants.
  Remaining #guard checks cover concrete dispatch/effect tests.
-/
import Tc.Nav
import Tc.View
import Tc.UI.Info
import Tc.Types
import Tc.Filter
import Tc.Folder
import Tc.Data.Text
import Tc.Remote
import Tc.Session

namespace PureTest2

open Tc

/-! ## Mock Table -/

structure MockTable (nRows nCols : Nat) where
  names : Array String

instance : TblOps (MockTable nRows nCols) where
  nRows _ := nRows
  colNames t := t.names
  filter _ _ := pure none
  distinct _ _ := pure #[]
  findRow _ _ _ _ _ := pure none
  render _ _ := pure #[]

def mock53 : MockTable 5 3 := ⟨#["c0", "c1", "c2"]⟩

def testNav : NavState 5 3 (MockTable 5 3) :=
  NavState.new mock53 rfl rfl (by decide) (by decide)

def testView : View (MockTable 5 3) :=
  View.new (NavState.new mock53 rfl rfl (by decide) (by decide)) "data/test.csv"

def testStack : ViewStack (MockTable 5 3) := ⟨testView, []⟩

/-! ## Key Mapping Tests (derived from screen tests)
    Screen tests use keystroke injection and check screen output.
    Here we test the pure key→cmd pipeline directly. -/

section KeyMapTests

-- Navigation keys via evToCmd (hjkl, arrows — not in KeyMap.char)
theorem key_j : evToCmd (charToEvent 'j') .tbl = some (Cmd.row '>') := by native_decide
theorem key_k : evToCmd (charToEvent 'k') .tbl = some (Cmd.row '<') := by native_decide
theorem key_l : evToCmd (charToEvent 'l') .tbl = some (Cmd.col '>') := by native_decide
theorem key_h : evToCmd (charToEvent 'h') .tbl = some (Cmd.col '<') := by native_decide

-- Single-key shortcuts via KeyMap.char (centralized data table, all obj+verb Cmd)
theorem key_bang  : lookup KeyMap.char '!' = some (Cmd.col '!')        := by native_decide
theorem key_T     : lookup KeyMap.char 'T' = some (Cmd.row '~')       := by native_decide
theorem key_lbr   : lookup KeyMap.char '[' = some (Cmd.col '[')       := by native_decide
theorem key_rbr   : lookup KeyMap.char ']' = some (Cmd.col ']')       := by native_decide
theorem key_q     : lookup KeyMap.char 'q' = some (Cmd.stk '<')       := by native_decide
theorem key_space : lookup KeyMap.char ' ' = some (Cmd.stk '/')    := by native_decide
theorem key_n     : lookup KeyMap.char 'n' = some (Cmd.row '+')       := by native_decide
theorem key_N     : lookup KeyMap.char 'N' = some (Cmd.row '-')       := by native_decide
theorem key_lbrace: lookup KeyMap.char '{' = some (Cmd.info '[')       := by native_decide
theorem key_rbrace: lookup KeyMap.char '}' = some (Cmd.info ']')       := by native_decide

-- Ctrl keys via evToCmd (from test_page_down/up)
theorem key_ctrlD : evToCmd (charToEvent '\x04') .tbl = some (Cmd.row ']') := by native_decide
theorem key_ctrlU : evToCmd (charToEvent '\x15') .tbl = some (Cmd.row '[') := by native_decide

-- Context-sensitive Enter via evToCmd (from test_freq_enter, test_meta_0_enter, test_folder_enter)
theorem enter_freq : evToCmd (charToEvent '\r') (.freqV #["a"] 10) = some (Cmd.freq '~') := by native_decide
theorem enter_meta : evToCmd (charToEvent '\r') .colMeta = some (Cmd.metaV '~')           := by native_decide
theorem enter_fld  : evToCmd (charToEvent '\r') (.fld "/tmp" 1) = some (Cmd.fld '~')      := by native_decide
theorem enter_tbl  : evToCmd (charToEvent '\r') .tbl = none                             := by native_decide

-- Backspace in folder view → parent (from test_folder_backspace)
theorem bs_fld : evToCmd (charToEvent '\x7f') (.fld "/tmp" 1) = some (Cmd.fld '{')       := by native_decide
-- Backspace outside folder view → no action
theorem bs_tbl : evToCmd (charToEvent '\x7f') .tbl = none                               := by native_decide

-- Unmodified arrow keys → single-step nav, not page (bug: termbox2 tagged \x1b[A-D as shift)
theorem key_arrow_down : evToCmd ⟨Term.eventKey, 0, Term.keyArrowDown, 0, 0, 0⟩ .tbl
    = some (Cmd.row '>') := by native_decide
theorem key_arrow_up : evToCmd ⟨Term.eventKey, 0, Term.keyArrowUp, 0, 0, 0⟩ .tbl
    = some (Cmd.row '<') := by native_decide
theorem key_arrow_right : evToCmd ⟨Term.eventKey, 0, Term.keyArrowRight, 0, 0, 0⟩ .tbl
    = some (Cmd.col '>') := by native_decide
theorem key_arrow_left : evToCmd ⟨Term.eventKey, 0, Term.keyArrowLeft, 0, 0, 0⟩ .tbl
    = some (Cmd.col '<') := by native_decide

-- Synthetic arrow chars (\x1c-\x1f) via charToEvent → single-step nav
theorem key_synth_down : evToCmd (charToEvent '\x1c') .tbl = some (Cmd.row '>') := by native_decide
theorem key_synth_up : evToCmd (charToEvent '\x1d') .tbl = some (Cmd.row '<') := by native_decide
theorem key_synth_right : evToCmd (charToEvent '\x1e') .tbl = some (Cmd.col '>') := by native_decide
theorem key_synth_left : evToCmd (charToEvent '\x1f') .tbl = some (Cmd.col '<') := by native_decide

-- Shift+Arrow keys (from test_key_shift: reorder key columns)
-- Synthetic shift+arrow events: mod=4 (modShift), key=arrow code
theorem key_shift_left : evToCmd ⟨Term.eventKey, Term.modShift, Term.keyArrowLeft, 0, 0, 0⟩ .tbl
    = some (Cmd.col '-') := by native_decide
theorem key_shift_right : evToCmd ⟨Term.eventKey, Term.modShift, Term.keyArrowRight, 0, 0, 0⟩ .tbl
    = some (Cmd.col '+') := by native_decide

-- Test synthetic shift+arrow via charToEvent (\x11=S-left, \x12=S-right)
theorem key_synth_shift_left : evToCmd (charToEvent '\x11') .tbl = some (Cmd.col '-') := by native_decide
theorem key_synth_shift_right : evToCmd (charToEvent '\x12') .tbl = some (Cmd.col '+') := by native_decide

end KeyMapTests

/-! ## View.update Tests (derived from screen tests) -/

section ViewUpdateTests

-- [ (col.lbr) returns sort effect with asc=true (from test_sort_asc)
#guard (View.update testView "sort.asc" 1).map (·.2) ==
  some (.query (.sort 0 #[] #[] true))

-- ] (col.rbr) returns sort effect with asc=false (from test_sort_desc)
#guard (View.update testView "sort.desc" 1).map (·.2) ==
  some (.query (.sort 0 #[] #[] false))

-- Navigation delegates to NavState and returns Effect.none
-- (from test_nav_down: "j moves to row 1")
#guard (View.update testView "nav.rowInc" 1).map (·.1.nav.row.cur.val) == some 1
#guard (View.update testView "nav.rowInc" 1).map (·.2) == some .none

-- row.dec at 0 stays at 0 (from test_nav_up: "jk returns to row 0")
#guard (View.update testView "nav.rowDec" 1).map (·.1.nav.row.cur.val) == some 0

end ViewUpdateTests

/-! ## ViewStack.update Tests (derived from screen tests) -/

section ViewStackUpdateTests

-- S (stk.ent) swaps (from test_stack_swap: "S swaps/dups view")
-- On single-element stack, swap is identity
#guard (ViewStack.update testStack "stk.swap").map (·.1.hd.path) == some "data/test.csv"
#guard (ViewStack.update testStack "stk.swap").map (·.2) == some .none

-- stk.inc dups: pushes copy
#guard (ViewStack.update testStack "stk.dup").map (·.1.tl.length) == some 1
#guard (ViewStack.update testStack "stk.dup").map (·.2) == some .none

-- q (stk.dec) on empty stack → quit (from test_q_quit: "q on empty stack exits cleanly")
#guard (ViewStack.update testStack "stk.pop").map (·.2) == some .quit

-- q (stk.dec) with parent → pops (from test_meta_quit, test_freq_quit)
def twoStack : ViewStack (MockTable 5 3) := testStack.dup
#guard (ViewStack.update twoStack "stk.pop").map (·.1.tl.length) == some 0
#guard (ViewStack.update twoStack "stk.pop").map (·.2) == some .none

-- Unhandled commands return none
#guard (ViewStack.update testStack "nav.rowInc").isNone

end ViewStackUpdateTests

/-! ## View.tabName Tests (derived from screen tests) -/

section TabNameTests

-- Table view: shows filename (from test_freq_enter_parquet: "tab shows sample.parquet")
def tblView : View (MockTable 5 3) :=
  View.new (NavState.new mock53 rfl rfl (by decide) (by decide)) "data/sample.parquet"
#guard tblView.tabName == "sample.parquet"

-- Folder view: shows path (from test_folder_tab: "Folder tab shows absolute path")
def fldView : View (MockTable 5 3) :=
  { tblView with vkind := .fld "/home/user/Tc" 1, path := "/home/user/Tc" }
#guard fldView.tabName == "/home/user/Tc"

-- Custom disp name (from test_meta_shows: "M shows meta in tab")
def metaView' : View (MockTable 5 3) :=
  { testView with vkind := .colMeta, disp := "meta" }
#guard metaView'.tabName == "meta"

-- Freq view with custom disp (from test_freq_shows: "F shows freq in tab")
def freqView' : View (MockTable 5 3) :=
  { testView with vkind := .freqV #["c0"] 5, disp := "freq" }
#guard freqView'.tabName == "freq"

end TabNameTests

/-! ## Filter.update Effect Tests (derived from screen tests) -/

section FilterUpdateTests

-- / → fzf.row (from test_search_jump: row search)
#guard (Tc.Filter.update testStack "filter.rowSearch").map (·.2) == some (.fzf .row)

-- \ → fzf.filter (from test_filter_parquet_full_db: row filter)
#guard (Tc.Filter.update testStack "filter.rowFilter").map (·.2) == some (.fzf .filter)

-- s → fzf.col (from test_col_search: column search)
#guard (Tc.Filter.update testStack "filter.colSearch").map (·.2) == some (.fzf .col)

-- n → search.next (from test_search_next)
#guard (Tc.Filter.update testStack "filter.searchNext").map (·.2) == some (.search .next)

-- N → search.prev (from test_search_prev)
#guard (Tc.Filter.update testStack "filter.searchPrev").map (·.2) == some (.search .prev)

-- Unhandled returns none
#guard (Tc.Filter.update testStack "nav.rowInc").isNone

end FilterUpdateTests

/-! ## Cmd Parse/ToString Round-Trip Theorems -/

section CmdRoundTripTests

-- Cmd parse roundtrip (obj+verb pairs)
theorem parse_r_inc : (@Parse.parse? Cmd _ "r>") = some (Cmd.row '>') := by native_decide
theorem parse_r_dec : (@Parse.parse? Cmd _ "r<") = some (Cmd.row '<') := by native_decide
theorem parse_r_ent : (@Parse.parse? Cmd _ "r~") = some (Cmd.row '~') := by native_decide
theorem parse_r_del : (@Parse.parse? Cmd _ "r-") = some (Cmd.row '-') := by native_decide
theorem parse_r_dup : (@Parse.parse? Cmd _ "r+") = some (Cmd.row '+') := by native_decide
theorem parse_r_lbr : (@Parse.parse? Cmd _ "r[") = some (Cmd.row '[') := by native_decide
theorem parse_c_inc : (@Parse.parse? Cmd _ "c>") = some (Cmd.col '>') := by native_decide
theorem parse_c_dec : (@Parse.parse? Cmd _ "c<") = some (Cmd.col '<') := by native_decide
-- Heat mode: now on info object (i0-i3)
theorem parse_i0 : (@Parse.parse? Cmd _ "i0") = some (Cmd.info '0') := by native_decide
theorem parse_i1 : (@Parse.parse? Cmd _ "i1") = some (Cmd.info '1') := by native_decide
theorem parse_i2 : (@Parse.parse? Cmd _ "i2") = some (Cmd.info '2') := by native_decide
theorem parse_i3 : (@Parse.parse? Cmd _ "i3") = some (Cmd.info '3') := by native_decide
-- 2-char obj+verb takes priority over ArgCmd prefix (prevents "s~" → colJump "~" bug)
theorem parse_s_ent : (@Parse.parse? Cmd _ "s~") = some (Cmd.stk '~') := by native_decide
theorem parse_s_dec : (@Parse.parse? Cmd _ "s<") = some (Cmd.stk '<') := by native_decide
-- Argument command parse (all 9 arg variants — unified as flat Cmd with arg field)
theorem parse_split   : (@Parse.parse? Cmd _ ":-")          = some { obj := 'c', verb := ':', arg := "-" }          := by native_decide
theorem parse_derive  : (@Parse.parse? Cmd _ "=d = x * 2")  = some { obj := 'c', verb := '=', arg := "d = x * 2" } := by native_decide
theorem parse_filter  : (@Parse.parse? Cmd _ "\\Bid > 100") = some { obj := 'r', verb := '\\', arg := "Bid > 100" } := by native_decide
theorem parse_search  : (@Parse.parse? Cmd _ "/NYSE")        = some { obj := 'r', verb := '/', arg := "NYSE" }      := by native_decide
theorem parse_colJump : (@Parse.parse? Cmd _ "gExchange")    = some { obj := 'c', verb := '/', arg := "Exchange" }  := by native_decide
theorem parse_export  : (@Parse.parse? Cmd _ "ecsv")         = some { obj := 'e', verb := '~', arg := "csv" }       := by native_decide
theorem parse_sessSave: (@Parse.parse? Cmd _ "Wmysess")      = some { obj := 'W', verb := '~', arg := "mysess" }    := by native_decide
theorem parse_sessLoad: (@Parse.parse? Cmd _ "Lmysess")      = some { obj := 'L', verb := '~', arg := "mysess" }    := by native_decide
theorem parse_join    : (@Parse.parse? Cmd _ "J0")           = some { obj := 'J', verb := '~', arg := "0" }         := by native_decide
theorem parse_M0    : (@Parse.parse? Cmd _ "M0") = some (Cmd.metaV '0') := by native_decide
theorem parse_M1    : (@Parse.parse? Cmd _ "M1") = some (Cmd.metaV '1') := by native_decide
-- Stk verbs: quit/transpose/diff/join reuse stk obj
theorem parse_sd : (@Parse.parse? Cmd _ "s-") = some (Cmd.stk '-')      := by native_decide
theorem parse_sq : (@Parse.parse? Cmd _ "s{") = some (Cmd.stk '{')      := by native_decide
theorem parse_s1 : (@Parse.parse? Cmd _ "s1") = some (Cmd.stk '1')  := by native_decide
theorem parse_s2 : (@Parse.parse? Cmd _ "s2") = some (Cmd.stk '2')  := by native_decide
-- Fzf menu reuses col obj
theorem parse_cc : (@Parse.parse? Cmd _ "c+") = some (Cmd.col '+')       := by native_decide

end CmdRoundTripTests

/-! ## parseKeys Tests -/

section ParseKeysTests

-- From screen tests using <ret>, <C-d>, <C-u> in key sequences
#guard parseKeys "<ret>" == "\r"
#guard parseKeys "<C-d>" == "\x04"
#guard parseKeys "<C-u>" == "\x15"
#guard parseKeys "<esc>" == "\x1b"
#guard parseKeys "jjj<ret>" == "jjj\r"
#guard parseKeys "<C-d><C-u>" == "\x04\x15"
#guard parseKeys "abc" == "abc"
#guard parseKeys "<S-left>" == "\x11"
#guard parseKeys "<S-right>" == "\x12"
#guard parseKeys "!l!<S-left>" == "!l!\x11"

end ParseKeysTests

/-! ## NavState Theorems (derived from screen keystroke sequences) -/

section NavTheorems

-- Theorem 2: row.inc then row.dec is identity (from test_nav_up: "jk returns to row 0")
theorem nav_row_inverse :
    (do let n1 ← NavState.exec "nav.rowInc" testNav 1
        let n2 ← NavState.exec "nav.rowDec" n1 1
        pure n2.row.cur.val) = some 0 := by
  native_decide

-- Theorem 3: col.inc then col.dec is identity (from test_nav_left: "lh returns to col 0")
theorem nav_col_inverse :
    (do let n1 ← NavState.exec "nav.colInc" testNav 1
        let n2 ← NavState.exec "nav.colDec" n1 1
        pure n2.col.cur.val) = some 0 := by
  native_decide

-- Theorem 4: group toggle twice returns to empty (from test_key_remove: "!! removes key")
theorem grp_toggle_inverse :
    (do let n1 ← NavState.exec "nav.colGrp" testNav 1
        let n2 ← NavState.exec "nav.colGrp" n1 1
        pure n2.grp) = some #[] := by
  native_decide

-- Theorem 5: hidden toggle twice returns to empty (from test_hide_unhide: "HH unhides")
theorem hidden_toggle_inverse :
    (do let n1 ← NavState.exec "nav.colHide" testNav 1
        let n2 ← NavState.exec "nav.colHide" n1 1
        pure n2.hidden) = some #[] := by
  native_decide

-- Theorem 6: TjT accumulates selections [0, 1] (from test_multi_select)
theorem sel_accumulation :
    (do let n1 ← NavState.exec "nav.rowSel" testNav 1
        let n2 ← NavState.exec "nav.rowInc" n1 1
        let n3 ← NavState.exec "nav.rowSel" n2 1
        pure n3.row.sels) = some #[0, 1] := by
  native_decide

-- l! groups c1 (from test_key_reorder: "Key col moves to front")
theorem nav_grp_col :
    (do let n1 ← NavState.exec "nav.colInc" testNav 1
        let n2 ← NavState.exec "nav.colGrp" n1 1
        pure n2.grp) = some #["c1"] := by
  native_decide

-- l! → dispOrder puts c1 first (from test_key_reorder)
theorem nav_disp_grp_first :
    (do let n1 ← NavState.exec "nav.colInc" testNav 1
        let n2 ← NavState.exec "nav.colGrp" n1 1
        pure (n2.dispIdxs.getD 0 999)) = some 1 := by
  native_decide

-- ~ hides current column (from test_hide_col)
theorem nav_hide :
    (do let n1 ← NavState.exec "nav.colHide" testNav 1
        pure n1.hidden) = some #["c0"] := by
  native_decide

end NavTheorems

/-! ## Cell.toPrql Tests -/

section CellPrqlTests

#guard Cell.toPrql .null == "null"
#guard Cell.toPrql (.int 42) == "42"
#guard Cell.toPrql (.str "hello") == "'hello'"
#guard Cell.toPrql (.str "it's") == "'it''s'"
#guard Cell.toPrql (.bool true) == "true"
#guard Cell.toPrql (.bool false) == "false"

end CellPrqlTests

/-! ## buildFilterPrql Tests (derived from filter screen tests) -/

section BuildFilterTests

-- Single selection: exact match (from test_filter_parquet_full_db)
#guard buildFilterPrql "sym" #["AAPL", "GOOG", "MSFT"] "AAPL\nAAPL" false ==
  "sym == 'AAPL'"

-- Numeric single selection: no quotes
#guard buildFilterPrql "age" #["18", "25", "80"] "25\n25" true ==
  "age == 25"

-- Multiple selections: OR expression
#guard buildFilterPrql "sym" #["AAPL", "GOOG"] "AAPL\nAAPL\nGOOG" false ==
  "(sym == 'AAPL' || sym == 'GOOG')"

-- Custom query (no matching hint): passthrough
#guard buildFilterPrql "age" #["18", "25"] "age > 20\n" true == "age > 20"

-- Empty input: empty result
#guard buildFilterPrql "col" #["a", "b"] "" false == ""

end BuildFilterTests

/-! ## Folder Helper Tests -/

-- Note: isDuckDB, isSQLite, isDataFile are now config-driven (IO) and tested via integration tests

section FolderHelperTests
open Tc.Folder
-- | .gz files recognized as data files via inner extension
#guard (findFormat "data.csv.gz").isSome
#guard (findFormat "data.parquet.gz").isSome
#guard (findFormat "data.json.gz").isSome
#guard (findFormat "data.xlsx.gz").isSome
#guard (findFormat "data.txt.gz").isNone
#guard (findFormat "data.csv").isSome
-- | isDataFile delegates to findFormat
#guard isDataFile "foo.csv.gz"
#guard isDataFile "foo.parquet.gz"
#guard !isDataFile "foo.txt.gz"
-- | isTxtFile: matches .txt and .txt.gz
#guard isTxtFile "data.txt"
#guard isTxtFile "data.txt.gz"
#guard !isTxtFile "data.csv"
#guard !isTxtFile "data.csv.gz"
#guard !isTxtFile "data.parquet"
end FolderHelperTests

/-! ## View.fromTbl Theorems -/

section ViewFromTblTests

-- Theorem 7: valid table returns some
theorem fromTbl_some :
    (View.fromTbl (T := MockTable 5 3) mock53 "test").isSome = true := by
  native_decide

-- Theorem 8: empty table (0 rows) returns none
theorem fromTbl_none_rows :
    (View.fromTbl (T := MockTable 0 3) ⟨#["a","b","c"]⟩ "test").isNone = true := by
  native_decide

-- Zero-col table returns none
#guard (View.fromTbl (MockTable.mk (nRows := 5) (nCols := 0) #[]) "test").isNone

-- fromTbl sets correct path
#guard (View.fromTbl mock53 "data/basic.csv").map (·.path) == some "data/basic.csv"

-- fromTbl default vkind is .tbl
#guard (View.fromTbl mock53 "test").map (·.vkind) == some .tbl

-- fromTbl with initial col
#guard (View.fromTbl mock53 "test" (col := 2)).map (·.nav.col.cur.val) == some 2

-- fromTbl with initial grp
#guard (View.fromTbl mock53 "test" (grp := #["c0"])).map (·.nav.grp) == some #["c0"]

end ViewFromTblTests

/-! ## colsToText Tests -/

section ColsToTextTests

#guard colsToText #["a", "b"] #[Column.ints #[1, 2], Column.strs #["x", "y"]] 2 ==
  "a\tb\n1\tx\n2\ty"

#guard colsToText #["x"] #[Column.ints #[10]] 1 == "x\n10"

-- Empty rows: header only
#guard colsToText #["a"] #[Column.ints #[]] 0 == "a"

end ColsToTextTests

/-! ## keepCols Tests -/

section KeepColsTests

#guard keepCols 3 #[1] #["a", "b", "c"] == #["a", "c"]
#guard keepCols 3 #[0, 2] #["a", "b", "c"] == #["b"]
#guard keepCols 3 #[] #["a", "b", "c"] == #["a", "b", "c"]

end KeepColsTests

/-! ## expand: empty placeholder removes preceding slash -/

section ExpandTests
open Tc.SourceConfig

-- empty {3+} after slash: "/tree/main/{3+}" → "/tree/main" not "/tree/main/"
#guard expand "curl -sf https://example.com/{1}/{2}/tree/main/{3+}"
  #[("1", "openai"), ("2", "gsm8k"), ("3+", "")] == "curl -sf https://example.com/openai/gsm8k/tree/main"

-- non-empty {3+} preserved normally
#guard expand "https://example.com/{1}/{2}/tree/{3+}"
  #[("1", "a"), ("2", "b"), ("3+", "sub/dir")] == "https://example.com/a/b/tree/sub/dir"

-- empty {1} in middle: no double slash
#guard expand "a/{1}/b" #[("1", "")] == "a/b"

end ExpandTests

/-! ## NavState Tests (moved from Test.lean) -/

section NavTests2

-- 5 rows, 3 cols — uses mock53/testNav already defined above

-- | j (row.inc) moves cursor from 0 to 1
#guard (NavState.exec "nav.rowInc" testNav 1).map (·.row.cur.val) == some 1

-- | k (row.dec) at row 0 stays at 0 (clamped)
#guard (NavState.exec "nav.rowDec" testNav 1).map (·.row.cur.val) == some 0

-- | l (col.inc) moves cursor from 0 to 1
#guard (NavState.exec "nav.colInc" testNav 1).map (·.col.cur.val) == some 1

-- | h (col.dec) at col 0 stays at 0 (clamped)
#guard (NavState.exec "nav.colDec" testNav 1).map (·.col.cur.val) == some 0

-- | } (row.rbc = bottom) goes to last row (4)
#guard (NavState.exec "nav.rowBot" testNav 1).map (·.row.cur.val) == some 4

-- | { (row.lbc = top) goes to first row (0)
def navAtRow4 : NavState 5 3 (MockTable 5 3) :=
  match NavState.exec "nav.rowBot" testNav 1 with
  | some n => n
  | none => testNav
#guard (NavState.exec "nav.rowTop" navAtRow4 1).map (·.row.cur.val) == some 0

-- | col } (col.rbc = last) goes to last col (2)
#guard (NavState.exec "nav.colLast" testNav 1).map (·.col.cur.val) == some 2

-- | col { (col.lbc = first) goes to first col (0)
def navAtCol2 : NavState 5 3 (MockTable 5 3) :=
  match NavState.exec "nav.colLast" testNav 1 with
  | some n => n
  | none => testNav
#guard (NavState.exec "nav.colFirst" navAtCol2 1).map (·.col.cur.val) == some 0

-- | ] (row.rbr = page down) with page size 2 moves from 0 to 2
#guard (NavState.exec "nav.rowPgDn" testNav 2).map (·.row.cur.val) == some 2

-- | [ (row.lbr = page up) at row 0 stays at 0
#guard (NavState.exec "nav.rowPgUp" testNav 2).map (·.row.cur.val) == some 0

-- | T (row.ent) toggles row selection
#guard (NavState.exec "nav.rowSel" testNav 1).map (·.row.sels) == some #[0]

-- | T twice removes selection
def navWithSel : NavState 5 3 (MockTable 5 3) :=
  match NavState.exec "nav.rowSel" testNav 1 with
  | some n => n
  | none => testNav
#guard (NavState.exec "nav.rowSel" navWithSel 1).map (·.row.sels) == some #[]

-- | ! (col.ent) toggles group
#guard (NavState.exec "nav.colGrp" testNav 1).map (·.grp) == some #["c0"]

-- | colShift (c-) on non-keyed column is no-op
#guard (NavState.exec "nav.colShiftL" testNav 1).isNone

-- | colShift on keyed column swaps grp order
-- !l! → grp=["c0","c1"], then shift-left (c-) on c1 (cursor at disp pos 1) → grp=["c1","c0"]
def navGrp2 : NavState 5 3 (MockTable 5 3) :=
  match do let n1 ← NavState.exec "nav.colGrp" testNav 1
           let n2 ← NavState.exec "nav.colInc" n1 1
           NavState.exec "nav.colGrp" n2 1 with
  | some n => n | none => testNav
#guard navGrp2.grp == #["c0", "c1"]
#guard (NavState.exec "nav.colShiftL" navGrp2 1).map (·.grp) == some #["c1", "c0"]

-- | colShift at boundary is no-op (first key col can't shift left)
def navGrp2AtFirst : NavState 5 3 (MockTable 5 3) :=
  match NavState.exec "nav.colDec" navGrp2 1 with
  | some n => n | none => navGrp2
#guard (NavState.exec "nav.colShiftL" navGrp2AtFirst 1).isNone

-- | update returns Effect.none for nav commands
#guard (NavState.exec "nav.rowInc" testNav 1).isSome

end NavTests2

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

/-! ## Fin.clamp Tests -/

section ClampTests

#guard (⟨0, by decide⟩ : Fin 5).clamp 10 == ⟨4, by decide⟩
#guard (⟨4, by decide⟩ : Fin 5).clamp (-10) == ⟨0, by decide⟩
#guard (⟨2, by decide⟩ : Fin 5).clamp 1 == ⟨3, by decide⟩
#guard (⟨2, by decide⟩ : Fin 5).clamp (-1) == ⟨1, by decide⟩

end ClampTests

/-! ## Info.State Tests -/

section InfoTests2

def infoOff : UI.Info.State := { vis := false }
def infoOn : UI.Info.State := { vis := true }

-- | Default is on
#guard ({} : UI.Info.State).vis == true

-- | I toggles info visibility
#guard (UI.Info.State.update infoOff "infoTog").map (·.1.vis) == some true
#guard (UI.Info.State.update infoOn "infoTog").map (·.1.vis) == some false

-- | info.inc/dec now handled by AppState (not Info.State), returns none here
#guard (UI.Info.State.update infoOff "precInc").isNone
#guard (UI.Info.State.update infoOn "precDec").isNone

-- | Unhandled returns none
#guard (UI.Info.State.update infoOff "nav.rowInc").isNone

-- | Info update returns Effect.none
#guard (UI.Info.State.update infoOff "infoTog").map (·.2) == some .none

end InfoTests2

/-! ## dispOrder Tests -/

section DispOrderTests

-- | Empty group: order unchanged
#guard dispOrder #[] #["a", "b", "c"] == #[0, 1, 2]

-- | Group first col: moves to front
#guard dispOrder #["b"] #["a", "b", "c"] == #[1, 0, 2]

-- | Group multiple: group columns come first (in grp array order)
#guard dispOrder #["c", "a"] #["a", "b", "c"] == #[2, 0, 1]

-- | Group non-existent: ignored
#guard dispOrder #["x"] #["a", "b", "c"] == #[0, 1, 2]

-- | Group order reversed: display order follows grp array
#guard dispOrder #["a", "c"] #["a", "b", "c"] == #[0, 2, 1]

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

-- | S3 prefix recognized
#guard "s3://bucket/path".startsWith "s3://" == true
#guard "s3://my-bucket".startsWith "s3://" == true

-- | Non-S3 paths rejected
#guard "/local/path".startsWith "s3://" == false
#guard "http://example.com".startsWith "s3://" == false
#guard "".startsWith "s3://" == false

-- | parent strips last component (minParts=3 for S3)
#guard Remote.parent "s3://bucket/a/b/" 3 == some "s3://bucket/a/"
#guard Remote.parent "s3://bucket/a/" 3 == some "s3://bucket/"

-- | parent returns none at bucket root
#guard Remote.parent "s3://bucket/" 3 == none

-- | parent without trailing slash
#guard Remote.parent "s3://bucket/a/b" 3 == some "s3://bucket/a/"

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

-- | HF parent via Remote.parent (minParts=5 for hf://datasets/user/ds)
#guard Remote.parent "hf://datasets/user/ds/a/b/" 5 == some "hf://datasets/user/ds/a/"
#guard Remote.parent "hf://datasets/user/ds/a/" 5 == some "hf://datasets/user/ds/"
#guard Remote.parent "hf://datasets/user/ds/" 5 == none

end HFTests

/-! ## FTP Path Helper Tests -/

section FTPTests
open Tc.SourceConfig

-- | FTP parent via Remote.parent (minParts=3: ftp://host is root)
#guard Remote.parent "ftp://ftp.nyse.com/a/b/" 3 == some "ftp://ftp.nyse.com/a/"
#guard Remote.parent "ftp://ftp.nyse.com/a/" 3 == some "ftp://ftp.nyse.com/"
#guard Remote.parent "ftp://ftp.nyse.com/" 3 == none

-- | FTP pathParts strips prefix (names are raw, not URL-encoded)
#guard pathParts "ftp://" "ftp://ftp.nyse.com/Historical Data Samples" == #["ftp.nyse.com", "Historical Data Samples"]
#guard pathParts "ftp://" "ftp://ftp.nyse.com/" == #["ftp.nyse.com"]
#guard pathParts "ftp://" "ftp://ftp.nyse.com" == #["ftp.nyse.com"]

-- | FTP join builds correct paths
#guard Remote.join "ftp://ftp.nyse.com/" "DAILY TAQ/" == "ftp://ftp.nyse.com/DAILY TAQ/"
#guard Remote.join "ftp://ftp.nyse.com/a/" "file.csv" == "ftp://ftp.nyse.com/a/file.csv"

-- | FTP dispName extracts last component
#guard Remote.dispName "ftp://ftp.nyse.com/Historical Data" == "Historical Data"
#guard Remote.dispName "ftp://ftp.nyse.com/" == "ftp.nyse.com"

-- | URL encode applied at command time, not parse time
#guard Tc.Ftp.urlEncode "Historical Data Samples" == "Historical%20Data%20Samples"
#guard Tc.Ftp.urlEncode "file.csv" == "file.csv"
#guard Tc.Ftp.urlEncode "a b/c d" == "a%20b/c%20d"

-- | urlEncodeUrl encodes path segments only, not protocol/host
#guard Tc.Ftp.urlEncodeUrl "ftp://" "ftp://ftp.nyse.com/a b/c d/" == "ftp://ftp.nyse.com/a%20b/c%20d/"
#guard Tc.Ftp.urlEncodeUrl "ftp://" "ftp://ftp.nyse.com/" == "ftp://ftp.nyse.com/"
#guard Tc.Ftp.urlEncodeUrl "ftp://" "ftp://ftp.nyse.com/file.csv" == "ftp://ftp.nyse.com/file.csv"

end FTPTests

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

/-! ## Sort Behavior Tests -/

section SortTests

-- Group exclusion logic
#guard (#[] ++ #[(0:Nat)]).filter (!#[(0:Nat)].contains ·) == #[]
#guard (#[(0:Nat)] ++ #[1]).filter (!#[(0:Nat)].contains ·) == #[1]
#guard (#[(0:Nat), 2] ++ #[1]).filter (!#[(0:Nat), 3].contains ·) == #[2, 1]

-- Deduplication
def dedup (arr : Array Nat) : Array Nat :=
  arr.foldl (init := #[]) fun acc c => if acc.contains c then acc else acc.push c
#guard dedup (#[(0:Nat), 1] ++ #[1]) == #[0, 1]
#guard dedup (#[(2:Nat), 0] ++ #[2]) == #[2, 0]

end SortTests

/-! ## pathParts Tests -/

section PathPartsTests
open Tc.SourceConfig

-- | S3 path splits into bucket + key components
#guard pathParts "s3://" "s3://bucket/a/b" == #["bucket", "a", "b"]

-- | S3 path with trailing slash
#guard pathParts "s3://" "s3://bucket/a/" == #["bucket", "a"]

-- | S3 bucket root
#guard pathParts "s3://" "s3://bucket" == #["bucket"]

-- | HF path splits into components
#guard pathParts "hf://datasets/" "hf://datasets/openai/gsm8k/data" == #["openai", "gsm8k", "data"]

-- | Empty remainder returns empty
#guard pathParts "s3://" "s3://" == #[]

-- | No prefix returns full path parts
#guard pathParts "" "a/b/c" == #["a", "b", "c"]

end PathPartsTests

/-! ## escSql Tests -/

section EscSqlTests

-- | Single quote escaped
#guard escSql "it's" == "it''s"

-- | Multiple quotes
#guard escSql "it's Bob's" == "it''s Bob''s"

-- | No quotes unchanged
#guard escSql "hello" == "hello"

-- | Empty string unchanged
#guard escSql "" == ""

end EscSqlTests

/-! ## clamp / adjOff Tests -/

section ClampTests2

-- | clamp within range
#guard Tc.clamp 3 0 10 == 3

-- | clamp below range
#guard Tc.clamp 0 5 10 == 5

-- | clamp above range (hi-1)
#guard Tc.clamp 15 0 10 == 9

-- | clamp degenerate range (hi ≤ lo)
#guard Tc.clamp 5 10 10 == 10

-- | adjOff keeps cursor visible
#guard Tc.adjOff 5 0 10 == 0
#guard Tc.adjOff 15 0 10 == 6
#guard Tc.adjOff 0 5 10 == 0

end ClampTests2

/-! ## Agg.short Tests -/

section AggTests

#guard Agg.short .count == "count"
#guard Agg.short .sum == "sum"
#guard Agg.short .avg == "avg"
#guard Agg.short .min == "min"
#guard Agg.short .max == "max"
#guard Agg.short .stddev == "stddev"
#guard Agg.short .dist == "dist"

end AggTests

/-! ## Cell.toRaw Tests -/

section CellToRawTests

#guard Cell.toRaw .null == ""
#guard Cell.toRaw (.int 42) == "42"
#guard Cell.toRaw (.str "hello") == "hello"
#guard Cell.toRaw (.bool true) == "true"
#guard Cell.toRaw (.bool false) == "false"

end CellToRawTests

/-! ## Array.idxOf? Tests -/

section IdxOfTests

#guard (#["a", "b", "c"]).idxOf? "b" == some 1
#guard (#["a", "b", "c"]).idxOf? "d" == none
#guard (#[] : Array Nat).idxOf? 1 == none
#guard (#[10, 20, 30]).idxOf? 10 == some 0
#guard (#[10, 20, 30]).idxOf? 30 == some 2

end IdxOfTests

/-! ## Session Serialization Round-Trip Tests
    Verify opToJson/parseOp and vkindToJson/parseVkind are inverses
    on the pure JSON layer (no IO). -/

section SessionRoundTrip

open Tc.Session

-- | Helper: serialize Op to JSON string, parse back to JVal, then parseOp
private def opRoundTrip (op : Op) : Option Op :=
  let json := opToJson op
  let (jval, _) := parseVal json 0
  parseOp jval

-- | Helper: serialize ViewKind to JSON string, parse back to JVal, then parseVkind
private def vkindRoundTrip (vk : ViewKind) : ViewKind :=
  let json := vkindToJson vk
  let (jval, _) := parseVal json 0
  parseVkind jval

-- Op.filter round-trips
#guard opRoundTrip (.filter "age > 20") == some (.filter "age > 20")
#guard opRoundTrip (.filter "name == 'O\\'Brien'") == some (.filter "name == 'O\\'Brien'")
#guard opRoundTrip (.filter "") == some (.filter "")

-- Op.sort round-trips
#guard opRoundTrip (.sort #[("col1", true), ("col2", false)]) == some (.sort #[("col1", true), ("col2", false)])
#guard opRoundTrip (.sort #[]) == some (.sort #[])

-- Op.sel round-trips
#guard opRoundTrip (.sel #["a", "b", "c"]) == some (.sel #["a", "b", "c"])
#guard opRoundTrip (.sel #[]) == some (.sel #[])

-- Op.derive round-trips
#guard opRoundTrip (.derive #[("pct", "amount / total")]) == some (.derive #[("pct", "amount / total")])

-- Op.take round-trips
#guard opRoundTrip (.take 100) == some (.take 100)
#guard opRoundTrip (.take 0) == some (.take 0)

-- Op.group round-trips
#guard opRoundTrip (.group #["region"] #[(.count, "n", "id"), (.sum, "total", "amount")])
  == some (.group #["region"] #[(.count, "n", "id"), (.sum, "total", "amount")])

-- ViewKind.tbl round-trips
#guard vkindRoundTrip .tbl == .tbl

-- ViewKind.colMeta round-trips
#guard vkindRoundTrip .colMeta == .colMeta

-- ViewKind.freqV round-trips
#guard vkindRoundTrip (.freqV #["a", "b"] 42) == .freqV #["a", "b"] 42

-- ViewKind.fld round-trips
#guard vkindRoundTrip (.fld "/tmp/data" 3) == .fld "/tmp/data" 3
#guard vkindRoundTrip (.fld "s3://bucket/key" 1) == .fld "s3://bucket/key" 1

-- Op.exclude round-trips
#guard opRoundTrip (.exclude #["col1", "col2"]) == some (.exclude #["col1", "col2"])

-- JSON escaping round-trips for strings with special chars
#guard opRoundTrip (.filter "col == \"hello\"") == some (.filter "col == \"hello\"")
#guard opRoundTrip (.filter "a\nb") == some (.filter "a\nb")
#guard opRoundTrip (.filter "a\tb") == some (.filter "a\tb")

-- autoName derives name from tab
-- (can't test without AdbcTable, but sanitize is testable)
#guard Tc.Session.sanitize "basic.csv" == "basic.csv"
#guard Tc.Session.sanitize "../../../etc/passwd" == "......etcpasswd"
#guard Tc.Session.sanitize "hello world!" == "helloworld"
#guard Tc.Session.sanitize "" == ""

end SessionRoundTrip

end PureTest2
