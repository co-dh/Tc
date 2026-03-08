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

-- Navigation keys (from test_nav_down/up/right/left)
theorem key_j : evToCmd (charToEvent 'j') .tbl = some (.row .inc) := by native_decide
theorem key_k : evToCmd (charToEvent 'k') .tbl = some (.row .dec) := by native_decide
theorem key_l : evToCmd (charToEvent 'l') .tbl = some (.col .inc) := by native_decide
theorem key_h : evToCmd (charToEvent 'h') .tbl = some (.col .dec) := by native_decide

-- Action keys (from test_key_toggle, test_row_select, test_hide_col, test_sort_asc/desc)
theorem key_bang  : evToCmd (charToEvent '!') .tbl = some (.grp .ent)    := by native_decide
theorem key_T     : evToCmd (charToEvent 'T') .tbl = some (.rowSel .ent) := by native_decide
theorem key_H     : evToCmd (charToEvent 'H') .tbl = some (.colSel .dup) := by native_decide
theorem key_lbr   : evToCmd (charToEvent '[') .tbl = some (.colSel .inc) := by native_decide
theorem key_rbr   : evToCmd (charToEvent ']') .tbl = some (.colSel .dec) := by native_decide

-- View keys (from test_meta_shows, test_freq_shows, test_folder_D)
theorem key_M : evToCmd (charToEvent 'M') .tbl = some (.metaV .dup) := by native_decide
theorem key_F : evToCmd (charToEvent 'F') .tbl = some (.freq .dup)  := by native_decide
theorem key_D : evToCmd (charToEvent 'D') .tbl = some (.fld .dup)   := by native_decide

-- Stack keys (from test_meta_quit, test_stack_swap)
theorem key_q : evToCmd (charToEvent 'q') .tbl = some (.stk .dec) := by native_decide
theorem key_S : evToCmd (charToEvent 'S') .tbl = some (.stk .ent) := by native_decide

-- Info, search, filter keys (from test_info, test_search_jump/next/prev, test_col_search)
theorem key_I     : evToCmd (charToEvent 'I')  .tbl = some (.info .ent)   := by native_decide
theorem key_slash : evToCmd (charToEvent '/')  .tbl = some (.rowSel .inc) := by native_decide
theorem key_n     : evToCmd (charToEvent 'n')  .tbl = some (.grp .inc)    := by native_decide
theorem key_N     : evToCmd (charToEvent 'N')  .tbl = some (.grp .dec)    := by native_decide
theorem key_s     : evToCmd (charToEvent 's')  .tbl = some (.col .ent)    := by native_decide
theorem key_bslash: evToCmd (charToEvent '\\') .tbl = some (.rowSel .dec) := by native_decide

-- Ctrl keys (from test_page_down/up)
theorem key_ctrlD : evToCmd (charToEvent '\x04') .tbl = some (.vPage .inc) := by native_decide
theorem key_ctrlU : evToCmd (charToEvent '\x15') .tbl = some (.vPage .dec) := by native_decide

-- Context-sensitive Enter (from test_freq_enter, test_meta_0_enter, test_folder_enter, test_enter_no_quit)
theorem enter_freq : evToCmd (charToEvent '\r') (.freqV #["a"] 10) = some (.freq .ent) := by native_decide
theorem enter_meta : evToCmd (charToEvent '\r') .colMeta = some (.metaV .ent)           := by native_decide
theorem enter_fld  : evToCmd (charToEvent '\r') (.fld "/tmp" 1) = some (.fld .ent)      := by native_decide
theorem enter_tbl  : evToCmd (charToEvent '\r') .tbl = none                             := by native_decide

end KeyMapTests

/-! ## View.update Tests (derived from screen tests) -/

section ViewUpdateTests

-- , (prec.dec) decreases precision (from test_prec_dec)
#guard (View.update testView (.prec .dec) 1).map (·.1.precAdj) == some (-1)

-- . (prec.inc) increases precision (from test_prec_inc)
#guard (View.update testView (.prec .inc) 1).map (·.1.precAdj) == some 1

-- prec returns Effect.none (no IO needed)
#guard (View.update testView (.prec .inc) 1).map (·.2) == some .none

-- width adjustment
#guard (View.update testView (.width .inc) 1).map (·.1.widthAdj) == some 1
#guard (View.update testView (.width .dec) 1).map (·.1.widthAdj) == some (-1)

-- [ (colSel.inc) returns sort effect with asc=true (from test_sort_asc)
#guard (View.update testView (.colSel .inc) 1).map (·.2) ==
  some (.query (.sort 0 #[] #[] true))

-- ] (colSel.dec) returns sort effect with asc=false (from test_sort_desc)
#guard (View.update testView (.colSel .dec) 1).map (·.2) ==
  some (.query (.sort 0 #[] #[] false))

-- Navigation delegates to NavState and returns Effect.none
-- (from test_nav_down: "j moves to row 1")
#guard (View.update testView (.row .inc) 1).map (·.1.nav.row.cur.val) == some 1
#guard (View.update testView (.row .inc) 1).map (·.2) == some .none

-- row.dec at 0 stays at 0 (from test_nav_up: "jk returns to row 0")
#guard (View.update testView (.row .dec) 1).map (·.1.nav.row.cur.val) == some 0

end ViewUpdateTests

/-! ## ViewStack.update Tests (derived from screen tests) -/

section ViewStackUpdateTests

-- S (stk.ent) swaps (from test_stack_swap: "S swaps/dups view")
-- On single-element stack, swap is identity
#guard (ViewStack.update testStack (.stk .ent)).map (·.1.hd.path) == some "data/test.csv"
#guard (ViewStack.update testStack (.stk .ent)).map (·.2) == some .none

-- stk.inc dups: pushes copy
#guard (ViewStack.update testStack (.stk .inc)).map (·.1.tl.length) == some 1
#guard (ViewStack.update testStack (.stk .inc)).map (·.2) == some .none

-- q (stk.dec) on empty stack → quit (from test_q_quit: "q on empty stack exits cleanly")
#guard (ViewStack.update testStack (.stk .dec)).map (·.2) == some .quit

-- q (stk.dec) with parent → pops (from test_meta_quit, test_freq_quit)
def twoStack : ViewStack (MockTable 5 3) := testStack.dup
#guard (ViewStack.update twoStack (.stk .dec)).map (·.1.tl.length) == some 0
#guard (ViewStack.update twoStack (.stk .dec)).map (·.2) == some .none

-- Unhandled commands return none
#guard (ViewStack.update testStack (.row .inc)).isNone

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
#guard (Tc.Filter.update testStack (.rowSel .inc)).map (·.2) == some (.fzf .row)

-- \ → fzf.filter (from test_filter_parquet_full_db: row filter)
#guard (Tc.Filter.update testStack (.rowSel .dec)).map (·.2) == some (.fzf .filter)

-- s → fzf.col (from test_col_search: column search)
#guard (Tc.Filter.update testStack (.col .ent)).map (·.2) == some (.fzf .col)

-- n → search.next (from test_search_next)
#guard (Tc.Filter.update testStack (.grp .inc)).map (·.2) == some (.search .next)

-- N → search.prev (from test_search_prev)
#guard (Tc.Filter.update testStack (.grp .dec)).map (·.2) == some (.search .prev)

-- Unhandled returns none
#guard (Tc.Filter.update testStack (.row .inc)).isNone

end FilterUpdateTests

/-! ## Cmd Parse/ToString Round-Trip Theorem -/

section CmdRoundTripTests

-- All Cmd constructors round-trip through toString/parse?
theorem cmd_roundtrip (c : Cmd) : @Parse.parse? Cmd _ (toString c) = some c := by
  cases c <;> rename_i v <;> cases v <;> native_decide

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

end ParseKeysTests

/-! ## NavState Theorems (derived from screen keystroke sequences) -/

section NavTheorems

-- Theorem 2: row.inc then row.dec is identity (from test_nav_up: "jk returns to row 0")
theorem nav_row_inverse :
    (do let n1 ← NavState.exec (.row .inc) testNav 1 1
        let n2 ← NavState.exec (.row .dec) n1 1 1
        pure n2.row.cur.val) = some 0 := by
  native_decide

-- Theorem 3: col.inc then col.dec is identity (from test_nav_left: "lh returns to col 0")
theorem nav_col_inverse :
    (do let n1 ← NavState.exec (.col .inc) testNav 1 1
        let n2 ← NavState.exec (.col .dec) n1 1 1
        pure n2.col.cur.val) = some 0 := by
  native_decide

-- Theorem 4: group toggle twice returns to empty (from test_key_remove: "!! removes key")
theorem grp_toggle_inverse :
    (do let n1 ← NavState.exec (.grp .ent) testNav 1 1
        let n2 ← NavState.exec (.grp .ent) n1 1 1
        pure n2.grp) = some #[] := by
  native_decide

-- Theorem 5: hidden toggle twice returns to empty (from test_hide_unhide: "HH unhides")
theorem hidden_toggle_inverse :
    (do let n1 ← NavState.exec (.colSel .dup) testNav 1 1
        let n2 ← NavState.exec (.colSel .dup) n1 1 1
        pure n2.hidden) = some #[] := by
  native_decide

-- Theorem 6: TjT accumulates selections [0, 1] (from test_multi_select)
theorem sel_accumulation :
    (do let n1 ← NavState.exec (.rowSel .ent) testNav 1 1
        let n2 ← NavState.exec (.row .inc) n1 1 1
        let n3 ← NavState.exec (.rowSel .ent) n2 1 1
        pure n3.row.sels) = some #[0, 1] := by
  native_decide

-- l! groups c1 (from test_key_reorder: "Key col moves to front")
theorem nav_grp_col :
    (do let n1 ← NavState.exec (.col .inc) testNav 1 1
        let n2 ← NavState.exec (.grp .ent) n1 1 1
        pure n2.grp) = some #["c1"] := by
  native_decide

-- l! → dispOrder puts c1 first (from test_key_reorder)
theorem nav_disp_grp_first :
    (do let n1 ← NavState.exec (.col .inc) testNav 1 1
        let n2 ← NavState.exec (.grp .ent) n1 1 1
        pure (n2.dispIdxs.getD 0 999)) = some 1 := by
  native_decide

-- H hides current column (from test_hide_col)
theorem nav_hide :
    (do let n1 ← NavState.exec (.colSel .dup) testNav 1 1
        pure n1.hidden) = some #["c0"] := by
  native_decide

end NavTheorems

/-! ## Cell.toPrql Tests -/

section CellPrqlTests

#guard Cell.toPrql .null == "null"
#guard Cell.toPrql (.int 42) == "42"
#guard Cell.toPrql (.str "hello") == "'hello'"
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

section FolderHelperTests

-- isDuckDB (from test_duckdb_list, test_duckdb_enter)
#guard Tc.Folder.isDuckDB "data/test.duckdb" == true
#guard Tc.Folder.isDuckDB "data/test.db" == true
#guard Tc.Folder.isDuckDB "data/test.csv" == false
#guard Tc.Folder.isDuckDB "data/test.parquet" == false

-- isDataFile (from test_folder_enter_symlink where enter dispatches on file type)
#guard Tc.Folder.isDataFile "test.csv" == true
#guard Tc.Folder.isDataFile "test.parquet" == true
#guard Tc.Folder.isDataFile "test.duckdb" == true
#guard Tc.Folder.isDataFile "test.txt" == false
#guard Tc.Folder.isDataFile "test.json" == false

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

end PureTest2
