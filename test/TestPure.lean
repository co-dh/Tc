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
import Tc.Util
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
    Here we test the pure key→handler pipeline directly. -/

section KeyMapTests

-- Navigation keys via evToHandler (hjkl, arrows — not in KeyMap.char)
theorem key_j : evToHandler (charToEvent 'j') .tbl = some "nav.rowInc" := by native_decide
theorem key_k : evToHandler (charToEvent 'k') .tbl = some "nav.rowDec" := by native_decide
theorem key_l : evToHandler (charToEvent 'l') .tbl = some "nav.colInc" := by native_decide
theorem key_h : evToHandler (charToEvent 'h') .tbl = some "nav.colDec" := by native_decide

-- Single-key shortcuts via KeyMap.char (centralized data table, handler names)
theorem key_bang  : lookup KeyMap.char '!' = some "nav.colGrp"        := by native_decide
theorem key_T     : lookup KeyMap.char 'T' = some "nav.rowSel"       := by native_decide
theorem key_lbr   : lookup KeyMap.char '[' = some "sort.asc"         := by native_decide
theorem key_rbr   : lookup KeyMap.char ']' = some "sort.desc"        := by native_decide
theorem key_q     : lookup KeyMap.char 'q' = some "stk.pop"          := by native_decide
theorem key_space : lookup KeyMap.char ' ' = some "menu"             := by native_decide
theorem key_n     : lookup KeyMap.char 'n' = some "filter.searchNext" := by native_decide
theorem key_N     : lookup KeyMap.char 'N' = some "filter.searchPrev" := by native_decide
theorem key_lbrace: lookup KeyMap.char '{' = some "scrollUp"          := by native_decide
theorem key_rbrace: lookup KeyMap.char '}' = some "scrollDn"          := by native_decide
theorem key_S     : lookup KeyMap.char 'S' = some "stk.swap"         := by native_decide
theorem key_X     : lookup KeyMap.char 'X' = some "xpose"            := by native_decide
theorem key_d     : lookup KeyMap.char 'd' = some "diff"             := by native_decide
theorem key_I     : lookup KeyMap.char 'I' = some "infoTog"          := by native_decide
theorem key_M     : lookup KeyMap.char 'M' = some "meta.push"        := by native_decide
theorem key_F     : lookup KeyMap.char 'F' = some "freq.open"        := by native_decide
theorem key_D     : lookup KeyMap.char 'D' = some "folder.push"      := by native_decide
theorem key_H     : lookup KeyMap.char 'H' = some "nav.colHide"      := by native_decide

-- Ctrl keys via evToHandler (from test_page_down/up)
theorem key_ctrlD : evToHandler (charToEvent '\x04') .tbl = some "nav.rowPgDn" := by native_decide
theorem key_ctrlU : evToHandler (charToEvent '\x15') .tbl = some "nav.rowPgUp" := by native_decide

-- Context-sensitive Enter via evToHandler (from test_freq_enter, test_meta_0_enter, test_folder_enter)
theorem enter_freq : evToHandler (charToEvent '\r') (.freqV #["a"] 10) = some "freq.filter" := by native_decide
theorem enter_meta : evToHandler (charToEvent '\r') .colMeta = some "meta.setKey"           := by native_decide
theorem enter_fld  : evToHandler (charToEvent '\r') (.fld "/tmp" 1) = some "folder.enter"   := by native_decide
theorem enter_tbl  : evToHandler (charToEvent '\r') .tbl = none                             := by native_decide

-- Backspace in folder view → parent (from test_folder_backspace)
theorem bs_fld : evToHandler (charToEvent '\x7f') (.fld "/tmp" 1) = some "folder.parent"   := by native_decide
-- Backspace outside folder view → no action
theorem bs_tbl : evToHandler (charToEvent '\x7f') .tbl = none                               := by native_decide

-- Unmodified arrow keys → single-step nav, not page (bug: termbox2 tagged \x1b[A-D as shift)
theorem key_arrow_down : evToHandler ⟨Term.eventKey, 0, Term.keyArrowDown, 0, 0, 0⟩ .tbl
    = some "nav.rowInc" := by native_decide
theorem key_arrow_up : evToHandler ⟨Term.eventKey, 0, Term.keyArrowUp, 0, 0, 0⟩ .tbl
    = some "nav.rowDec" := by native_decide
theorem key_arrow_right : evToHandler ⟨Term.eventKey, 0, Term.keyArrowRight, 0, 0, 0⟩ .tbl
    = some "nav.colInc" := by native_decide
theorem key_arrow_left : evToHandler ⟨Term.eventKey, 0, Term.keyArrowLeft, 0, 0, 0⟩ .tbl
    = some "nav.colDec" := by native_decide

-- Synthetic arrow chars (\x1c-\x1f) via charToEvent → single-step nav
theorem key_synth_down : evToHandler (charToEvent '\x1c') .tbl = some "nav.rowInc" := by native_decide
theorem key_synth_up : evToHandler (charToEvent '\x1d') .tbl = some "nav.rowDec" := by native_decide
theorem key_synth_right : evToHandler (charToEvent '\x1e') .tbl = some "nav.colInc" := by native_decide
theorem key_synth_left : evToHandler (charToEvent '\x1f') .tbl = some "nav.colDec" := by native_decide

-- Shift+Arrow keys (from test_key_shift: reorder key columns)
-- Synthetic shift+arrow events: mod=4 (modShift), key=arrow code
theorem key_shift_left : evToHandler ⟨Term.eventKey, Term.modShift, Term.keyArrowLeft, 0, 0, 0⟩ .tbl
    = some "nav.colShiftL" := by native_decide
theorem key_shift_right : evToHandler ⟨Term.eventKey, Term.modShift, Term.keyArrowRight, 0, 0, 0⟩ .tbl
    = some "nav.colShiftR" := by native_decide

-- Test synthetic shift+arrow via charToEvent (\x11=S-left, \x12=S-right)
theorem key_synth_shift_left : evToHandler (charToEvent '\x11') .tbl = some "nav.colShiftL" := by native_decide
theorem key_synth_shift_right : evToHandler (charToEvent '\x12') .tbl = some "nav.colShiftR" := by native_decide

end KeyMapTests

/-! ## View.update Tests (derived from screen tests) -/

section ViewUpdateTests

-- [ (sort.asc) returns sort effect with asc=true (from test_sort_asc)
#guard (View.update testView "sort.asc" 1).map (·.2) ==
  some (.sort 0 #[] #[] true)

-- ] (sort.desc) returns sort effect with asc=false (from test_sort_desc)
#guard (View.update testView "sort.desc" 1).map (·.2) ==
  some (.sort 0 #[] #[] false)

-- Navigation delegates to NavState and returns Effect.none
-- (from test_nav_down: "j moves to row 1")
#guard (View.update testView "nav.rowInc" 1).map (·.1.nav.row.cur.val) == some 1
#guard (View.update testView "nav.rowInc" 1).map (·.2) == some .none

-- row.dec at 0 stays at 0 (from test_nav_up: "jk returns to row 0")
#guard (View.update testView "nav.rowDec" 1).map (·.1.nav.row.cur.val) == some 0

end ViewUpdateTests

/-! ## ViewStack.update Tests (derived from screen tests) -/

section ViewStackUpdateTests

-- S swaps (from test_stack_swap: "S swaps/dups view")
-- On single-element stack, swap is identity
#guard (ViewStack.update testStack "stk.swap").map (·.1.hd.path) == some "data/test.csv"
#guard (ViewStack.update testStack "stk.swap").map (·.2) == some .none

-- stk.dup dups: pushes copy
#guard (ViewStack.update testStack "stk.dup").map (·.1.tl.length) == some 1
#guard (ViewStack.update testStack "stk.dup").map (·.2) == some .none

-- q (stk.pop) on empty stack → quit (from test_q_quit: "q on empty stack exits cleanly")
#guard (ViewStack.update testStack "stk.pop").map (·.2) == some .quit

-- q (stk.pop) with parent → pops (from test_meta_quit, test_freq_quit)
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

/-! ## Filter.dispatch Tests (dispatch returns IO action directly, no Effect) -/

-- Filter.dispatch takes AdbcTable (does IO), so can't test with MockTable.
-- Coverage via screen tests (test_search_jump, test_filter_parquet_full_db, etc.).

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
