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
import Tc.Key
import Tc.Fzf
import Tc.Plot

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

/-! ## evToKey Tests -/

section KeyMapTests

-- evToKey: terminal event → readable key string
theorem evToKey_j     : evToKey (charToEvent 'j') = "j"          := by native_decide
theorem evToKey_ret   : evToKey (charToEvent '\r') = "<ret>"      := by native_decide
theorem evToKey_bs    : evToKey (charToEvent '\x7f') = "<bs>"     := by native_decide
theorem evToKey_ctrlD : evToKey (charToEvent '\x04') = "<C-d>"    := by native_decide
theorem evToKey_ctrlU : evToKey (charToEvent '\x15') = "<C-u>"    := by native_decide
-- Arrow keys via charToEvent (synthetic codes) → normalized hjkl
theorem evToKey_down  : evToKey (charToEvent '\x1c') = "j"        := by native_decide
theorem evToKey_up    : evToKey (charToEvent '\x1d') = "k"        := by native_decide
theorem evToKey_right : evToKey (charToEvent '\x1e') = "l"        := by native_decide
theorem evToKey_left  : evToKey (charToEvent '\x1f') = "h"        := by native_decide
-- Shift+Arrow via charToEvent
theorem evToKey_sleft  : evToKey (charToEvent '\x11') = "<S-left>"  := by native_decide
theorem evToKey_sright : evToKey (charToEvent '\x12') = "<S-right>" := by native_decide
-- Direct terminal arrow events
theorem evToKey_arrow_down  : evToKey ⟨Term.eventKey, 0, Term.keyArrowDown, 0, 0, 0⟩ = "j"  := by native_decide
theorem evToKey_arrow_up    : evToKey ⟨Term.eventKey, 0, Term.keyArrowUp, 0, 0, 0⟩ = "k"    := by native_decide
theorem evToKey_arrow_right : evToKey ⟨Term.eventKey, 0, Term.keyArrowRight, 0, 0, 0⟩ = "l" := by native_decide
theorem evToKey_arrow_left  : evToKey ⟨Term.eventKey, 0, Term.keyArrowLeft, 0, 0, 0⟩ = "h"  := by native_decide
-- Direct shift+arrow events
theorem evToKey_shift_left  : evToKey ⟨Term.eventKey, Term.modShift, Term.keyArrowLeft, 0, 0, 0⟩ = "<S-left>"  := by native_decide
theorem evToKey_shift_right : evToKey ⟨Term.eventKey, Term.modShift, Term.keyArrowRight, 0, 0, 0⟩ = "<S-right>" := by native_decide

end KeyMapTests

/-! ## View.update Tests (derived from screen tests) -/

section ViewUpdateTests

-- [ (sort.asc) returns sort effect with asc=true (from test_sort_asc)
#guard (View.update testView .sortAsc 1).map (·.2) ==
  some (.sort 0 #[] #[] true)

-- ] (sort.desc) returns sort effect with asc=false (from test_sort_desc)
#guard (View.update testView .sortDesc 1).map (·.2) ==
  some (.sort 0 #[] #[] false)

-- Navigation delegates to NavState and returns Effect.none
-- (from test_nav_down: "j moves to row 1")
#guard (View.update testView .rowInc 1).map (·.1.nav.row.cur.val) == some 1
#guard (View.update testView .rowInc 1).map (·.2) == some .none

-- row.dec at 0 stays at 0 (from test_nav_up: "jk returns to row 0")
#guard (View.update testView .rowDec 1).map (·.1.nav.row.cur.val) == some 0

end ViewUpdateTests

/-! ## Menu Alignment Tests -/

-- parseFlatSel extracts handler from aligned "handler | ctx | key | label" format
#guard Tc.Fzf.parseFlatSel "plot.area    | cg |   | Plot: area chart" == some "plot.area"
#guard Tc.Fzf.parseFlatSel "sort.asc     | c  | [ | Sort ascending" == some "sort.asc"
#guard Tc.Fzf.parseFlatSel "" == none

/-! ## Plot rScript Tests -/

private def has (s needle : String) : Bool := (s.splitOn needle).length > 1

-- rScript includes centered title when provided
#guard has (Tc.Plot.rScript "d.dat" "p.png" .density "" "Close" false "" false "" .other "density of Close") "ggtitle('density of Close')"
#guard has (Tc.Plot.rScript "d.dat" "p.png" .density "" "Close" false "" false "" .other "density of Close") "hjust = 0.5"
-- rScript includes centered title for multi-col plot
#guard has (Tc.Plot.rScript "d.dat" "p.png" .line "Date" "Price" true "Ticker" false "" .other "line: Price vs Date by Ticker") "ggtitle('line: Price vs Date by Ticker')"
-- rScript omits ggtitle when title is empty
#guard !(has (Tc.Plot.rScript "d.dat" "p.png" .line "Date" "Price" false "" false "" .other) "ggtitle")

/-! ## ViewStack.update Tests (derived from screen tests) -/

section ViewStackUpdateTests

-- S swaps (from test_stack_swap: "S swaps/dups view")
-- On single-element stack, swap is identity
#guard (ViewStack.update testStack .stkSwap).map (·.1.hd.path) == some "data/test.csv"
#guard (ViewStack.update testStack .stkSwap).map (·.2) == some .none

-- stk.dup dups: pushes copy
#guard (ViewStack.update testStack .stkDup).map (·.1.tl.length) == some 1
#guard (ViewStack.update testStack .stkDup).map (·.2) == some .none

-- q (stk.pop) on empty stack → quit (from test_q_quit: "q on empty stack exits cleanly")
#guard (ViewStack.update testStack .stkPop).map (·.2) == some .quit

-- q (stk.pop) with parent → pops (from test_meta_quit, test_freq_quit)
def twoStack : ViewStack (MockTable 5 3) := testStack.dup
#guard (ViewStack.update twoStack .stkPop).map (·.1.tl.length) == some 0
#guard (ViewStack.update twoStack .stkPop).map (·.2) == some .none

-- Unhandled commands return none
#guard (ViewStack.update testStack .rowInc).isNone

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
