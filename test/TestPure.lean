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
import Tc.Term
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

def testNav : NavState (MockTable 5 3) :=
  { tbl := mock53, nRows := 5, nCols := 3 }

def testView : View (MockTable 5 3) := View.new testNav "data/test.csv"

def testStack : ViewStack (MockTable 5 3) := ⟨testView, []⟩

/-! ## evToKey Tests (real terminal events) -/

section KeyMapTests

-- Arrows: unmodified → hjkl
theorem evToKey_arrow_down  : evToKey ⟨Term.eventKey, 0, Term.keyArrowDown, 0, 0, 0⟩ = "j"  := by native_decide
theorem evToKey_arrow_up    : evToKey ⟨Term.eventKey, 0, Term.keyArrowUp, 0, 0, 0⟩ = "k"    := by native_decide
theorem evToKey_arrow_right : evToKey ⟨Term.eventKey, 0, Term.keyArrowRight, 0, 0, 0⟩ = "l" := by native_decide
theorem evToKey_arrow_left  : evToKey ⟨Term.eventKey, 0, Term.keyArrowLeft, 0, 0, 0⟩ = "h"  := by native_decide
-- Arrows: modified → <M-direction>
theorem evToKey_shift_left  : evToKey ⟨Term.eventKey, Term.modShift, Term.keyArrowLeft, 0, 0, 0⟩ = "<S-left>"  := by native_decide
theorem evToKey_shift_right : evToKey ⟨Term.eventKey, Term.modShift, Term.keyArrowRight, 0, 0, 0⟩ = "<S-right>" := by native_decide
theorem evToKey_ctrl_up     : evToKey ⟨Term.eventKey, Term.modCtrl, Term.keyArrowUp, 0, 0, 0⟩ = "<C-up>" := by native_decide
theorem evToKey_alt_down    : evToKey ⟨Term.eventKey, Term.modAlt, Term.keyArrowDown, 0, 0, 0⟩ = "<A-down>" := by native_decide
-- Special keys (termbox2 sets mod=ctrl for control chars like Enter/Bs/Esc)
theorem evToKey_enter : evToKey ⟨Term.eventKey, Term.modCtrl, Term.keyEnter, 0, 0, 0⟩ = "<ret>" := by native_decide
theorem evToKey_enter0 : evToKey ⟨Term.eventKey, 0, Term.keyEnter, 0, 0, 0⟩ = "<ret>" := by native_decide
theorem evToKey_bs    : evToKey ⟨Term.eventKey, Term.modCtrl, Term.keyBackspace, 0, 0, 0⟩ = "<bs>" := by native_decide
theorem evToKey_bs2   : evToKey ⟨Term.eventKey, 0, Term.keyBackspace2, 0, 0, 0⟩ = "<bs>" := by native_decide
-- Generic ctrl from control code
theorem evToKey_ctrl_d : evToKey ⟨Term.eventKey, 2, 4, 0, 0, 0⟩ = "<C-d>" := by native_decide
theorem evToKey_ctrl_u : evToKey ⟨Term.eventKey, 2, 21, 0, 0, 0⟩ = "<C-u>" := by native_decide
-- Printable char with modifier
theorem evToKey_alt_x : evToKey ⟨Term.eventKey, Term.modAlt, 0, 120, 0, 0⟩ = "<A-x>" := by native_decide

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
#guard (View.update testView .rowInc 1).map (·.1.nav.row.cur) == some 1
#guard (View.update testView .rowInc 1).map (·.2) == some .none

-- row.dec at 0 stays at 0 (from test_nav_up: "jk returns to row 0")
#guard (View.update testView .rowDec 1).map (·.1.nav.row.cur) == some 0

end ViewUpdateTests

/-! ## Menu Alignment Tests -/

-- parseFlatSel extracts handler from aligned "handler | ctx | key | label" format
#guard Tc.Fzf.parseFlatSel "plot.area    | cg |   | Plot: area chart" == some "plot.area"
#guard Tc.Fzf.parseFlatSel "sort.asc     | c  | [ | Sort ascending" == some "sort.asc"
#guard Tc.Fzf.parseFlatSel "" == none

/-! ## Plot downsampling: interval bar only shows when nr > maxPoints -/

-- Small dataset (100 rows) should NOT trigger downsampling (no ,/. bar)
#guard !(100 > Tc.Plot.maxPoints)
-- Large dataset should trigger downsampling
#guard (3000 > Tc.Plot.maxPoints)

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
  View.new { tbl := mock53, nRows := 5, nCols := 3 } "data/sample.parquet"
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

/-! ## tokenizeKeys Tests -/

section TokenizeKeysTests

-- Single chars become individual tokens
#guard tokenizeKeys "abc" == #["a", "b", "c"]
#guard tokenizeKeys "jjj" == #["j", "j", "j"]
-- Bracketed keys become single tokens
#guard tokenizeKeys "<ret>" == #["<ret>"]
#guard tokenizeKeys "<C-d>" == #["<C-d>"]
#guard tokenizeKeys "<C-u>" == #["<C-u>"]
#guard tokenizeKeys "<esc>" == #["<esc>"]
#guard tokenizeKeys "<S-left>" == #["<S-left>"]
#guard tokenizeKeys "<S-right>" == #["<S-right>"]
-- Mixed: chars + bracketed
#guard tokenizeKeys "jjj<ret>" == #["j", "j", "j", "<ret>"]
#guard tokenizeKeys "<C-d><C-u>" == #["<C-d>", "<C-u>"]
#guard tokenizeKeys "!l!<S-left>" == #["!", "l", "!", "<S-left>"]
-- Backslash is a regular printable char, no alias needed
#guard tokenizeKeys "\\" == #["\\"]
-- Wait tokens
#guard tokenizeKeys "<wait><wait>" == #["<wait>", "<wait>"]
-- Arrow aliases resolve to hjkl (matching evToKey normalization)
#guard tokenizeKeys "<down><up>" == #["j", "k"]
#guard tokenizeKeys "<right><left>" == #["l", "h"]
-- Edge cases: unclosed/empty brackets treated as literal chars
#guard tokenizeKeys "" == #[]
#guard tokenizeKeys "<" == #["<"]
#guard tokenizeKeys "a<b" == #["a", "<", "b"]
#guard tokenizeKeys "<>" == #["<", ">"]

end TokenizeKeysTests

/-! ## Term.parseColor Tests -/

section ParseColorTests

-- default → TB_DEFAULT (0)
#guard Term.parseColor "default" == 0
-- ANSI names → standard indices
#guard Term.parseColor "black" == 16   -- cube black, since 0 = TB_DEFAULT
#guard Term.parseColor "red" == 1
#guard Term.parseColor "white" == 7
#guard Term.parseColor "brCyan" == 14
#guard Term.parseColor "brWhite" == 15
-- rgb cube: index = 16 + 36R + 6G + B
#guard Term.parseColor "rgb000" == 16   -- black in cube
#guard Term.parseColor "rgb555" == 231  -- white in cube
#guard Term.parseColor "rgb520" == 208  -- orange
#guard Term.parseColor "rgb234" == 110  -- frost blue
-- grayscale: index = 232 + N
#guard Term.parseColor "gray0" == 232
#guard Term.parseColor "gray23" == 255
#guard Term.parseColor "gray4" == 236
-- invalid → 0 (default)
#guard Term.parseColor "rgb600" == 0    -- out of range
#guard Term.parseColor "gray24" == 0    -- out of range
#guard Term.parseColor "nosuchcolor" == 0

end ParseColorTests
