/-
  Pure tests for state machine logic.
  No IO - all tests are compile-time checks via #guard.
-/
import Tc.Nav
import Tc.View
import Tc.UI.Info
import Tc.Types

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
  queryMeta _ := pure (#[], #[], #[], #[], #[], #[], #[])
  queryFreq _ _ := pure (#[], #[], #[], #[], #[])
  filter _ _ := pure none
  distinct _ _ := pure #[]
  findRow _ _ _ _ _ := pure none
  render _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ := pure #[]

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

def infoOff : UI.Info.State := {}
def infoOn : UI.Info.State := { vis := true }

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

-- | Group multiple: maintains group order
#guard dispOrder #["c", "a"] #["a", "b", "c"] == #[2, 0, 1]

-- | Group non-existent: ignored
#guard dispOrder #["x"] #["a", "b", "c"] == #[0, 1, 2]

end DispOrderTests

end PureTest
