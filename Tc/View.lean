/-
  View: GView Table and GViewStack Table for full tc build
  Uses LoadTable typeclass for file loading (no direct ADBC import).
-/
import Tc.Table
import Tc.View.Generic
import Tc.Data.Mem.Text

namespace Tc

-- | View and ViewStack for unified Table type
abbrev View := GView Table
abbrev ViewStack := GViewStack Table

namespace View

-- | Create View from Table + path (alias for GView.fromTbl)
def fromTbl (tbl : Table) (path : String)
    (col : Nat := 0) (grp : Array String := #[]) (row : Nat := 0) : Option View :=
  GView.fromTbl tbl path col grp row

-- | Pure update (alias for GView.update)
def update (v : View) (cmd : Cmd) (rowPg : Nat) : Option (View × Effect) :=
  GView.update v cmd rowPg

-- | Create View from file path (uses LoadTable typeclass)
def fromFile (path : String) : IO (Option View) := do
  match ← LoadTable.fromFile (α := Table) path with
  | some tbl => pure (GView.fromTbl tbl path)
  | none => pure none

-- | verbDelta theorems (for View = GView Table)
theorem verbDelta_inc : GView.update (T := Table) v (.width .inc) rowPg =
    some ({ v with widthAdj := v.widthAdj + 1 }, .none) := rfl
theorem verbDelta_dec : GView.update (T := Table) v (.width .dec) rowPg =
    some ({ v with widthAdj := v.widthAdj - 1 }, .none) := rfl

end View

end Tc
