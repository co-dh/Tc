/-
  View: GView Table and GViewStack Table for full tc build
  Imports Table (ADBC/Kdb) and provides Table-specific fromFile.
-/
import Tc.Table
import Tc.View.Generic
import Tc.Data.Mem.Text
import Tc.Data.ADBC.Table

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

-- | Create View from file path (csv/parquet)
def fromFile (path : String) : IO (Option View) := do
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .error _ => pure none
    | .ok tbl => pure (GView.fromTbl (.mem tbl) path)
  else  -- parquet or other ADBC-supported format
    try
      match ← AdbcTable.fromFile path with
      | none => pure none
      | some tbl => pure (GView.fromTbl (.adbc tbl) path)
    catch _ => pure none

-- | verbDelta theorems (for View = GView Table)
theorem verbDelta_inc : GView.update (T := Table) v (.width .inc) rowPg =
    some ({ v with widthAdj := v.widthAdj + 1 }, .none) := rfl
theorem verbDelta_dec : GView.update (T := Table) v (.width .dec) rowPg =
    some ({ v with widthAdj := v.widthAdj - 1 }, .none) := rfl

end View

end Tc
