/-
  Core types: Cell, Column, Table, PureKey
  Table stores columns by name (HashMap) for direct name-based access
-/
import Std.Data.HashMap
import Tc.Data.ADBC.FFI

-- | Join array of strings with separator
def Array.join (arr : Array String) (sep : String) : String :=
  String.intercalate sep arr.toList

-- | Toggle element in array (add if absent, remove if present)
def Array.toggle [BEq α] (arr : Array α) (x : α) : Array α :=
  if arr.contains x then arr.filter (· != x) else arr.push x

-- | Cell value (sum type)
-- Uses Int64 to guarantee scalar representation (no MPZ boxing)
inductive Cell where
  | null
  | int (v : Int64)
  | float (v : Float)
  | str (v : String)
  | bool (v : Bool)
  deriving Repr, Inhabited

-- | Column: uniform typed storage (one type per column)
-- More efficient than Array Cell (no per-cell tag overhead)
-- ints: no null support; floats: NaN = null; strs: empty = null
inductive Column where
  | ints   (data : Array Int64)
  | floats (data : Array Float)
  | strs   (data : Array String)
  deriving Repr, Inhabited

namespace Column

-- | Get cell at row index
@[inline] def get (col : Column) (i : Nat) : Cell :=
  match col with
  | .ints data => .int (data.getD i 0)
  | .floats data =>
    let f := data.getD i 0
    if f.isNaN then .null else .float f
  | .strs data =>
    let s := data.getD i ""
    if s.isEmpty then .null else .str s

-- | Row count
def size : Column → Nat
  | .ints data => data.size
  | .floats data => data.size
  | .strs data => data.size

end Column

namespace Cell

-- | Raw string value (for PRQL filters)
def toRaw : Cell → String
  | .null    => ""
  | .int n   => s!"{n}"
  | .float f => s!"{f}"
  | .str s   => s
  | .bool b  => if b then "true" else "false"

-- | Extract string value
def str? : Cell → Option String | .str s => some s | _ => none

-- | Extract int value
def int? : Cell → Option Int64 | .int n => some n | _ => none

-- | Format cell value as PRQL literal
def toPrql : Cell → String
  | .null => "null"
  | .int n => s!"{n}"
  | .float f => s!"{f}"
  | .str s => s!"'{s}'"
  | .bool b => if b then "true" else "false"

end Cell

-- | DisplayInfo: metadata for navigation/PRQL building (no cell access)
structure DisplayInfo where
  colNames : Array String
  nRows    : Nat
  nCols    : Nat


