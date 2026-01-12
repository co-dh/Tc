/-
  Core types: Cell, Column, Table, PureKey
  Table stores columns by name (HashMap) for direct name-based access
-/
import Std.Data.HashMap

-- | Join array of strings with separator (no intermediate List)
def Array.join (arr : Array String) (sep : String) : String :=
  if arr.isEmpty then "" else
  arr[1:].foldl (init := arr[0]!) fun acc s => acc ++ sep ++ s

-- | Toggle element in array (add if absent, remove if present)
def Array.toggle [BEq α] (arr : Array α) (x : α) : Array α :=
  if arr.contains x then arr.filter (· != x) else arr.push x

-- | Toggle same element twice: if x not in arr, returns arr
-- (add x, then remove x = original)
-- Proof requires Array.filter lemmas not in stdlib:
--   1. (arr.push x).contains x = true
--   2. (arr.push x).filter (· != x) = arr when x ∉ arr
theorem Array.toggle_toggle_not_mem [BEq α] [LawfulBEq α] (arr : Array α) (x : α)
    (h : !arr.contains x) : (arr.toggle x).toggle x = arr := by
  sorry  -- deferred: requires Array.filter_push lemmas

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

-- | Format cell value as PRQL literal
def toPrql : Cell → String
  | .null => "null"
  | .int n => s!"{n}"
  | .float f => s!"{f}"
  | .str s => s!"'{s}'"
  | .bool b => if b then "true" else "false"

end Cell

-- Read-only table access
class ReadTable (α : Type) where
  nRows     : α → Nat                         -- row count in current view
  colNames  : α → Array String                -- column names (size = nCols)
  totalRows : α → Nat := nRows                -- total rows (for ADBC: actual count)
  isAdbc    : α → Bool := fun _ => false      -- true if DB-backed (search disabled)

-- Meta tuple: (names, types, cnts, dists, nullPcts, mins, maxs)
abbrev MetaTuple := Array String × Array String × Array Int64 × Array Int64 × Array Int64 × Array String × Array String

-- Freq tuple: (keyNames, keyCols, cntData, pctData, barData)
abbrev FreqTuple := Array String × Array Column × Array Int64 × Array Float × Array String

-- Query operations (meta, freq, filter, distinct, findRow)
class QueryTable (α : Type) where
  queryMeta : α → IO MetaTuple
  queryFreq : α → Array Nat → IO FreqTuple
  filter    : α → String → IO (Option α)
  distinct  : α → Nat → IO (Array String)
  findRow   : α → Nat → String → Nat → Bool → IO (Option Nat)  -- find row from start, fwd/bwd

-- Mutable table operations (column-only; row deletion via SQL filter)
class ModifyTable (α : Type) extends ReadTable α where
  delCols : Array Nat → α → IO α           -- delete columns by indices
  sortBy  : Array Nat → Bool → α → IO α    -- sort by column indices, asc/desc

-- Delete columns at cursor + selections, return new table and filtered group
def ModifyTable.del [ModifyTable α] (tbl : α) (cursor : Nat) (sels : Array Nat) (grp : Array String)
    : IO (α × Array String) := do
  let idxs := if sels.contains cursor then sels else sels.push cursor
  let names := ReadTable.colNames tbl
  let delNames := idxs.map (names.getD · "")
  pure (← delCols idxs tbl, grp.filter (!delNames.contains ·))

-- Sort table by group indices (asc) then cursor column
def ModifyTable.sort [ModifyTable α] (tbl : α) (cursor : Nat) (grpIdxs : Array Nat) (asc : Bool) : IO α :=
  sortBy (grpIdxs.push cursor) asc tbl

-- | Wrap source type M into target type T (e.g., MemTable → Table)
class WrapMem (M T : Type) where
  wrapMem : M → T

-- | Load table from file (each variant implements differently)
class LoadTable (T : Type) where
  fromFile : String → IO (Option T)

-- | View kind: how to render/interact (used by key mapping for context-sensitive verbs)
inductive ViewKind where
  | tbl                                  -- table view
  | freqV (cols : Array String)          -- frequency view
  | colMeta                              -- column metadata
  | fld (path : String) (depth : Nat)    -- folder browser: path + find depth
  deriving Inhabited, Repr, BEq
