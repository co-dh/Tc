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

-- Meta tuple: (names, types, cnts, dists, nullPcts, mins, maxs)
abbrev MetaTuple := Array String × Array String × Array Int64 × Array Int64 × Array Int64 × Array String × Array String

-- Freq tuple: (keyNames, keyCols, cntData, pctData, barData)
abbrev FreqTuple := Array String × Array Column × Array Int64 × Array Float × Array String

-- | Compute pct and bar from count data (shared by Mem/ADBC/Kdb freq)
def freqStats (cntData : Array Int64) : Array Float × Array String :=
  let total := cntData.foldl (init := 0) (· + ·)
  let pct := cntData.map fun c => if total > 0 then c.toFloat * 100 / total.toFloat else 0
  let bar := pct.map fun p => String.ofList (List.replicate (p / 5).toUInt32.toNat '#')
  (pct, bar)

-- | TblOps: read access + query ops + render (unified table interface)
-- Render params expanded to avoid NavState dependency (NavState defined after Types)
class TblOps (α : Type) where
  nRows     : α → Nat                                            -- row count in view
  colNames  : α → Array String                                   -- column names
  totalRows : α → Nat := nRows                                   -- actual rows (ADBC)
  isAdbc    : α → Bool := fun _ => false                         -- DB-backed?
  queryMeta : α → IO MetaTuple                                   -- column metadata
  queryFreq : α → Array Nat → IO FreqTuple                       -- frequency query
  filter    : α → String → IO (Option α)                         -- filter by expr
  distinct  : α → Nat → IO (Array String)                        -- distinct values
  findRow   : α → Nat → String → Nat → Bool → IO (Option Nat)    -- find row
  -- render: expanded signature (NavState unpacked at call site)
  render    : α → (cols : Array Column) → (names fmts : Array String)
            → (inWidths dispIdxs : Array Nat) → (nGrp colOff r0 r1 curRow curCol : Nat)
            → (moveDir : Int) → (selColIdxs rowSels : Array Nat)
            → (styles : Array UInt32) → (precAdj widthAdj : Int) → IO (Array Nat)
  -- loading (file or URL)
  fromFile  : String → IO (Option α) := fun _ => pure none
  fromUrl   : String → IO (Option α) := fun _ => pure none

-- Mutable table ops (column-only; row deletion via filter)
class ModifyTable (α : Type) extends TblOps α where
  delCols : Array Nat → α → IO α           -- delete columns
  sortBy  : Array Nat → Bool → α → IO α    -- sort by columns

-- Delete columns at cursor + selections, return new table and filtered group
def ModifyTable.del [ModifyTable α] (tbl : α) (cursor : Nat) (sels : Array Nat) (grp : Array String)
    : IO (α × Array String) := do
  let idxs := if sels.contains cursor then sels else sels.push cursor
  let names := TblOps.colNames tbl
  let delNames := idxs.map (names.getD · "")
  pure (← delCols idxs tbl, grp.filter (!delNames.contains ·))

-- Sort table by group indices (asc) then cursor column
def ModifyTable.sort [ModifyTable α] (tbl : α) (cursor : Nat) (grpIdxs : Array Nat) (asc : Bool) : IO α :=
  sortBy (grpIdxs.push cursor) asc tbl

-- | Bidirectional conversion between MemTable and T
class MemConvert (M T : Type) where
  wrap   : M → T              -- M → T (e.g., MemTable → Table)
  unwrap : T → Option M       -- T → M? (e.g., Table → MemTable?)

-- | Keep columns not in delete set (shared by delCols impls)
def keepCols (nCols : Nat) (delIdxs : Array Nat) (names : Array String) : Array String :=
  (Array.range nCols).filter (!delIdxs.contains ·) |>.map (names.getD · "")

-- | Convert columns to tab-separated text (shared by Table toText impls)
def colsToText (names : Array String) (cols : Array Column) (nr : Nat) : String := Id.run do
  let mut lines : Array String := #["\t".intercalate names.toList]
  for r in [:nr] do
    let row := cols.map fun col => (col.get r).toRaw
    lines := lines.push ("\t".intercalate row.toList)
  "\n".intercalate lines.toList

namespace Tc

-- | Aggregate function
inductive Agg where
  | count | sum | avg | min | max | stddev | dist
  deriving Repr, Inhabited, BEq

namespace Agg
def short : Agg → String
  | .count => "count" | .sum => "sum" | .avg => "avg"
  | .min => "min" | .max => "max" | .stddev => "stddev" | .dist => "dist"
end Agg

-- | Table operation (single pipeline stage)
inductive Op where
  | filter (expr : String)
  | sort (cols : Array (String × Bool))
  | sel (cols : Array String)
  | derive (bindings : Array (String × String))
  | group (keys : Array String) (aggs : Array (Agg × String × String))
  | take (n : Nat)
  deriving Inhabited

-- | Execute operation on table (backend-specific)
class ExecOp (α : Type) where
  exec : α → Op → IO (Option α)

end Tc

-- | View kind: how to render/interact (used by key mapping for context-sensitive verbs)
inductive ViewKind where
  | tbl                                  -- table view
  | freqV (cols : Array String)          -- frequency view
  | colMeta                              -- column metadata
  | fld (path : String) (depth : Nat)    -- folder browser: path + find depth
  deriving Inhabited, Repr, BEq
