/-
  Core types: Cell, Column, Table, PureKey
  Table stores columns by name (HashMap) for direct name-based access
-/
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
  unfold Array.toggle
  have h' : ¬(arr.contains x = true) := by simp_all
  rw [if_neg h']
  have hc : (arr.push x).contains x = true := by simp
  rw [if_pos hc]
  -- goal: (arr.push x).filter (fun y => !(y == x)) = arr
  have hx : x ∉ arr := fun hm => h' (Array.contains_iff_mem.mpr hm)
  -- go through List for easier reasoning
  suffices hl : ((arr.push x).filter (fun y => !(y == x))).toList = arr.toList by
    have := congrArg List.toArray hl
    simp only [Array.toArray_toList] at this
    exact this
  rw [Array.toList_filter, Array.toList_push, List.filter_append, List.filter_cons]
  simp only [beq_self_eq_true, Bool.not_true, Bool.false_eq_true, ite_false, List.filter_nil,
    List.append_nil]
  rw [List.filter_eq_self]
  intro a ha
  have ha' : a ∈ arr := Array.mem_def.mpr ha
  exact bne_iff_ne.mpr (fun heq => hx (heq ▸ ha'))

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

-- | Gather rows by index array (reindex with type-appropriate defaults)
def gather (col : Column) (idxs : Array Nat) : Column :=
  match col with
  | .ints data   => .ints   (idxs.map fun i => data.getD i 0)
  | .floats data => .floats (idxs.map fun i => data.getD i 0)
  | .strs data   => .strs   (idxs.map fun i => data.getD i "")

-- | Take first n rows
def take (col : Column) (n : Nat) : Column :=
  match col with
  | .ints data   => .ints   (data.extract 0 n)
  | .floats data => .floats (data.extract 0 n)
  | .strs data   => .strs   (data.extract 0 n)

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

namespace Tc

-- | Compute pct and bar from count data (for Kdb/Mem freq → fromArrays).
def freqPctBar (cntData : Array Int64) : Array Float × Array String :=
  let total := cntData.foldl (init := 0) (· + ·)
  let pct := cntData.map fun c => if total > 0 then c.toFloat * 100 / total.toFloat else 0
  let bar := pct.map fun p => String.ofList (List.replicate (p / 5.0).toUInt32.toNat '#')
  (pct, bar)

/-! ## Core Typeclasses -/

-- Render context: all parameters for table rendering (avoids NavState dependency)
structure RenderCtx where
  inWidths   : Array Nat
  dispIdxs   : Array Nat
  nGrp       : Nat
  colOff     : Nat
  r0         : Nat
  r1         : Nat
  curRow     : Nat
  curCol     : Nat
  moveDir    : Int
  selColIdxs : Array Nat
  rowSels    : Array Nat
  styles     : Array UInt32
  precAdj    : Int
  widthAdj   : Int

/-- TblOps: unified read-only table interface.
    Provides row/column access, metadata queries, filtering, and rendering. -/
class TblOps (α : Type) where
  nRows     : α → Nat                                            -- row count in view
  colNames  : α → Array String                                   -- column names
  totalRows : α → Nat := nRows                                   -- actual rows (ADBC)
  filter    : α → String → IO (Option α)                         -- filter by expr
  distinct  : α → Nat → IO (Array String)                        -- distinct values
  findRow   : α → Nat → String → Nat → Bool → IO (Option Nat)    -- find row
  render    : α → RenderCtx → IO (Array Nat)
  -- extract columns [r0, r1) by index (for plot/export)
  getCols   : α → Array Nat → Nat → Nat → IO (Array Column) := fun _ _ _ _ => pure #[]
  -- column type name (e.g. "time", "int", "float", "str")
  colType   : α → Nat → String := fun _ _ => "?"
  -- export plot data to /tmp/tc-plot.dat via DB (returns category list, or none for fallback)
  -- args: tbl xName yName catName? xIsTime step truncLen
  plotExport : α → String → String → Option String → Bool → Nat → Nat → IO (Option (Array String))
    := fun _ _ _ _ _ _ _ => pure none
  -- fetch more rows (scroll-to-bottom): returns table with more rows, or none
  fetchMore : α → IO (Option α) := fun _ => pure none
  -- loading (file or URL)
  fromFile  : String → IO (Option α) := fun _ => pure none
  fromUrl   : String → IO (Option α) := fun _ => pure none

/-- ModifyTable: mutable table operations (extends TblOps).
    Column deletion and sorting; row deletion is done via filter. -/
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

-- Sort table by selected columns + cursor column, excluding group (key) columns
def ModifyTable.sort [ModifyTable α] (tbl : α) (cursor : Nat) (selIdxs : Array Nat) (grpIdxs : Array Nat) (asc : Bool) : IO α :=
  let cols := (selIdxs ++ #[cursor]).filter (!grpIdxs.contains ·)
  let cols := cols.foldl (init := #[]) fun acc c => if acc.contains c then acc else acc.push c
  if cols.isEmpty then pure tbl else sortBy cols asc tbl


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

-- | View kind: how to render/interact (used by key mapping for context-sensitive verbs)
inductive ViewKind where
  | tbl                                           -- table view
  | freqV (cols : Array String) (total : Nat)     -- frequency view with total distinct groups
  | colMeta                                       -- column metadata
  | fld (path : String) (depth : Nat)             -- folder browser: path + find depth
  deriving Inhabited, Repr, BEq

end Tc
