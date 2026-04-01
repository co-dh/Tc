/-
  Core types: Cell, Column, Table, PureKey
  Table stores columns by name (HashMap) for direct name-based access
-/
import Tc.StrEnum
-- | Join array elements with separator (avoids .toList |> sep.intercalate)
@[inline] def Array.joinWith (a : Array String) (sep : String) : String :=
  sep.intercalate a.toList

-- | Column type — parsed once at FFI boundary, used everywhere else
inductive ColType | int | float | decimal | str | date | time | timestamp | bool | other
  deriving BEq, Repr, Inhabited, StrEnum

namespace ColType
-- ofString with fallback (FFI boundary returns unknown types)
def ofString (s : String) : ColType := StrEnum.ofString? s |>.getD .other
def isNumeric : ColType → Bool | .int | .float | .decimal => true | _ => false
def isTime : ColType → Bool | .time | .timestamp | .date => true | _ => false
end ColType

-- | Toggle element in array (add if absent, remove if present)
@[inline] def Array.toggle [BEq α] (arr : Array α) (x : α) : Array α :=
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

@[inline] def mapArr (col : Column)
    (fi : Array Int64 → Array Int64) (ff : Array Float → Array Float) (fs : Array String → Array String) : Column :=
  match col with | .ints d => .ints (fi d) | .floats d => .floats (ff d) | .strs d => .strs (fs d)

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
@[inline] def size (col : Column) : Nat :=
  match col with | .ints d => d.size | .floats d => d.size | .strs d => d.size

def gather (col : Column) (idxs : Array Nat) : Column :=
  col.mapArr (fun d => idxs.map fun i => d.getD i 0) (fun d => idxs.map fun i => d.getD i 0)
    (fun d => idxs.map fun i => d.getD i "")

def take (col : Column) (n : Nat) : Column := col.mapArr (·.extract 0 n) (·.extract 0 n) (·.extract 0 n)

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
  | .str s => s!"'{s.replace "'" "''"}'"
  | .bool b => if b then "true" else "false"

end Cell

namespace Tc

-- | Escape single quotes for SQL string literals
def escSql (s : String) : String := s.replace "'" "''"

-- | Compute pct and bar from count data (for freq → fromArrays).
def freqPctBar (cntData : Array Int64) : Array Float × Array String :=
  let total := cntData.sum
  let pct := cntData.map fun c => if total > 0 then c.toFloat * 100 / total.toFloat else 0
  let bar := pct.map fun p => "".pushn '#' (p / 5.0).toUInt32.toNat
  (pct, bar)

/-! ## Core Typeclasses -/

-- Render context: all parameters for table rendering (avoids NavState dependency)
structure RenderCtx where
  inWidths   : Array Nat
  dispIdxs   : Array Nat
  nGrp       : Nat
  r0         : Nat
  r1         : Nat
  curRow     : Nat
  curCol     : Nat
  moveDir    : Int
  selColIdxs : Array Nat
  rowSels    : Array Nat
  hiddenIdxs : Array Nat
  styles     : Array UInt32
  prec       : Int
  widthAdj   : Int
  heatMode   : UInt8 := 1  -- 0=off, 1=numeric, 2=categorical, 3=both
  sparklines : Array String := #[]

-- | Build PRQL filter expression from fzf result (default for TblOps.buildFilter)
-- With --print-query: line 0 = query, lines 1+ = selections
def buildFilterPrql (col : String) (vals : Array String) (result : String) (numeric : Bool) : String :=
  let lines := result.splitOn "\n" |>.filter (!·.isEmpty) |>.toArray
  let input := lines.getD 0 ""
  let fromHints := (lines.extract 1 lines.size).filter vals.contains
  let selected := if vals.contains input && !fromHints.contains input
                  then #[input] ++ fromHints else fromHints
  let q := fun v => if numeric then v else s!"'{v}'"
  if selected.size == 1 then s!"{col} == {q (selected.getD 0 "")}"
  else if selected.size > 1 then "(" ++ (selected.map fun v => s!"{col} == {q v}").joinWith " || " ++ ")"
  else if !input.isEmpty then input
  else ""

-- compat shim — prefer ColType.isNumeric on typed values
def isNumericType (t : String) : Bool := (ColType.ofString t).isNumeric

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
  -- column type
  colType   : α → Nat → ColType := fun _ _ => .other
  -- build filter expression from fzf result (default: PRQL syntax)
  buildFilter : α → String → Array String → String → Bool → String
    := fun _ => buildFilterPrql
  -- filter header hint (shown above fzf input, default: PRQL examples)
  filterPrompt : α → String → String → String
    := fun _ col typ =>
      let eg := if isNumericType typ
        then s!"e.g. {col} > 5,  {col} >= 10 && {col} < 100"
        else s!"e.g. {col} == 'USD',  {col} ~= 'pattern'"
      s!"PRQL filter on {col} ({typ}):  {eg}"
  -- export plot data to tmpdir/plot.dat via DB (returns category list, or none for fallback)
  -- args: tbl xName yName catName? xIsTime step truncLen
  plotExport : α → String → String → Option String → Bool → Nat → Nat → IO (Option (Array String))
    := fun _ _ _ _ _ _ _ => pure none
  -- get cell value as string (for preview)
  cellStr   : α → Nat → Nat → IO String := fun _ _ _ => pure ""
  -- fetch more rows (scroll-to-bottom): returns table with more rows, or none
  fetchMore : α → IO (Option α) := fun _ => pure none
  -- loading (file or URL)
  fromFile  : String → IO (Option α) := fun _ => pure none
  fromUrl   : String → IO (Option α) := fun _ => pure none

/-- ModifyTable: mutable table operations (extends TblOps).
    Column hiding and sorting; row deletion is done via filter. -/
class ModifyTable (α : Type) extends TblOps α where
  hideCols : Array Nat → α → IO α           -- hide columns
  sortBy  : Array Nat → Bool → α → IO α    -- sort by columns

-- Hide columns at cursor + selections, return new table and filtered group
def ModifyTable.hide [ModifyTable α] (tbl : α) (cursor : Nat) (sels : Array Nat) (grp : Array String)
    : IO (α × Array String) := do
  let idxs := if sels.contains cursor then sels else sels.push cursor
  let names := TblOps.colNames tbl
  let hideNames := idxs.map (names.getD · "")
  pure (← hideCols idxs tbl, grp.filter (!hideNames.contains ·))

-- Sort table by selected columns + cursor column, excluding group (key) columns
def ModifyTable.sort [ModifyTable α] (tbl : α) (cursor : Nat) (selIdxs : Array Nat) (grpIdxs : Array Nat) (asc : Bool) : IO α :=
  let cols := (selIdxs ++ #[cursor])
    |>.filter (!grpIdxs.contains ·)
    |>.toList.eraseDups.toArray
  if cols.isEmpty then pure tbl else sortBy cols asc tbl


-- | Keep columns not in hide set (shared by hideCols impls)
def keepCols (nCols : Nat) (hideIdxs : Array Nat) (names : Array String) : Array String :=
  (Array.range nCols).filter (!hideIdxs.contains ·) |>.map (names.getD · "")

-- | Convert columns to tab-separated text (shared by Table toText impls)
def colsToText (names : Array String) (cols : Array Column) (nr : Nat) : String := Id.run do
  let mut lines : Array String := #[names.joinWith "\t"]
  for r in [:nr] do
    let row := cols.map fun col => (col.get r).toRaw
    lines := lines.push (row.joinWith "\t")
  lines.joinWith "\n"

-- | Aggregate function
inductive Agg where
  | count | sum | avg | min | max | stddev | dist
  deriving Repr, Inhabited, BEq, StrEnum

namespace Agg
def short (a : Agg) : String := toString a
def fromStr? := @StrEnum.ofString? Agg _
end Agg

-- | Table operation (single pipeline stage)
inductive Op where
  | filter (expr : String)
  | sort (cols : Array (String × Bool))
  | sel (cols : Array String)
  | exclude (cols : Array String)
  | derive (bindings : Array (String × String))
  | group (keys : Array String) (aggs : Array (Agg × String × String))
  | take (n : Nat)
  deriving Inhabited, BEq

-- | View kind: how to render/interact (used by key mapping for context-sensitive verbs)
inductive ViewKind where
  | tbl                                           -- table view
  | freqV (cols : Array String) (total : Nat)     -- frequency view with total distinct groups
  | colMeta                                       -- column metadata
  | fld (path : String) (depth : Nat)             -- folder browser: path + find depth
  deriving Inhabited, Repr, BEq

-- | Context string for config lookup (shared by Fzf and App dispatch)
def ViewKind.ctxStr : ViewKind → String
  | .freqV _ _ => "freqV" | .colMeta => "colMeta" | .fld _ _ => "fld" | .tbl => "tbl"

-- | Plot types and export formats
inductive PlotKind where | line | bar | scatter | hist | box | area | density | step | violin deriving Repr, BEq, StrEnum

inductive ExportFmt where | csv | parquet | json | ndjson deriving Repr, BEq, StrEnum

-- | Residual effects from pure code that can't do IO (View.update, ViewStack.update, Freq.update).
inductive Effect where
  | none | quit | fetchMore
  | sort (colIdx : Nat) (sels : Array Nat) (grp : Array Nat) (asc : Bool)
  | exclude (cols : Array String)
  | freq (colNames : Array String)
  | freqFilter (cols : Array String) (row : Nat)
  deriving Repr, BEq

namespace Effect
def isNone : Effect → Bool | .none => true | _ => false
end Effect

end Tc

-- | Macro: generates inductive + toStr + ToString + all + strMap + ofString?
-- Adding a command = one line. Each entry listed exactly once.
open Lean Elab Command Parser in
elab "cmd_enum " name:ident " where" entries:((ppLine "| " ident " => " str))* : command => do
  let pairs := entries.map fun e =>
    let n := e.raw[1].getId.toString (escape := false)
    let s := e.raw[3].isStrLit?.getD ""
    (n, s)
  let elabStr (s : String) : CommandElabM Unit := do
    match runParserCategory (← getEnv) `command s with
    | .ok stx => elabCommand stx
    | .error e => throwError e
  let nm := name.getId.toString (escape := false)
  -- inductive
  let ctors := pairs.map (fun (n, _) => s!"  | {n}") |>.joinWith "\n"
  elabStr s!"inductive {nm} where\n{ctors}\n  deriving BEq, Hashable, Repr, Inhabited"
  elabStr s!"namespace {nm}"
  -- toStr
  let arms := pairs.map (fun (n, s) => s!"  | .{n} => \"{s}\"") |>.joinWith "\n"
  elabStr s!"def toStr : {nm} → String\n{arms}"
  elabStr s!"instance : ToString {nm} where toString := toStr"
  -- all
  let items := pairs.map (fun (n, _) => s!".{n}") |>.joinWith ", "
  elabStr s!"def all : Array {nm} := #[{items}]"
  -- strMap + ofString?
  elabStr s!"private def strMap : Std.HashMap String {nm} := all.foldl (init := \{}) fun m c => m.insert c.toStr c"
  elabStr s!"def ofString? (s : String) : Option {nm} := strMap.get? s"
  elabStr s!"end {nm}"

namespace Tc

cmd_enum Cmd where
  | rowInc       => "row.inc"
  | rowDec       => "row.dec"
  | rowPgdn      => "row.pgdn"
  | rowPgup      => "row.pgup"
  | rowTop       => "row.top"
  | rowBot       => "row.bot"
  | rowSel       => "row.sel"
  | rowSearch    => "row.search"
  | rowFilter    => "row.filter"
  | rowSearchNext => "row.searchNext"
  | rowSearchPrev => "row.searchPrev"
  | colInc       => "col.inc"
  | colDec       => "col.dec"
  | colFirst     => "col.first"
  | colLast      => "col.last"
  | colGrp       => "col.grp"
  | colHide      => "col.hide"
  | colExclude   => "col.exclude"
  | colShiftL    => "col.shiftL"
  | colShiftR    => "col.shiftR"
  | sortAsc      => "sort.asc"
  | sortDesc     => "sort.desc"
  | colSplit     => "col.split"
  | colDerive    => "col.derive"
  | colSearch    => "col.search"
  | plotArea     => "plot.area"
  | plotLine     => "plot.line"
  | plotScatter  => "plot.scatter"
  | plotBar      => "plot.bar"
  | plotBox      => "plot.box"
  | plotStep     => "plot.step"
  | plotHist     => "plot.hist"
  | plotDensity  => "plot.density"
  | plotViolin   => "plot.violin"
  | tblMenu      => "tbl.menu"
  | stkSwap      => "stk.swap"
  | stkPop       => "stk.pop"
  | stkDup       => "stk.dup"
  | tblQuit      => "tbl.quit"
  | tblXpose     => "tbl.xpose"
  | tblDiff      => "tbl.diff"
  | infoTog      => "info.tog"
  | precDec      => "prec.dec"
  | precInc      => "prec.inc"
  | precZero     => "prec.zero"
  | precMax      => "prec.max"
  | cellUp       => "cell.up"
  | cellDn       => "cell.dn"
  | heat0        => "heat.0"
  | heat1        => "heat.1"
  | heat2        => "heat.2"
  | heat3        => "heat.3"
  | metaPush     => "meta.push"
  | metaSetKey   => "meta.setKey"
  | metaSelNull  => "meta.selNull"
  | metaSelSingle => "meta.selSingle"
  | freqOpen     => "freq.open"
  | freqFilter   => "freq.filter"
  | folderPush   => "folder.push"
  | folderEnter  => "folder.enter"
  | folderParent => "folder.parent"
  | folderDel    => "folder.del"
  | folderDepthDec => "folder.depthDec"
  | folderDepthInc => "folder.depthInc"
  | tblExport    => "tbl.export"
  | sessSave     => "sess.save"
  | sessLoad     => "sess.load"
  | tblJoin      => "tbl.join"
  | themeOpen    => "theme.open"
  | themePreview => "theme.preview"

namespace Cmd

def plotKind? : Cmd → Option PlotKind
  | .plotArea => some .area | .plotLine => some .line | .plotScatter => some .scatter
  | .plotBar => some .bar | .plotBox => some .box | .plotStep => some .step
  | .plotHist => some .hist | .plotDensity => some .density | .plotViolin => some .violin
  | _ => none

end Cmd

end Tc
