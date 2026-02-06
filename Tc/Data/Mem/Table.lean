/-
  In-memory table: column-major with typed columns
-/
import Std.Data.HashMap
import Std.Data.HashSet
import Tc.Data.CSV
import Tc.Error
import Tc.Nav
import Tc.Render
import Tc.Term
import Tc.Types

namespace Tc

-- | In-memory table: typed column storage (names.size = cols.size invariant)
structure MemTable where
  names : Array String
  cols  : Array Column
  h_eq  : names.size = cols.size := by omega

namespace MemTable

-- | Parse string to float (returns NaN on failure)
@[extern "lean_string_to_float"]
opaque stringToFloat : @& String → Float

-- | Detect column type: only numeric if ALL non-empty values convert
def detectType (vals : Array String) : Char :=
  let nonEmpty := vals.filter (·.length > 0)
  if nonEmpty.isEmpty then 's'
  else if nonEmpty.all (·.toInt?.isSome) then 'i'
  else if nonEmpty.all (fun s => !(stringToFloat s).isNaN) then 'f'
  else 's'

-- | Build typed column from string values.
-- Note: for int columns, unparseable values silently default to 0 via .toInt?.getD 0.
-- This is intentional: detectType already verified ALL non-empty values parse,
-- so only empty strings (nulls) hit the default path.
def buildColumn (vals : Array String) : Column :=
  match detectType vals with
  | 'i' => .ints (vals.map fun s => s.toInt?.getD 0 |>.toInt64)
  | 'f' => .floats (vals.map stringToFloat)
  | _ => .strs vals

-- | Load CSV file into MemTable
def load (path : String) : IO (Except String MemTable) := do
  let content ← IO.FS.readFile path
  let recs := CSV.parse content
  match recs.toList with
  | [] => pure (.ok ⟨#[], #[], rfl⟩)
  | hdr :: rest =>
    let names := hdr
    let nc := names.size
    let strCols : Array (Array String) := Id.run do
      let mut cols := (List.replicate nc #[]).toArray
      for row in rest do
        for i in [:nc] do
          cols := cols.modify i (·.push (row.getD i ""))
      cols
    let cols := strCols.map buildColumn
    if h : names.size = cols.size then pure (.ok ⟨names, cols, h⟩)
    else pure (.error "column count mismatch")

-- | Row count
def nRows (t : MemTable) : Nat := (t.cols.getD 0 default).size

-- | Compare floats (handles NaN as null)
def cmpFloat (x y : Float) : Ordering :=
  if x < y then .lt else if x > y then .gt else .eq

-- | Compare two cells (for sorting)
def cmpCell (a b : Cell) : Ordering :=
  match a, b with
  | .null, .null => .eq
  | .null, _ => .lt
  | _, .null => .gt
  | .int x, .int y => compare x y
  | .float x, .float y => cmpFloat x y
  | .str x, .str y => compare x y
  | _, _ => .eq

-- | Compare two rows by multiple column indices
def cmpRows (cols : Array Column) (idxs : Array Nat) (r1 r2 : Nat) : Ordering :=
  idxs.foldl (init := .eq) fun ord c =>
    if ord != .eq then ord
    else cmpCell ((cols.getD c default).get r1) ((cols.getD c default).get r2)

-- | Sort table by column indices (asc=true for ascending)
def sort (t : MemTable) (idxs : Array Nat) (asc : Bool) : MemTable :=
  let n := nRows t
  let perm := (Array.range n).qsort fun i j =>
    let ord := cmpRows t.cols idxs i j
    if asc then ord == .lt else ord == .gt
  let cols' := t.cols.map (·.gather perm)
  { t with cols := cols', h_eq := by simp [cols', Array.size_map, t.h_eq] }

-- | sort preserves names.size = cols.size
theorem sort_preserves_h_eq (t : MemTable) (idxs : Array Nat) (asc : Bool) :
    (MemTable.sort t idxs asc).names.size = (MemTable.sort t idxs asc).cols.size :=
  (MemTable.sort t idxs asc).h_eq

-- | nRows equals the size of the first column (when cols is non-empty)
theorem nRows_eq_col_size (t : MemTable) (h : t.cols.size > 0) :
    MemTable.nRows t = (t.cols[0]).size := by
  unfold nRows
  simp [Array.getD]
  split
  · rfl
  · omega

end MemTable

-- NOTE: ReadTable/ModifyTable/RenderTable instances for MemTable are defined in Table variants
-- (Table.lean, Table/Mem.lean, etc.) which import queryMeta/queryFreq from Mem/Meta.lean, Mem/Freq.lean

namespace MemTable

-- | Parse filter expr "col == val" or "col == 'val'" into (colName, rawVal)
private def parseEq (s : String) : Option (String × String) :=
  match s.splitOn " == " with
  | [col, val] =>
    let v := if val.startsWith "'" && val.endsWith "'" then val.drop 1 |>.dropEnd 1 else val
    some (col.trimAscii.toString, v.toString)
  | _ => none

-- | Filter MemTable by expr like "a == 1 && b == 'x'"
def filter (t : MemTable) (expr : String) : IO (Option MemTable) := do
  -- parse "col == val && col == val ..."
  let parts := expr.splitOn " && "
  let eqs := parts.filterMap parseEq
  if eqs.isEmpty then return some t
  -- find column indices and expected values
  let colIdxs := eqs.filterMap fun (col, _) => t.names.idxOf? col
  let vals := eqs.map (·.2)
  if colIdxs.length != vals.length then return some t
  -- find matching rows
  let n := MemTable.nRows t
  let mut rows : Array Nat := #[]
  for r in [:n] do
    let mut ok := true
    for i in [:colIdxs.length] do
      let cIdx := colIdxs.getD i 0
      let exp := vals.getD i ""
      let act := (t.cols.getD cIdx default).get r |>.toRaw
      if act != exp then ok := false; break
    if ok then rows := rows.push r
  -- build filtered table
  let cols' := t.cols.map (·.gather rows)
  return some ⟨t.names, cols', by simp [cols', Array.size_map, t.h_eq]⟩

-- | filter preserves names.size = cols.size (for the returned MemTable)
-- Note: filter returns IO (Option MemTable) — each MemTable it constructs
-- satisfies the invariant by construction (the h_eq proof is embedded in the mk).

-- | Distinct values for a column
def distinct (t : MemTable) (col : Nat) : IO (Array String) := pure <| Id.run do
  let c := t.cols.getD col default
  let mut seen : Std.HashSet String := {}
  let mut result : Array String := #[]
  for i in [:MemTable.nRows t] do
    let s := (c.get i).toRaw
    if !seen.contains s then
      seen := seen.insert s
      result := result.push s
  result

-- | Find row from starting position, forward or backward (with wrap)
def findRow (t : MemTable) (col : Nat) (val : String) (start : Nat) (fwd : Bool) : IO (Option Nat) := pure <| Id.run do
  let c := t.cols.getD col default
  let n := MemTable.nRows t
  if fwd then
    for i in [start:n] do
      if (c.get i).toRaw == val then return some i
    -- wrap around
    for i in [:start] do
      if (c.get i).toRaw == val then return some i
  else
    -- backward: start-1 down to 0, then wrap to end
    for i in [:start] do
      let idx := start - 1 - i
      if (c.get idx).toRaw == val then return some idx
    for i in [:n - start] do
      let idx := n - 1 - i
      if (c.get idx).toRaw == val then return some idx
  none

-- | Format table as plain text (tab-separated, no unicode)
def toText (t : MemTable) : String := Id.run do
  let nr := nRows t
  let nc := t.names.size
  let mut lines : Array String := #[]
  -- header
  lines := lines.push ("\t".intercalate t.names.toList)
  -- data rows
  for r in [:nr] do
    let mut row : Array String := #[]
    for c in [:nc] do
      row := row.push ((t.cols.getD c default).get r).toRaw
    lines := lines.push ("\t".intercalate row.toList)
  "\n".intercalate lines.toList

-- | Select columns by name
def selCols (t : MemTable) (names : Array String) : MemTable :=
  let idxs := names.filterMap t.names.idxOf?
  let cols' := idxs.map fun i => t.cols.getD i default
  let names' := idxs.map fun i => t.names.getD i ""
  ⟨names', cols', by simp [names', cols', Array.size_map]⟩

-- | selCols preserves names.size = cols.size
theorem selCols_preserves_h_eq (t : MemTable) (keep : Array String) :
    (MemTable.selCols t keep).names.size = (MemTable.selCols t keep).cols.size :=
  (MemTable.selCols t keep).h_eq

-- | Take first n rows
def take (t : MemTable) (n : Nat) : MemTable :=
  let cols' := t.cols.map (·.take n)
  { t with cols := cols', h_eq := by simp [cols', Array.size_map, t.h_eq] }

-- | take preserves names.size = cols.size
theorem take_preserves_h_eq (t : MemTable) (n : Nat) :
    (MemTable.take t n).names.size = (MemTable.take t n).cols.size :=
  (MemTable.take t n).h_eq

-- | Sort by column names (converts to indices)
def sortByNames (t : MemTable) (cols : Array (String × Bool)) : MemTable :=
  let idxs := cols.filterMap fun (n, _) => t.names.idxOf? n
  let asc := if h : cols.size > 0 then cols[cols.size - 1].2 else true
  sort t idxs asc

-- | sortByNames preserves names.size = cols.size
theorem sortByNames_preserves_h_eq (t : MemTable) (cols : Array (String × Bool)) :
    (MemTable.sortByNames t cols).names.size = (MemTable.sortByNames t cols).cols.size :=
  (MemTable.sortByNames t cols).h_eq

end MemTable

-- | ExecOp instance for MemTable (partial support)
instance : ExecOp MemTable where
  exec t op := match op with
    | .filter expr => MemTable.filter t expr
    | .sort cols => pure (some (MemTable.sortByNames t cols))
    | .sel cols => pure (some (MemTable.selCols t cols))
    | .take n => pure (some (MemTable.take t n))
    | .derive _ | .group _ _ => pure none  -- unsupported

-- | MemConvert instance for MemTable (identity)
instance : MemConvert MemTable MemTable where
  wrap   := id
  unwrap := some

end Tc
