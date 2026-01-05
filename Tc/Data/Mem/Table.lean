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

-- | In-memory table: typed column storage
structure MemTable where
  names : Array String
  cols  : Array Column

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

-- | Build typed column from string values
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
  | [] => pure (.ok ⟨#[], #[]⟩)
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
    pure (.ok ⟨names, cols⟩)

-- | Split line on whitespace (multiple spaces = one delimiter)
private def splitWs (s : String) : Array String :=
  s.splitOn " " |>.filter (·.length > 0) |>.toArray

-- | Find mode (most common value) in array
private def mode (xs : Array Nat) : Nat := Id.run do
  let mut m : Std.HashMap Nat Nat := {}
  for x in xs do m := m.insert x (m.getD x 0 + 1)
  let mut best := 0; let mut cnt := 0
  for (k, v) in m do if v > cnt then best := k; cnt := v
  best

-- | Parse space-separated text (like ps aux, ls -l output)
-- Auto-detects column count via mode (handles filenames with spaces)
-- If header has fewer fields than mode, auto-generate column names
def fromText (content : String) : Except String MemTable :=
  let lines := content.splitOn "\n" |>.filter (·.length > 0)
  match lines with
  | [] => .ok ⟨#[], #[]⟩
  | hdr :: rest =>
    let hdrFields := splitWs hdr
    -- find mode of field counts across all lines
    let allFields := lines.map (splitWs · |>.size) |>.toArray
    let nc := mode allFields
    -- use header if it has enough fields, else auto-generate
    let (names, dataLines) := if hdrFields.size >= nc
      then (hdrFields, rest)
      else ((List.range nc).map (s!"c{·+1}") |>.toArray, hdr :: rest)
    let strCols : Array (Array String) := Id.run do
      let mut cols := (List.replicate nc #[]).toArray
      for line in dataLines do
        let fields := splitWs line
        for i in [:nc] do
          -- last col gets rest of line if more fields than nc
          let v := if i == nc - 1 && fields.size > nc
            then " ".intercalate (fields.toList.drop i)
            else fields.getD i ""
          cols := cols.modify i (·.push v)
      cols
    let cols := strCols.map buildColumn
    .ok ⟨names, cols⟩

-- | Load from stdin (reads all input)
def fromStdin : IO (Except String MemTable) := do
  let content ← (← IO.getStdin).readToEnd
  pure (fromText content)

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
  let cols' := t.cols.map fun col =>
    match col with
    | .ints data => .ints (perm.map fun i => data.getD i 0)
    | .floats data => .floats (perm.map fun i => data.getD i 0)
    | .strs data => .strs (perm.map fun i => data.getD i "")
  { t with cols := cols' }

end MemTable

-- | ReadTable instance for MemTable
instance : ReadTable MemTable where
  nRows    := MemTable.nRows
  colNames := (·.names)

-- | ModifyTable instance for MemTable
instance : ModifyTable MemTable where
  delCols := fun delIdxs t => pure
    { names := let keepIdxs := (Array.range t.names.size).filter (!delIdxs.contains ·)
               keepIdxs.map fun i => t.names.getD i ""
      cols  := let keepIdxs := (Array.range t.names.size).filter (!delIdxs.contains ·)
               keepIdxs.map fun i => t.cols.getD i default }
  sortBy := fun idxs asc t => pure (MemTable.sort t idxs asc)

-- | RenderTable instance for MemTable
instance : RenderTable MemTable where
  render nav inWidths colOff r0 r1 moveDir st precAdj widthAdj :=
    Term.renderTable nav.tbl.cols nav.tbl.names #[] inWidths nav.dispColIdxs
      (MemTable.nRows nav.tbl).toUInt64 nav.grp.size.toUInt64 colOff.toUInt64
      r0.toUInt64 r1.toUInt64 nav.row.cur.val.toUInt64 nav.curColIdx.toUInt64
      moveDir.toInt64 nav.selColIdxs nav.row.sels st precAdj.toInt64 widthAdj.toInt64

namespace MemTable

-- | Filter: no-op for MemTable (in-memory filter not yet impl)
def filter (t : MemTable) (_ : String) : IO (Option MemTable) := pure (some t)

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

end MemTable
end Tc
