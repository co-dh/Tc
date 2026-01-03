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

-- | Is column numeric (for right-alignment)
def isNum : Column → Bool
  | .ints _ => true
  | .floats _ => true
  | .strs _ => false

end Column

namespace Cell

-- | Format integer with comma separators
def fmtInt (n : Int64) : String :=
  let v := n.toInt
  let s := s!"{v.natAbs}"
  let chars := s.toList.reverse
  let rec go (cs : List Char) (acc : List Char) (cnt : Nat) : List Char :=
    match cs with
    | [] => acc
    | c :: rest =>
      let acc' := if cnt > 0 && cnt % 3 = 0 then c :: ',' :: acc else c :: acc
      go rest acc' (cnt + 1)
  let digits := go chars [] 0
  if v < 0 then "-" ++ String.ofList digits else String.ofList digits

-- | Theorem: fmtInt preserves digit order (no reversal)
theorem fmtInt_123 : fmtInt 123 = "123" := by native_decide
theorem fmtInt_1234 : fmtInt 1234 = "1,234" := by native_decide
theorem fmtInt_2015 : fmtInt 2015 = "2,015" := by native_decide

-- | Check if cell is numeric (for right-alignment)
def isNum : Cell → Bool
  | .int _   => true
  | .float _ => true
  | _        => false

-- | Theorem: int is numeric
theorem int_isNum (n : Int64) : (Cell.int n).isNum = true := rfl

-- | Theorem: float is numeric (must right-align)
theorem float_isNum (f : Float) : (Cell.float f).isNum = true := rfl

-- | Theorem: str is not numeric
theorem str_not_isNum (s : String) : (Cell.str s).isNum = false := rfl

-- | Format float with n decimal places
def fmtFloat (f : Float) (n : Nat) : String :=
  let s := s!"{f}"
  match s.splitOn "." with
  | [intPart, decPart] =>
    if n == 0 then intPart
    else intPart ++ "." ++ decPart.take n
  | _ => s

def toString : Cell → String
  | .null    => ""
  | .int n   => fmtInt n
  | .float f => s!"{f}"
  | .str s   => s
  | .bool b  => if b then "true" else "false"

-- | Raw string value (no formatting, for PRQL filters)
def toRaw : Cell → String
  | .null    => ""
  | .int n   => s!"{n}"
  | .float f => s!"{f}"
  | .str s   => s
  | .bool b  => if b then "true" else "false"

-- | Format cell with decimal precision
def toStringD (c : Cell) (decimals : Nat) : String :=
  match c with
  | .float f => fmtFloat f decimals
  | _ => c.toString

instance : ToString Cell where toString := toString

-- | Extract string value
def str? : Cell → Option String | .str s => some s | _ => none

-- | Extract int value
def int? : Cell → Option Int64 | .int n => some n | _ => none

end Cell

-- | DisplayInfo: metadata for navigation/PRQL building (no cell access)
structure DisplayInfo where
  colNames : Array String
  nRows    : Nat
  nCols    : Nat

-- | Zero-copy table: data stays in Arrow/C memory, accessed via FFI
structure SomeTable where
  qr       : Adbc.QueryResult   -- arrow data (opaque, C memory)
  colNames : Array String       -- cached column names
  colFmts  : Array Char         -- cached format chars per column
  nRows    : Nat
  nCols    : Nat

namespace SomeTable

-- | Build SomeTable from QueryResult (caches metadata, no cell copies)
def ofQueryResult (qr : Adbc.QueryResult) : IO SomeTable := do
  let nc ← Adbc.ncols qr
  let nr ← Adbc.nrows qr
  let mut names : Array String := #[]
  let mut fmts : Array Char := #[]
  for i in [:nc.toNat] do
    let n ← Adbc.colName qr i.toUInt64
    names := names.push n
    let fmt ← Adbc.colFmt qr i.toUInt64
    fmts := fmts.push (if h : fmt.length > 0 then fmt.toList[0] else '?')
  pure ⟨qr, names, fmts, nr.toNat, nc.toNat⟩

-- | Get cell at (row, col) - pure interface via unsafeIO
@[inline] unsafe def getIdxImpl (t : SomeTable) (row col : Nat) : Cell :=
  let r := row.toUInt64
  let c := col.toUInt64
  match unsafeIO (do
    if ← Adbc.cellIsNull t.qr r c then return Cell.null
    match t.colFmts.getD col '?' with
    | 'l' | 'i' | 's' | 'c' | 'L' | 'I' | 'S' | 'C' => return Cell.int (← Adbc.cellInt t.qr r c).toInt64
    | 'g' | 'f' | 'd' => return Cell.float (← Adbc.cellFloat t.qr r c)
    | 'b'             => return Cell.bool  ((← Adbc.cellStr t.qr r c) == "true")
    | _               => return Cell.str   (← Adbc.cellStr t.qr r c)) with
  | Except.ok cell => cell
  | Except.error _ => Cell.null

@[implemented_by getIdxImpl]
def getIdx (t : SomeTable) (_ _ : Nat) : Cell := .null

-- | Get DisplayInfo
def info (t : SomeTable) : DisplayInfo :=
  ⟨t.colNames, t.nRows, t.nCols⟩

-- | Extract column slice [r0, r1) as typed Column
-- Arrow formats: l/i/s/c = signed int64/32/16/8, L/I/S/C = unsigned, g/f = float64/32
@[inline] unsafe def getColImpl (t : SomeTable) (col r0 r1 : Nat) : Column :=
  let fmt := t.colFmts.getD col '?'
  match fmt with
  | 'l' | 'i' | 's' | 'c' | 'L' | 'I' | 'S' | 'C' =>
    let data := Id.run do
      let mut arr : Array Int64 := #[]
      for r in [r0:r1] do
        match unsafeIO (Adbc.cellInt t.qr r.toUInt64 col.toUInt64) with
        | .ok v => arr := arr.push v.toInt64
        | .error _ => arr := arr.push 0
      arr
    .ints data
  | 'g' | 'f' | 'd' =>
    let data := Id.run do
      let mut arr : Array Float := #[]
      for r in [r0:r1] do
        match unsafeIO (Adbc.cellFloat t.qr r.toUInt64 col.toUInt64) with
        | .ok v => arr := arr.push v
        | .error _ => arr := arr.push 0.0
      arr
    .floats data
  | _ =>
    let data := Id.run do
      let mut arr : Array String := #[]
      for r in [r0:r1] do
        match unsafeIO (Adbc.cellStr t.qr r.toUInt64 col.toUInt64) with
        | .ok v => arr := arr.push v
        | .error _ => arr := arr.push ""
      arr
    .strs data

@[implemented_by getColImpl]
def getCol (_ : SomeTable) (_ _ _ : Nat) : Column := .strs #[]

-- | Empty table
def empty : IO SomeTable := do
  let qr ← Adbc.query "SELECT 1 WHERE 1=0"
  ofQueryResult qr

end SomeTable

-- | All pure key operations (no IO needed)
inductive PureKey where
  -- navigation
  | j | k | l | h | g | G | _0 | _1 | dollar | c_d | c_u
  | colJump (idx : Nat)
  -- view transforms
  | asc | desc | D | I | dup | swap
  | bang | spc | incDec (inc : Bool) | q | esc
  -- views (push new view)
  | F | backslash (expr : String) | s (cols : Array String)
  | M (metaTbl : SomeTable) | pushFile (path : String)
  -- input modes
  | inputRename
  -- agg (funcs as strings: "count", "sum", "average", "min", "max", "stddev")
  | pushAgg (keys : Array String) (funcs : Array String) (cols : Array String)
  -- enter key
  | ret
