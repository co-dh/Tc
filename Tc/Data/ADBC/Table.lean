/-
  ADBC backend: AdbcTable with PRQL query support
-/
import Tc.Data.ADBC.FFI
import Tc.Data.ADBC.Prql
import Tc.Error
import Tc.Render
import Tc.Term
import Tc.Types

namespace Tc

-- | Default row limit for PRQL queries
def prqlLimit : Nat := 1000

-- | Zero-copy table with PRQL query for re-query on modification
structure AdbcTable where
  qr        : Adbc.QueryResult  -- arrow data (opaque, C memory)
  colNames  : Array String      -- cached column names
  colFmts   : Array Char        -- cached format chars per column
  nRows     : Nat               -- rows in current result (≤ prqlLimit)
  nCols     : Nat
  query     : Prql.Query        -- PRQL query (base + ops)
  totalRows : Nat               -- total rows in underlying data

namespace AdbcTable

-- | Build AdbcTable from QueryResult
def ofQueryResult (qr : Adbc.QueryResult) (query : Prql.Query := default) (total : Nat := 0) : IO AdbcTable := do
  let nc ← Adbc.ncols qr
  let nr ← Adbc.nrows qr
  let mut names : Array String := #[]
  let mut fmts : Array Char := #[]
  for i in [:nc.toNat] do
    let n ← Adbc.colName qr i.toUInt64
    names := names.push n
    let fmt ← Adbc.colFmt qr i.toUInt64
    fmts := fmts.push (if h : fmt.length > 0 then fmt.toList[0] else '?')
  pure ⟨qr, names, fmts, nr.toNat, nc.toNat, query, total⟩

-- | Get cell at (row, col) - pure interface via unsafeIO
@[inline] unsafe def getIdxImpl (t : AdbcTable) (row col : Nat) : Cell :=
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
def getIdx (t : AdbcTable) (_ _ : Nat) : Cell := .null

-- | Get DisplayInfo
def info (t : AdbcTable) : DisplayInfo :=
  ⟨t.colNames, t.nRows, t.nCols⟩

-- | Extract column slice [r0, r1) as typed Column
@[inline] unsafe def getColImpl (t : AdbcTable) (col r0 r1 : Nat) : Column :=
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
def getCol (_ : AdbcTable) (_ _ _ : Nat) : Column := .strs #[]

-- | Query total row count using cnt function
def queryCount (query : Prql.Query) : IO Nat := do
  let prql := s!"{query.render} | cnt"
  Log.write "prql" prql
  let some sql ← Prql.compile prql | return 0
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  if nr.toNat > 0 then
    let v ← Adbc.cellInt qr 0 0
    return v.toNat
  return 0

-- | Execute PRQL query and return new AdbcTable (preserves totalRows if provided)
def requery (query : Prql.Query) (total : Nat := 0) : IO (Option AdbcTable) := do
  let prql := s!"{query.render} | take {prqlLimit}"
  Log.write "prql" prql
  let some sql ← Prql.compile prql | return none
  let qr ← Adbc.query sql
  some <$> ofQueryResult qr query total

-- | Create from file path (queries total count)
def fromFile (path : String) : IO (Option AdbcTable) := do
  let query : Prql.Query := { base := s!"from `{path}`" }
  let total ← queryCount query
  requery query total

-- | Sort: append sort op and re-query (group cols asc, last col uses given asc)
@[inline] unsafe def sortByImpl (t : AdbcTable) (idxs : Array Nat) (asc : Bool) : AdbcTable :=
  let n := idxs.size
  let sortCols := idxs.mapIdx fun i idx =>
    let name := t.colNames.getD idx ""
    let isLast := i + 1 == n
    (name, if isLast then asc else true)
  let newQuery := t.query.pipe (.sort sortCols)
  match unsafeIO (requery newQuery t.totalRows) with
  | .ok (some t') => t'
  | _ => t

@[implemented_by sortByImpl]
def sortBy (_ : AdbcTable) (_ : Array Nat) (_ : Bool) : AdbcTable := sorry

-- | Delete columns: append select op and re-query
@[inline] unsafe def delColsImpl (t : AdbcTable) (delIdxs : Array Nat) : AdbcTable :=
  let keepCols := (Array.range t.nCols).filter (!delIdxs.contains ·)
    |>.map fun i => t.colNames.getD i ""
  let newQuery := t.query.pipe (.sel keepCols)
  match unsafeIO (requery newQuery t.totalRows) with
  | .ok (some t') => t'
  | _ => t

@[implemented_by delColsImpl]
def delCols (_ : AdbcTable) (_ : Array Nat) : AdbcTable := sorry

end AdbcTable

-- | ReadTable instance for AdbcTable
instance : ReadTable AdbcTable where
  nRows     := (·.nRows)
  colNames  := (·.colNames)
  totalRows := (·.totalRows)

-- | ModifyTable instance for AdbcTable
instance : ModifyTable AdbcTable where
  delCols := fun delIdxs t => AdbcTable.delCols t delIdxs
  sortBy  := fun idxs asc t => AdbcTable.sortBy t idxs asc

-- | RenderTable instance for AdbcTable
instance : RenderTable AdbcTable where
  render nav inWidths colOff r0 r1 moveDir st precAdj widthAdj := do
    if inWidths.isEmpty then
      let cols := (Array.range nav.tbl.nCols).map fun c => nav.tbl.getCol c 0 nav.tbl.nRows
      Term.renderTable cols nav.tbl.colNames nav.tbl.colFmts inWidths nav.dispColIdxs
        nav.tbl.nRows.toUInt64 nav.nKeys.toUInt64 colOff.toUInt64
        0 nav.tbl.nRows.toUInt64 nav.curRow.toUInt64 nav.curColIdx.toUInt64
        moveDir.toInt64 nav.selColIdxs nav.selRows st precAdj.toInt64 widthAdj.toInt64
    else
      let cols := (Array.range nav.tbl.nCols).map fun c => nav.tbl.getCol c r0 r1
      let adjCur := nav.curRow - r0
      let adjSel := nav.selRows.filterMap fun r =>
        if r >= r0 && r < r1 then some (r - r0) else none
      Term.renderTable cols nav.tbl.colNames nav.tbl.colFmts inWidths nav.dispColIdxs
        nav.tbl.nRows.toUInt64 nav.nKeys.toUInt64 colOff.toUInt64
        0 (r1 - r0).toUInt64 adjCur.toUInt64 nav.curColIdx.toUInt64
        moveDir.toInt64 nav.selColIdxs adjSel st precAdj.toInt64 widthAdj.toInt64

end Tc
