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

-- | Init ADBC backend (DuckDB + shellfs extension)
def init : IO Bool := do
  let ok ← Adbc.init
  if ok then let _ ← Adbc.query "LOAD shellfs"
  pure ok

-- | Shutdown ADBC backend
def shutdown : IO Unit := Adbc.shutdown

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

-- | Extract column slice [r0, r1) as typed Column
def getCol (t : AdbcTable) (col r0 r1 : Nat) : IO Column := do
  let fmt := t.colFmts.getD col '?'
  match fmt with
  | 'l' | 'i' | 's' | 'c' | 'L' | 'I' | 'S' | 'C' =>
    let mut arr : Array Int64 := #[]
    for r in [r0:r1] do
      arr := arr.push (← Adbc.cellInt t.qr r.toUInt64 col.toUInt64).toInt64
    pure (.ints arr)
  | 'g' | 'f' | 'd' =>
    let mut arr : Array Float := #[]
    for r in [r0:r1] do
      arr := arr.push (← Adbc.cellFloat t.qr r.toUInt64 col.toUInt64)
    pure (.floats arr)
  | _ =>
    let mut arr : Array String := #[]
    for r in [r0:r1] do
      arr := arr.push (← Adbc.cellStr t.qr r.toUInt64 col.toUInt64)
    pure (.strs arr)

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
def sortBy (t : AdbcTable) (idxs : Array Nat) (asc : Bool) : IO AdbcTable := do
  let n := idxs.size
  let sortCols := idxs.mapIdx fun i idx =>
    let name := t.colNames.getD idx ""
    let isLast := i + 1 == n
    (name, if isLast then asc else true)
  let newQuery := t.query.pipe (.sort sortCols)
  match ← requery newQuery t.totalRows with
  | some t' => pure t'
  | none => pure t

-- | Delete columns: append select op and re-query
def delCols (t : AdbcTable) (delIdxs : Array Nat) : IO AdbcTable := do
  match ← requery (t.query.pipe (.sel (keepCols t.nCols delIdxs t.colNames))) t.totalRows with
  | some t' => pure t' | none => pure t

end AdbcTable

-- NOTE: ReadTable/ModifyTable/RenderTable instances for AdbcTable are defined in Table variants
-- (Table.lean, Table/DuckDB.lean) which import queryMeta from ADBC/Meta.lean

namespace AdbcTable

-- | Freq: use SQL GROUP BY, query total distinct count
def queryFreq (t : AdbcTable) (colIdxs : Array Nat) : IO FreqTuple := do
  let names := t.colNames
  let keyNames := colIdxs.map fun i => names.getD i ""
  if keyNames.isEmpty then return (#[], #[], #[], #[], #[], 0)
  let cols := keyNames.map Prql.quote |> (", ".intercalate ·.toList)
  -- query total distinct groups using cntdist function
  let cntPrql := s!"{t.query.render} | cntdist \{{cols}}"
  Log.write "prql-cntdist" cntPrql
  let totalGroups ← do
    let some sql ← Prql.compile cntPrql | pure 0
    let qr ← Adbc.query sql
    let v ← Adbc.cellStr qr 0 0
    pure (v.toNat?.getD 0)
  -- limit to top 1000 by count (cell-by-cell fetch is O(rows × cols))
  let prql := s!"{t.query.render} | group \{{cols}} (aggregate \{Cnt = std.count this}) | sort \{-Cnt} | take 1000"
  Log.write "prql-freq" prql
  let some sql ← Prql.compile prql | return (#[], #[], #[], #[], #[], 0)
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  let mut keyCols : Array Column := #[]
  for i in [:keyNames.size] do
    let mut vals : Array String := #[]
    for r in [:nr.toNat] do
      vals := vals.push (← Adbc.cellStr qr r.toUInt64 i.toUInt64)
    keyCols := keyCols.push (.strs vals)
  let mut cntData : Array Int64 := #[]
  for r in [:nr.toNat] do
    let v ← Adbc.cellStr qr r.toUInt64 keyNames.size.toUInt64
    cntData := cntData.push (v.toInt?.getD 0).toInt64
  let (pctData, barData) := freqStats cntData
  pure (keyNames, keyCols, cntData, pctData, barData, totalGroups)

-- | Filter: requery with filter (queries new filtered count)
def filter (t : AdbcTable) (expr : String) : IO (Option AdbcTable) := do
  let q := t.query.filter expr
  let total ← queryCount q
  AdbcTable.requery q total

-- | Distinct: use SQL DISTINCT
def distinct (t : AdbcTable) (col : Nat) : IO (Array String) := do
  let colName := t.colNames.getD col ""
  let prql := s!"{t.query.render} | group \{{Prql.quote colName}} (take 1) | select \{{Prql.quote colName}} | take 100"
  Log.write "prql" prql
  let some sql ← Prql.compile prql | return #[]
  let qr ← Adbc.query sql
  let nr ← Adbc.nrows qr
  let mut result : Array String := #[]
  for i in [:nr.toNat] do
    result := result.push (← Adbc.cellStr qr i.toUInt64 0)
  pure result

-- | Find row from starting position, forward or backward (with wrap)
def findRow (t : AdbcTable) (col : Nat) (val : String) (start : Nat) (fwd : Bool) : IO (Option Nat) := do
  let n := t.nRows
  if fwd then
    for i in [start:n] do
      let c ← t.getCol col i (i + 1)
      if (c.get 0).toRaw == val then return some i
    for i in [:start] do
      let c ← t.getCol col i (i + 1)
      if (c.get 0).toRaw == val then return some i
  else
    for i in [:start] do
      let idx := start - 1 - i
      let c ← t.getCol col idx (idx + 1)
      if (c.get 0).toRaw == val then return some idx
    for i in [:n - start] do
      let idx := n - 1 - i
      let c ← t.getCol col idx (idx + 1)
      if (c.get 0).toRaw == val then return some idx
  return none

end AdbcTable

-- | ExecOp instance for AdbcTable (all ops via PRQL requery)
instance : ExecOp AdbcTable where
  exec t op := AdbcTable.requery (t.query.pipe op) t.totalRows

end Tc
