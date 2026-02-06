/-
  kdb backend: KdbTable with q query support
-/
import Tc.Data.Kdb.FFI
import Tc.Data.Kdb.Q
import Tc.Error
import Tc.Render
import Tc.Term
import Tc.Types

namespace Tc

-- | Default row limit for kdb queries
def kdbLimit : Nat := 1000

-- | Parse float string "3.14", "-1.5", "1e10", etc. (no stdlib support)
def parseFloat (s : String) : Option Float := do
  let s := s.trimAscii.toString
  guard (!s.isEmpty)
  let (neg, s) := if s.startsWith "-" then (true, s.drop 1) else (false, s)
  let parts := s.toString.splitOn "e"  -- handle scientific notation
  let (mant, exp) := (parts.head?.getD "", (parts.getD 1 "0").toInt?.getD 0)
  let dp := mant.splitOn "."  -- decimal point
  let whole := (dp.getD 0 "").toNat?.getD 0
  let (frac, fracLen) := if dp.length > 1 then
    let f := dp.getD 1 ""; (f.toNat?.getD 0, f.length)
  else (0, 0)
  let m := whole.toFloat + frac.toFloat / (10 ^ fracLen).toFloat
  let v := m * (10 : Float) ^ (Float.ofInt exp)
  some (if neg then -v else v)

#guard parseFloat "3.14" == some 3.14
#guard parseFloat "-1.5" == some (-1.5)
#guard parseFloat "100" == some 100.0
#guard parseFloat "" == none

-- | kdb table with cached metadata and query
structure KdbTable where
  qr        : Kdb.QueryResult   -- K object (opaque, C memory)
  colNames  : Array String      -- cached column names
  colTypes  : Array Char        -- cached type chars (j,f,s,...)
  nRows     : Nat               -- rows in current result (≤ kdbLimit)
  nCols     : Nat
  query     : Q.Query           -- q query (table + ops)
  totalRows : Nat               -- total rows in underlying table

namespace KdbTable

-- | Parse kdb://host:port/table URL → (host, port, table)
def parseUrl (s : String) : Option (String × UInt16 × String) := do
  guard (s.startsWith "kdb://")
  let p := (s.drop 6).toString.splitOn "/"  -- drop "kdb://", split on /
  guard (p.length > 1)
  let hostPort := p.getD 0 ""
  let tbl := p.getD 1 ""
  let hp := hostPort.splitOn ":"    -- host:port
  let host := hp.getD 0 ""
  guard (!host.isEmpty && !tbl.isEmpty)
  some (host, ((hp.getD 1 "5001").toNat?.getD 5001).toUInt16, tbl)

#guard parseUrl "kdb://localhost:8888/nbbo" == some ("localhost", 8888, "nbbo")
#guard parseUrl "kdb://host/tbl" == some ("host", 5001, "tbl")  -- default port
#guard parseUrl "http://x/y" == none  -- wrong scheme
#guard parseUrl "kdb://host" == none  -- no table

-- | Connect to kdb server
def connect (host : String) (port : UInt16) : IO Bool := Kdb.connect host port

-- | Disconnect from server
def disconnect : IO Unit := Kdb.disconnect

-- | Check connection
def connected : IO Bool := Kdb.connected

-- | Build KdbTable from QueryResult
def ofQueryResult (qr : Kdb.QueryResult) (query : Q.Query) (total : Nat := 0) : IO KdbTable := do
  let nc ← Kdb.ncols qr
  let nr ← Kdb.nrows qr
  let mut names : Array String := #[]
  let mut types : Array Char := #[]
  for i in [:nc.toNat] do
    names := names.push (← Kdb.colName qr i.toUInt64)
    types := types.push (← Kdb.colType qr i.toUInt64)
  pure ⟨qr, names, types, nr.toNat, nc.toNat, query, total⟩

-- | Fetch cells [r0,r1) as strings
private def fetchCells (qr : Kdb.QueryResult) (col r0 r1 : Nat) : IO (Array String) := do
  let mut a := #[]
  for r in [r0:r1] do a := a.push (← Kdb.cellStr qr r.toUInt64 col.toUInt64)
  pure a

-- | Extract column slice [r0, r1) as typed Column
def getCol (t : KdbTable) (col r0 r1 : Nat) : IO Column := do
  let cells ← fetchCells t.qr col r0 r1
  match t.colTypes.getD col '?' with
  | 'j' | 'i' | 'h' => pure (.ints (cells.map fun s => (s.toInt?.getD 0).toInt64))
  | 'f' | 'e' => pure (.floats (cells.map fun s => (parseFloat s).getD 0))
  | _ => pure (.strs cells)

-- | Check if table is partitioned via .Q.qp
def isPartitioned (tblName : String) : IO Bool := do
  let qr ← Kdb.query s!".Q.qp {tblName}"
  let nr ← Kdb.nrows qr
  if nr.toNat > 0 then
    let s ← Kdb.cellStr qr 0 0
    return s == "1"
  return false

-- | Query total row count (handles partitioned tables)
def queryCount (tblName : String) (part : Bool) : IO Nat := do
  let q := if part then s!"count select from {tblName} where date=max date" else Q.count tblName
  let qr ← Kdb.query q
  let nr ← Kdb.nrows qr
  if nr.toNat > 0 then
    let s ← Kdb.cellStr qr 0 0
    return s.toNat?.getD 0
  return 0

-- | Execute q query and return new KdbTable
def requery (query : Q.Query) (total : Nat := 0) : IO (Option KdbTable) := do
  let q := query.render kdbLimit
  Log.write "q" q
  let qr ← Kdb.query q
  some <$> ofQueryResult qr query total

-- | Load table from kdb server
def load (tblName : String) : IO (Option KdbTable) := do
  let part ← isPartitioned tblName
  let total ← queryCount tblName part
  -- use i< for row limit (works for both partitioned and in-memory)
  let tbl := if part
    then s!"select from {tblName} where date=max date, i<{kdbLimit}"
    else s!"select from {tblName} where i<{kdbLimit}"
  let query : Q.Query := { tbl := tbl }
  requery query total

-- | Open from kdb:// URL
def fromUrl (url : String) : IO (Option KdbTable) := do
  match parseUrl url with
  | none => pure none
  | some (host, port, tbl) =>
    let ok ← connect host port
    if !ok then return none
    load tbl

-- | Sort by columns (all same direction)
def sortBy (t : KdbTable) (idxs : Array Nat) (asc : Bool) : IO KdbTable := do
  let sortCols := idxs.map fun idx => (t.colNames.getD idx "", asc)
  match ← requery (t.query.pipe (.sort sortCols)) t.totalRows with
  | some t' => pure t'
  | none => pure t

-- | Delete columns (select remaining)
def delCols (t : KdbTable) (delIdxs : Array Nat) : IO KdbTable := do
  match ← requery (t.query.pipe (.sel (keepCols t.nCols delIdxs t.colNames))) t.totalRows with
  | some t' => pure t' | none => pure t

-- | Wrap select expr in parens for use in from clause
def wrapTbl (tbl : String) : String :=
  if tbl.startsWith "select" then s!"({tbl})" else tbl

-- | Extract partition filter from query (e.g. "date=max date" from "select from t where date=max date, i<1000")
def extractPartFilter (tbl : String) : String :=
  match tbl.splitOn "where " with
  | [_, rest] =>
    -- take up to first comma that's followed by i< (row limit)
    match rest.splitOn ", i<" with
    | [filt, _] => filt
    | _ => match rest.splitOn ",i<" with
      | [filt, _] => filt
      | _ => ""
  | _ => ""

-- | Extract table name from query
def extractTblName (tbl : String) : String :=
  -- "select from nbbo where ..." -> "nbbo"
  let parts := tbl.splitOn " "
  match parts.findIdx? (· == "from") with
  | some i => parts.getD (i + 1) "t"
  | none => parts.getLastD "t"

-- | Freq: group by + count (partition-aware)
def queryFreq (t : KdbTable) (colNames : Array String) : IO FreqResult := do
  if colNames.isEmpty then return emptyFreq
  let cols := colNames.toList |> String.intercalate ","
  let tblName := extractTblName t.query.tbl
  let partFilt := extractPartFilter t.query.tbl
  let whr := if partFilt.isEmpty then "" else s!" where {partFilt}"
  let q := s!"select Cnt:count i by {cols} from {tblName}{whr}"
  Log.write "q-freq" q
  let qr ← Kdb.query q
  let nr ← Kdb.nrows qr
  -- build key columns using sequential indices (result: key cols 0..n-1, then Cnt)
  let mut keyCols : Array Column := #[]
  for idx in [:colNames.size] do
    let mut vals : Array String := #[]
    for r in [:nr.toNat] do
      vals := vals.push (← Kdb.cellStr qr r.toUInt64 idx.toUInt64)
    keyCols := keyCols.push (Column.strs vals)
  let mut cntData : Array Int64 := #[]
  for r in [:nr.toNat] do
    let v ← Kdb.cellStr qr r.toUInt64 colNames.size.toUInt64
    cntData := cntData.push (v.toInt?.getD 0).toInt64
  let fs := freqStats cntData
  let pctData := fs.val.1
  let barData := fs.val.2
  let hData : cntData.size = pctData.size ∧ pctData.size = barData.size :=
    ⟨fs.property.1.symm, by rw [fs.property.1]; exact fs.property.2.symm⟩
  if hKeys : colNames.size = keyCols.size then
    pure ⟨colNames, keyCols, cntData, pctData, barData, nr.toNat, hKeys, hData⟩
  else return emptyFreq

-- | Filter: requery with filter expr
def filter (t : KdbTable) (expr : String) : IO (Option KdbTable) := do
  let q := t.query.filter expr
  let countQ := s!"count select from {wrapTbl t.query.tbl} where {expr}"
  let cntQr ← Kdb.query countQ
  let total ← do
    let nr ← Kdb.nrows cntQr
    if nr.toNat > 0 then
      let s ← Kdb.cellStr cntQr 0 0
      pure (s.toNat?.getD 0)
    else pure 0
  requery q total

-- | Distinct values for column
def distinct (t : KdbTable) (col : Nat) : IO (Array String) := do
  let colName := t.colNames.getD col ""
  let q := s!"distinct {wrapTbl t.query.tbl}[`{colName}]"
  Log.write "q" q
  let qr ← Kdb.query q
  let nr ← Kdb.nrows qr
  let mut result : Array String := #[]
  for i in [:nr.toNat] do
    result := result.push (← Kdb.cellStr qr i.toUInt64 0)
  pure result

-- | Find row (search disabled for kdb - remote data)
def findRow (_ : KdbTable) (_ : Nat) (_ : String) (_ : Nat) (_ : Bool) : IO (Option Nat) :=
  pure none

-- | Query meta: column stats (name, type, cnt, dist, null%, min, max)
-- Uses q `meta` for names/types, basic stats for rest
def queryMeta (t : KdbTable) : IO MetaTuple := do
  let names := t.colNames
  let types := t.colTypes.map String.singleton
  let cnt := t.totalRows.toInt64
  let n := names.size
  let cnts := #[].append (Array.range n |>.map fun _ => cnt)
  -- dists/nulls/mins/maxs: expensive, use placeholders
  let dists := #[].append (Array.range n |>.map fun _ => (0 : Int64))
  let nulls := #[].append (Array.range n |>.map fun _ => (0 : Int64))
  let mins := #[].append (Array.range n |>.map fun _ => "")
  let maxs := #[].append (Array.range n |>.map fun _ => "")
  pure (names, types, cnts, dists, nulls, mins, maxs)

end KdbTable

-- NOTE: Table/ModifyTable/ExecOp instances for KdbTable
-- are defined in Table.lean which imports all query methods

end Tc
