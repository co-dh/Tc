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

-- | Parse kdb://host:port/table URL
-- Returns (host, port, tableName) or none if not kdb URL
def parseUrl (s : String) : Option (String × UInt16 × String) :=
  if !s.startsWith "kdb://" then none
  else
    let rest := s.drop 6  -- drop "kdb://"
    match rest.splitOn "/" with
    | [hp, tbl] =>
      match hp.splitOn ":" with
      | [h, p] => some (h, p.toNat!.toUInt16, tbl)
      | [h] => some (h, 5001, tbl)  -- default port
      | _ => none
    | hp :: tbl :: _ =>
      match hp.splitOn ":" with
      | [h, p] => some (h, p.toNat!.toUInt16, tbl)
      | [h] => some (h, 5001, tbl)
      | _ => none
    | _ => none

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

-- | Extract column slice [r0, r1) as typed Column
def getCol (t : KdbTable) (col r0 r1 : Nat) : IO Column := do
  let typ := t.colTypes.getD col '?'
  match typ with
  | 'j' | 'i' | 'h' =>  -- long, int, short
    let mut arr : Array Int64 := #[]
    for r in [r0:r1] do
      let s ← Kdb.cellStr t.qr r.toUInt64 col.toUInt64
      arr := arr.push (s.toInt?.getD 0).toInt64
    pure (.ints arr)
  | 'f' | 'e' =>  -- float, real
    let mut arr : Array Float := #[]
    for r in [r0:r1] do
      let s ← Kdb.cellStr t.qr r.toUInt64 col.toUInt64
      -- parse float: try nat, else 0 (TODO: proper float parsing)
      let v := match s.toNat? with
        | some n => n.toFloat
        | none => 0.0
      arr := arr.push v
    pure (.floats arr)
  | _ =>  -- symbol, char, temporal, etc.
    let mut arr : Array String := #[]
    for r in [r0:r1] do
      arr := arr.push (← Kdb.cellStr t.qr r.toUInt64 col.toUInt64)
    pure (.strs arr)

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
    return s.toNat!
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
    if !ok then IO.eprintln s!"kdb: cannot connect {host}:{port}"; return none
    load tbl

-- | Sort by columns
def sortBy (t : KdbTable) (idxs : Array Nat) (asc : Bool) : IO KdbTable := do
  let n := idxs.size
  let sortCols := idxs.mapIdx fun i idx =>
    let name := t.colNames.getD idx ""
    let isLast := i + 1 == n
    (name, if isLast then asc else true)
  let newQuery := t.query.pipe (.sort sortCols)
  match ← requery newQuery t.totalRows with
  | some t' => pure t'
  | none => pure t

-- | Delete columns (select remaining)
def delCols (t : KdbTable) (delIdxs : Array Nat) : IO KdbTable := do
  let keepCols := (Array.range t.nCols).filter (!delIdxs.contains ·)
    |>.map fun i => t.colNames.getD i ""
  let newQuery := t.query.pipe (.sel keepCols)
  match ← requery newQuery t.totalRows with
  | some t' => pure t'
  | none => pure t

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
  | none => parts.getLast!

-- | Freq: group by + count (partition-aware)
def queryFreq (t : KdbTable) (colIdxs : Array Nat) : IO FreqTuple := do
  let names := t.colNames
  let keyNames := colIdxs.map fun i => names.getD i ""
  if keyNames.isEmpty then return (#[], #[], #[], #[], #[])
  let cols := keyNames.toList |> String.intercalate ","
  let tblName := extractTblName t.query.tbl
  let partFilt := extractPartFilter t.query.tbl
  let whr := if partFilt.isEmpty then "" else s!" where {partFilt}"
  let q := s!"select Cnt:count i by {cols} from {tblName}{whr}"
  Log.write "q-freq" q
  let qr ← Kdb.query q
  let nr ← Kdb.nrows qr
  -- key columns + Cnt column
  let mut keyCols : Array Column := #[]
  for i in [:keyNames.size] do
    let mut vals : Array String := #[]
    for r in [:nr.toNat] do
      vals := vals.push (← Kdb.cellStr qr r.toUInt64 i.toUInt64)
    keyCols := keyCols.push (.strs vals)
  let mut cntData : Array Int64 := #[]
  for r in [:nr.toNat] do
    let v ← Kdb.cellStr qr r.toUInt64 keyNames.size.toUInt64
    cntData := cntData.push (v.toInt?.getD 0).toInt64
  let total := cntData.foldl (init := 0) (· + ·)
  let pctData := cntData.map fun c => if total > 0 then c.toFloat * 100 / total.toFloat else 0
  let barData := pctData.map fun p => String.ofList (List.replicate (p / 5).toUInt32.toNat '#')
  pure (keyNames, keyCols, cntData, pctData, barData)

-- | Filter: requery with filter expr
def filter (t : KdbTable) (expr : String) : IO (Option KdbTable) := do
  let q := t.query.filter expr
  let countQ := s!"count select from {wrapTbl t.query.tbl} where {expr}"
  let cntQr ← Kdb.query countQ
  let total ← do
    let nr ← Kdb.nrows cntQr
    if nr.toNat > 0 then
      let s ← Kdb.cellStr cntQr 0 0
      pure s.toNat!
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

-- | ReadTable instance
instance : ReadTable KdbTable where
  nRows     := (·.nRows)
  colNames  := (·.colNames)
  totalRows := (·.totalRows)

-- | ModifyTable instance
instance : ModifyTable KdbTable where
  delCols := fun delIdxs t => KdbTable.delCols t delIdxs
  sortBy  := fun idxs asc t => KdbTable.sortBy t idxs asc

-- | Max rows to render (safety bound)
def maxRenderRows : Nat := 200

-- | RenderTable instance (always use visible range r0..r1, capped to maxRenderRows)
instance : RenderTable KdbTable where
  render nav inWidths colOff r0 r1 moveDir st precAdj widthAdj := do
    -- clamp row range to prevent runaway fetching
    let r1' := min r1 (r0 + maxRenderRows)
    let cols ← (Array.range nav.tbl.nCols).mapM fun c => nav.tbl.getCol c r0 r1'
    let adjCur := nav.row.cur.val - r0
    let adjSel := nav.row.sels.filterMap fun r =>
      if r >= r0 && r < r1' then some (r - r0) else none
    Term.renderTable cols nav.tbl.colNames nav.tbl.colTypes inWidths nav.dispColIdxs
      nav.tbl.nRows.toUInt64 nav.grp.size.toUInt64 colOff.toUInt64
      0 (r1' - r0).toUInt64 adjCur.toUInt64 nav.curColIdx.toUInt64
      moveDir.toInt64 nav.selColIdxs adjSel st precAdj.toInt64 widthAdj.toInt64

-- | ExecOp instance (re-query on op)
instance : ExecOp KdbTable where
  exec t op := KdbTable.requery (t.query.pipe op) t.totalRows

end Tc
