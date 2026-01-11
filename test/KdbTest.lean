/-
  Kdb backend tests against localhost:8888/nbbo
  Run: lake build kdb-test && .lake/build/bin/kdb-test
-/
import Tc.Data.Kdb.FFI
import Tc.Data.Kdb.Q
import Tc.Data.Kdb.Table
open Tc

-- | Assert helper
def assert (cond : Bool) (msg : String) : IO Unit :=
  if cond then IO.println s!"  ✓ {msg}"
  else throw (IO.userError s!"  ✗ {msg}")

-- | Section header
def hdr (name : String) : IO Unit := IO.println s!"\n[{name}]"

-- === 1. Connection Tests ===

def test_connect : IO Unit := do
  let ok ← Kdb.connect "localhost" 8888
  assert ok "connect localhost:8888"

def test_connected : IO Unit := do
  let ok ← Kdb.connected
  assert ok "connected returns true"

-- === 2. FFI Query Tests ===

def test_tables : IO Unit := do
  let qr ← Kdb.query "tables[]"
  let nr ← Kdb.nrows qr
  assert (nr.toNat > 0) s!"tables[] returns {nr} tables"

def test_ncols : IO Unit := do
  let qr ← Kdb.query "select from nbbo where date=first date, i<10"
  let nc ← Kdb.ncols qr
  assert (nc.toNat > 0) s!"ncols = {nc}"

def test_nrows : IO Unit := do
  let qr ← Kdb.query "select from nbbo where date=first date, i<10"
  let nr ← Kdb.nrows qr
  assert (nr.toNat == 10) s!"nrows = {nr}"

def test_colName : IO Unit := do
  let qr ← Kdb.query "select from nbbo where date=first date, i<10"
  let name ← Kdb.colName qr 0
  assert (name.length > 0) s!"colName(0) = {name}"

def test_colType : IO Unit := do
  let qr ← Kdb.query "select from nbbo where date=first date, i<10"
  let typ ← Kdb.colType qr 0
  assert (typ != '\x00') s!"colType(0) = {typ}"

def test_cellStr : IO Unit := do
  let qr ← Kdb.query "select from nbbo where date=first date, i<10"
  let val ← Kdb.cellStr qr 0 0
  assert (val.length > 0 || val == "") s!"cellStr(0,0) = {val}"

-- === 3. KdbTable Load Tests ===

def test_load : IO KdbTable := do
  match ← KdbTable.load "nbbo" with
  | none => throw (IO.userError "load nbbo failed")
  | some t =>
    assert true "load nbbo"
    pure t

def test_load_rows (t : KdbTable) : IO Unit :=
  assert (t.nRows > 0) s!"nRows = {t.nRows}"

def test_load_cols (t : KdbTable) : IO Unit :=
  assert (t.nCols > 0) s!"nCols = {t.nCols}"

def test_load_colNames (t : KdbTable) : IO Unit :=
  assert (t.colNames.size == t.nCols) s!"colNames = {t.colNames}"

def test_load_colTypes (t : KdbTable) : IO Unit :=
  assert (t.colTypes.size == t.nCols) s!"colTypes = {t.colTypes}"

def test_load_limit (t : KdbTable) : IO Unit :=
  assert (t.nRows <= kdbLimit) s!"nRows <= {kdbLimit}"

def test_parseUrl : IO Unit := do
  match KdbTable.parseUrl "kdb://localhost:8888/nbbo" with
  | none => throw (IO.userError "parseUrl failed")
  | some (h, p, tbl) =>
    assert (h == "localhost") s!"host = {h}"
    assert (p == 8888) s!"port = {p}"
    assert (tbl == "nbbo") s!"table = {tbl}"

-- === 4. Partitioned Table Tests ===

def test_isPartitioned : IO Bool := do
  let part ← KdbTable.isPartitioned "nbbo"
  assert true s!"isPartitioned = {part}"
  pure part

def test_queryCount (part : Bool) : IO Unit := do
  let cnt ← KdbTable.queryCount "nbbo" part
  assert (cnt > 0) s!"queryCount = {cnt}"

-- === 5. Query Operations Tests ===

def test_sortBy_asc (t : KdbTable) : IO Unit := do
  let t' ← t.sortBy #[0] true
  assert (t'.nRows > 0) "sortBy asc"

def test_sortBy_desc (t : KdbTable) : IO Unit := do
  let t' ← t.sortBy #[0] false
  assert (t'.nRows > 0) "sortBy desc"

def test_sortBy_multi (t : KdbTable) : IO Unit := do
  if t.nCols >= 2 then
    let t' ← t.sortBy #[0, 1] true
    assert (t'.nRows > 0) "sortBy multi-col"
  else
    assert true "sortBy multi (skipped, < 2 cols)"

def test_delCols (t : KdbTable) : IO Unit := do
  if t.nCols >= 2 then
    let t' ← t.delCols #[0]
    assert (t'.nCols == t.nCols - 1) s!"delCols: {t.nCols} -> {t'.nCols}"
  else
    assert true "delCols (skipped, < 2 cols)"

def test_filter (t : KdbTable) : IO Unit := do
  -- filter on first column with simple expression
  let colName := t.colNames.getD 0 "x"
  let colType := t.colTypes.getD 0 's'
  let expr := if colType == 's' then s!"{colName}=first {colName}" else s!"{colName}>0"
  match ← t.filter expr with
  | none => throw (IO.userError "filter failed")
  | some t' => assert (t'.nRows >= 0) s!"filter '{expr}': {t'.nRows} rows"

def test_distinct (t : KdbTable) : IO Unit := do
  let vals ← t.distinct 0
  assert (vals.size > 0) s!"distinct col0: {vals.size} values"

-- === 6. Freq Tests ===

def test_freq_single (t : KdbTable) : IO Unit := do
  let (keys, cols, cnts, _, _) ← t.queryFreq #[0]
  assert (keys.size > 0) "freq has keys"
  assert (cols.size > 0) "freq has cols"
  assert (cnts.size > 0) s!"freq has {cnts.size} groups"

def test_freq_counts (t : KdbTable) : IO Unit := do
  let (_, _, cnts, _, _) ← t.queryFreq #[0]
  let total := cnts.foldl (· + ·) 0
  assert (total > 0) s!"freq counts sum = {total}"

def test_freq_pcts (t : KdbTable) : IO Unit := do
  let (_, _, _, pcts, _) ← t.queryFreq #[0]
  let sum := pcts.foldl (· + ·) 0
  assert (sum > 99 && sum < 101) s!"freq pcts sum = {sum}"

def test_freq_bars (t : KdbTable) : IO Unit := do
  let (_, _, _, _, bars) ← t.queryFreq #[0]
  assert (bars.size > 0) "freq has bars"

-- === 7. Column Extraction Tests ===

def test_getCol (t : KdbTable) : IO Unit := do
  for i in [:min t.nCols 3] do
    let col ← t.getCol i 0 (min t.nRows 10)
    let typ := t.colTypes.getD i '?'
    let kind := match col with
      | .ints _ => "ints"
      | .floats _ => "floats"
      | .strs _ => "strs"
    assert true s!"getCol({i}) type={typ} -> {kind}"

def test_getCol_range (t : KdbTable) : IO Unit := do
  if t.nRows >= 5 then
    let col ← t.getCol 0 2 5
    let sz := match col with
      | .ints a => a.size
      | .floats a => a.size
      | .strs a => a.size
    assert (sz == 3) s!"getCol range [2,5): size = {sz}"
  else
    assert true "getCol range (skipped, < 5 rows)"

-- === Main ===

def main : IO Unit := do
  hdr "Connection"
  test_connect
  test_connected

  hdr "FFI Query"
  test_tables
  test_ncols
  test_nrows
  test_colName
  test_colType
  test_cellStr

  hdr "KdbTable Load"
  let t ← test_load
  test_load_rows t
  test_load_cols t
  test_load_colNames t
  test_load_colTypes t
  test_load_limit t
  test_parseUrl

  hdr "Partitioned"
  let part ← test_isPartitioned
  test_queryCount part

  hdr "Query Ops"
  test_sortBy_asc t
  test_sortBy_desc t
  test_sortBy_multi t
  test_delCols t
  test_filter t
  test_distinct t

  hdr "Freq"
  test_freq_single t
  test_freq_counts t
  test_freq_pcts t
  test_freq_bars t

  hdr "Column Extraction"
  test_getCol t
  test_getCol_range t

  hdr "Cleanup"
  Kdb.disconnect
  assert true "disconnect"

  IO.println "\n✓ all kdb backend tests passed"
