/-
  Table variant: MemTable + AdbcTable (DuckDB build, no Kdb)
-/
import Tc.Data.Mem.Table
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq
import Tc.Data.ADBC.Table
import Tc.Data.ADBC.Meta

namespace Tc

-- | TblOps instance for MemTable (read + query + render)
instance : TblOps MemTable where
  nRows     := MemTable.nRows
  colNames  := (·.names)
  queryMeta := MemTable.queryMeta
  queryFreq := MemTable.queryFreq
  filter    := MemTable.filter
  distinct  := MemTable.distinct
  findRow   := MemTable.findRow
  render t cols names _ inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj :=
    let c := if cols.isEmpty then t.cols else cols
    let n := if names.isEmpty then t.names else names
    Term.renderTable c n #[] inWidths dispIdxs
      (MemTable.nRows t).toUInt64 nGrp.toUInt64 colOff.toUInt64
      r0.toUInt64 r1.toUInt64 curRow.toUInt64 curCol.toUInt64
      moveDir.toInt64 selColIdxs rowSels st precAdj.toInt64 widthAdj.toInt64

-- | ModifyTable instance for MemTable
instance : ModifyTable MemTable where
  delCols := fun delIdxs t => pure
    { names := let keepIdxs := (Array.range t.names.size).filter (!delIdxs.contains ·)
               keepIdxs.map fun i => t.names.getD i ""
      cols  := let keepIdxs := (Array.range t.names.size).filter (!delIdxs.contains ·)
               keepIdxs.map fun i => t.cols.getD i default }
  sortBy := fun idxs asc t => pure (MemTable.sort t idxs asc)

-- | TblOps instance for AdbcTable
instance : TblOps AdbcTable where
  nRows     := (·.nRows)
  colNames  := (·.colNames)
  totalRows := (·.totalRows)
  isAdbc    := fun _ => true
  queryMeta := AdbcTable.queryMeta
  queryFreq := AdbcTable.queryFreq
  filter    := AdbcTable.filter
  distinct  := AdbcTable.distinct
  findRow   := AdbcTable.findRow
  render t _ _ _ inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj := do
    if inWidths.isEmpty then
      let cols ← (Array.range t.nCols).mapM fun c => t.getCol c 0 t.nRows
      Term.renderTable cols t.colNames t.colFmts inWidths dispIdxs
        t.nRows.toUInt64 nGrp.toUInt64 colOff.toUInt64
        0 t.nRows.toUInt64 curRow.toUInt64 curCol.toUInt64
        moveDir.toInt64 selColIdxs rowSels st precAdj.toInt64 widthAdj.toInt64
    else
      let cols ← (Array.range t.nCols).mapM fun c => t.getCol c r0 r1
      let adjCur := curRow - r0
      let adjSel := rowSels.filterMap fun r =>
        if r >= r0 && r < r1 then some (r - r0) else none
      Term.renderTable cols t.colNames t.colFmts inWidths dispIdxs
        t.nRows.toUInt64 nGrp.toUInt64 colOff.toUInt64
        0 (r1 - r0).toUInt64 adjCur.toUInt64 curCol.toUInt64
        moveDir.toInt64 selColIdxs adjSel st precAdj.toInt64 widthAdj.toInt64

-- | ModifyTable instance for AdbcTable
instance : ModifyTable AdbcTable where
  delCols := fun delIdxs t => AdbcTable.delCols t delIdxs
  sortBy  := fun idxs asc t => AdbcTable.sortBy t idxs asc

-- | Table type: mem or adbc (no kdb)
inductive Table where
  | mem  : MemTable → Table
  | adbc : AdbcTable → Table

namespace Table

-- | Extract MemTable (for meta/freq views)
def asMem? : Table → Option MemTable
  | .mem t => some t
  | _ => none

-- | Check if DB-backed
def isAdbc : Table → Bool
  | .adbc _ => true
  | .mem _ => false

-- | MemConvert instance (MemTable ↔ Table)
instance : MemConvert MemTable Table where
  wrap   := Table.mem
  unwrap := Table.asMem?

-- | Load from file (CSV via Mem, parquet via ADBC)
def fromFile (path : String) : IO (Option Table) := do
  if path.endsWith ".csv" then
    match ← MemTable.load path with
    | .ok t => pure (some (.mem t))
    | .error _ => pure none
  else
    match ← AdbcTable.fromFile path with
    | some t => pure (some (.adbc t))
    | none => pure none

-- | Load from URL (kdb not supported in DuckDB build)
def fromUrl (url : String) : IO (Option Table) := do
  Log.error s!"tc-duckdb: kdb:// not supported: {url}"; pure none

-- | TblOps instance (includes query ops + render)
instance : TblOps Table where
  nRows
    | .mem t => MemTable.nRows t
    | .adbc t => AdbcTable.nRows t
  colNames
    | .mem t => t.names
    | .adbc t => AdbcTable.colNames t
  totalRows
    | .mem t => MemTable.nRows t
    | .adbc t => AdbcTable.totalRows t
  isAdbc := Table.isAdbc
  queryMeta
    | .mem t => MemTable.queryMeta t
    | .adbc t => AdbcTable.queryMeta t
  queryFreq tbl idxs := match tbl with
    | .mem t => MemTable.queryFreq t idxs
    | .adbc t => AdbcTable.queryFreq t idxs
  filter tbl expr := match tbl with
    | .mem t => MemTable.filter t expr <&> (·.map .mem)
    | .adbc t => AdbcTable.filter t expr <&> (·.map .adbc)
  distinct tbl col := match tbl with
    | .mem t => MemTable.distinct t col
    | .adbc t => AdbcTable.distinct t col
  findRow tbl col val start fwd := match tbl with
    | .mem t => MemTable.findRow t col val start fwd
    | .adbc t => AdbcTable.findRow t col val start fwd
  render tbl _ _ _ inWidths dispIdxs nGrp colOff r0 r1 curRow curCol moveDir selColIdxs rowSels st precAdj widthAdj :=
    match tbl with
    | .mem t =>
      Term.renderTable t.cols t.names #[] inWidths dispIdxs
        (MemTable.nRows t).toUInt64 nGrp.toUInt64 colOff.toUInt64
        r0.toUInt64 r1.toUInt64 curRow.toUInt64 curCol.toUInt64
        moveDir.toInt64 selColIdxs rowSels st precAdj.toInt64 widthAdj.toInt64
    | .adbc t => do
      if inWidths.isEmpty then
        let cols ← (Array.range t.nCols).mapM fun c => t.getCol c 0 t.nRows
        Term.renderTable cols t.colNames t.colFmts inWidths dispIdxs
          t.nRows.toUInt64 nGrp.toUInt64 colOff.toUInt64
          0 t.nRows.toUInt64 curRow.toUInt64 curCol.toUInt64
          moveDir.toInt64 selColIdxs rowSels st precAdj.toInt64 widthAdj.toInt64
      else
        let cols ← (Array.range t.nCols).mapM fun c => t.getCol c r0 r1
        let adjCur := curRow - r0
        let adjSel := rowSels.filterMap fun r =>
          if r >= r0 && r < r1 then some (r - r0) else none
        Term.renderTable cols t.colNames t.colFmts inWidths dispIdxs
          t.nRows.toUInt64 nGrp.toUInt64 colOff.toUInt64
          0 (r1 - r0).toUInt64 adjCur.toUInt64 curCol.toUInt64
          moveDir.toInt64 selColIdxs adjSel st precAdj.toInt64 widthAdj.toInt64
  fromFile := Table.fromFile

-- | ModifyTable instance
instance : ModifyTable Table where
  delCols idxs
    | .mem t => .mem <$> ModifyTable.delCols idxs t
    | .adbc t => .adbc <$> ModifyTable.delCols idxs t
  sortBy idxs asc
    | .mem t => .mem <$> ModifyTable.sortBy idxs asc t
    | .adbc t => .adbc <$> ModifyTable.sortBy idxs asc t

-- | ExecOp instance for Table
instance : ExecOp Table where
  exec tbl op := match tbl with
    | .mem t => ExecOp.exec t op <&> (·.map .mem)
    | .adbc t => ExecOp.exec t op <&> (·.map .adbc)

-- | Format table as plain text
def toText : Table → IO String
  | .mem t => pure (MemTable.toText t)
  | .adbc t => do
    let nr := t.nRows; let nc := t.nCols
    let cols ← (Array.range nc).mapM fun c => t.getCol c 0 nr
    let mut lines : Array String := #["\t".intercalate t.colNames.toList]
    for r in [:nr] do
      let row := cols.map fun col => (col.get r).toRaw
      lines := lines.push ("\t".intercalate row.toList)
    pure ("\n".intercalate lines.toList)

end Table

-- | Backend lifecycle for DuckDB build
namespace Backend
def init : IO Bool := AdbcTable.init
def shutdown : IO Unit := AdbcTable.shutdown
end Backend

end Tc
