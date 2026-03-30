/-
  Export: save current view to file (csv/parquet/json/ndjson).
  Uses DuckDB COPY for efficient streaming export.
-/
import Tc.Data.ADBC.Ops
import Tc.Fzf
import Tc.View

namespace Tc.ExportFmt
def ext (f : ExportFmt) : String := toString f
def copyOpt : ExportFmt → String
  | .csv => "(FORMAT CSV, HEADER true)" | .json => "(FORMAT JSON)"
  | .parquet => "(FORMAT PARQUET)" | .ndjson => "(FORMAT JSON, ARRAY false)"
end Tc.ExportFmt

namespace Tc.Export

-- | Prompt user for export format via fzf
def pickFmt : IO (Option ExportFmt) := do
  match ← Fzf.fzf #["--prompt=export: "] "csv\nparquet\njson\nndjson" with
  | some raw => pure (raw.trimAscii.toString |> StrEnum.ofString?)
  | none => pure none

-- | Export current view to file via DuckDB COPY
def exportView (t : AdbcTable) (path : String) (fmt : ExportFmt) : IO Unit := do
  let some sql ← Prql.compile t.query.render | throw (.userError "PRQL compile failed")
  let copySql := s!"COPY ({stripSemi sql}) TO '{escSql path}' {fmt.copyOpt}"
  Log.write "export" copySql
  let _ ← Adbc.query copySql

-- | Run export effect: build path from view name, export via DuckDB COPY
def run (s : ViewStack AdbcTable) (fmt : ExportFmt) : IO (ViewStack AdbcTable) := do
  let name := s.cur.tabName.replace "/" "_" |>.replace " " "_"
  let stem := name.splitOn "." |>.head? |>.filter (!·.isEmpty) |>.getD name
  let path := s!"{← Log.dir}/tv_export_{stem}.{fmt.ext}"
  exportView s.tbl path fmt
  statusMsg s!"exported {path}"
  pure s

-- | Export by format string directly (no fzf). Called by socket/dispatch.
def runWith (s : ViewStack AdbcTable) (fmtStr : String) : IO (ViewStack AdbcTable) := do
  let some fmt := StrEnum.ofString? fmtStr | return s
  run s fmt

end Tc.Export
