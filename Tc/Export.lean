/-
  Export: save current view to file (csv/parquet/json/ndjson).
  Uses DuckDB COPY for efficient streaming export.
-/
import Tc.Data.ADBC.Ops
import Tc.Fzf
import Tc.View

namespace ExportFmt
def ext : ExportFmt → String | .csv => "csv" | .parquet => "parquet" | .json => "json" | .ndjson => "ndjson"
def copyOpt : ExportFmt → String
  | .csv => "(FORMAT CSV, HEADER true)" | .json => "(FORMAT JSON)"
  | .parquet => "(FORMAT PARQUET)" | .ndjson => "(FORMAT JSON, ARRAY false)"
def ofString? : String → Option ExportFmt
  | "csv" => some .csv | "parquet" => some .parquet | "json" => some .json | "ndjson" => some .ndjson | _ => none
end ExportFmt

namespace Tc.Export

-- | Prompt user for export format via fzf
def pickFmt : IO (Option ExportFmt) := do
  match ← Fzf.fzf #["--prompt=export: "] "csv\nparquet\njson\nndjson" with
  | some raw => pure (ExportFmt.ofString? raw.trimAscii.toString)
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
  let stem := (name.splitOn ".").head?.filter (!·.isEmpty) |>.getD name
  let home := (← IO.getEnv "HOME").getD "."
  let path := s!"{home}/tc_export_{stem}.{fmt.ext}"
  exportView s.tbl path fmt
  statusMsg s!"exported {path}"
  pure s

end Tc.Export
