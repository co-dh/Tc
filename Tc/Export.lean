/-
  Export: save current view to file (csv/parquet/json/ndjson).
  Uses DuckDB COPY for efficient streaming export.
-/
import Tc.Data.ADBC.Ops
import Tc.View

namespace Tc.Export

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
