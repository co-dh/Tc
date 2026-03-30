/-
  Export: save current view to file (csv/parquet/json/ndjson).
  Uses DuckDB COPY for efficient streaming export.
-/
import Tc.Data.ADBC.Ops
import Tc.Fzf
import Tc.View

namespace Tc.ExportFmt
-- | (extension, COPY option, ExportFmt) — single source of truth
private def all : Array (String × String × ExportFmt) := #[
  ("csv", "(FORMAT CSV, HEADER true)", .csv), ("parquet", "(FORMAT PARQUET)", .parquet),
  ("json", "(FORMAT JSON)", .json), ("ndjson", "(FORMAT JSON, ARRAY false)", .ndjson)]
def ext (f : ExportFmt) : String := all.findSome? (fun (e, _, v) => if v == f then some e else none) |>.getD "csv"
def copyOpt (f : ExportFmt) : String := all.findSome? (fun (_, o, v) => if v == f then some o else none) |>.getD ""
def ofString? (s : String) : Option ExportFmt := all.findSome? fun (e, _, v) => if e == s then some v else none
end Tc.ExportFmt

namespace Tc.Export

-- | Prompt user for export format via fzf
def pickFmt : IO (Option ExportFmt) := do
  match ← Fzf.fzf #["--prompt=export: "] "csv\nparquet\njson\nndjson" with
  | some raw => pure (ExportFmt.ofString? raw.trimAscii.toString)
  | none => pure none

-- | Export current view to file via DuckDB COPY
def run (s : ViewStack AdbcTable) (fmt : ExportFmt) : IO (ViewStack AdbcTable) := do
  let some sql ← Prql.compile s.tbl.query.render | throw (.userError "PRQL compile failed")
  let name := s.cur.tabName.replace "/" "_" |>.replace " " "_"
  let stem := (name.splitOn ".").head?.filter (!·.isEmpty) |>.getD name
  let path := s!"{← Log.dir}/tv_export_{stem}.{fmt.ext}"
  let copySql := s!"COPY ({stripSemi sql}) TO '{escSql path}' {fmt.copyOpt}"
  Log.write "export" copySql
  let _ ← Adbc.query copySql
  statusMsg s!"exported {path}"; pure s

-- | Export by format string directly (no fzf)
def runWith (s : ViewStack AdbcTable) (fmtStr : String) : IO (ViewStack AdbcTable) := do
  let some fmt := ExportFmt.ofString? fmtStr | return s
  run s fmt

end Tc.Export
