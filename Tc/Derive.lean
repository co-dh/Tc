/-
  Derive: add computed column via PRQL derive.
  Press '=' → fzf prompt with column names as hints → requery with derive → push view.
-/
import Tc.Data.ADBC.Ops
import Tc.Fzf
import Tc.View

namespace Tc.Derive

-- Auto-incrementing counter for derived column names (_1, _2, ...)
initialize deriveCount : IO.Ref Nat ← IO.mkRef 0

-- Sample PRQL expressions by column type
private def samples (col : String) : String → String
  | "int" | "float" | "decimal" =>
    s!"{col} * 2 | {col} + other | {col} != null | math.round 2 {col}"
  | "str" => -- f-string uses literal braces (PRQL syntax)
    s!"{col} != null | f\"\{col}-\{other}\" | {col} | text.upper | {col} | text.length"
  | "date" =>
    s!"{col} != null | {col} | date.year | {col} - @2024-01-01"
  | "time" | "timestamp" =>
    s!"{col} != null | {col} | date.hour | {col} | date.minute"
  | "bool" =>
    s!"{col} != null | {col} == false | !{col}"
  | _ =>
    s!"{col} != null | {col} > 0 | f\"\{col}\""

-- | Prompt for expression, requery with derive, push new view.
-- Returns unchanged stack on cancel (like Export.run).
def run (s : ViewStack AdbcTable) : IO (ViewStack AdbcTable) := do
  let names := TblOps.colNames s.tbl
  let curCol := s.cur.nav.curColIdx
  let curName := names.getD curCol ""
  let typ := TblOps.colType s.tbl curCol
  let cols := names.mapIdx (fun i n => s!"{n}:{s.tbl.colTypes.getD i "?"}") |>.toList |> " ".intercalate
  let header := s!"{cols}\n{samples curName typ}"
  let hint := "\n".intercalate names.toList
  let some expr ← Fzf.fzf #["--print-query", "--prompt=derive: ", s!"--header={header}"] hint | return s
  let expr := expr.trimAscii.toString
  if expr.isEmpty then return s
  let n ← deriveCount.modifyGet fun n => (n + 1, n + 1)
  let name := s!"_{n}"
  let q := s.tbl.query.pipe (.derive #[(name, expr)])
  Log.write "derive" q.render
  -- derive preserves row count, no need for queryCount
  let some tbl' ← AdbcTable.requery q s.tbl.totalRows | return s
  let some v := s.cur.rebuild tbl' | return s
  return s.push { v with disp := s!"={name}" }

end Tc.Derive
