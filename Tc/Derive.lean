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

-- | Build "col : type" lines with aligned ":"
private def colHints (names : Array String) (types : Array String) : String :=
  let maxLen := names.foldl (fun mx n => max mx n.length) 0
  let lines := names.mapIdx fun i n =>
    let pad := String.ofList (List.replicate (maxLen - n.length) ' ')
    s!"{n}{pad} : {types.getD i "?"}"
  "\n".intercalate lines.toList

-- | Prompt for expression, requery with derive, push new view.
-- Returns unchanged stack on cancel (like Export.run).
-- If user selects a hint line ("col : type"), strips the type annotation.
def run (s : ViewStack AdbcTable) : IO (ViewStack AdbcTable) := do
  let names := TblOps.colNames s.tbl
  let curCol := s.cur.nav.curColIdx
  let curName := names.getD curCol ""
  let typ := TblOps.colType s.tbl curCol
  let header := samples curName typ
  let hint := colHints names s.tbl.colTypes
  let some raw ← Fzf.fzf #["--print-query", "--prompt=derive: ", s!"--header={header}"] hint | return s
  -- strip " : type" suffix if user selected a hint line verbatim
  let expr := match (raw.trimAscii.toString).splitOn " : " with
    | [name, _] => if names.contains name then name else raw.trimAscii.toString
    | _ => raw.trimAscii.toString
  if expr.isEmpty then return s
  let n ← deriveCount.modifyGet fun n => (n + 1, n + 1)
  let name := s!"_{n}"
  let q := s.tbl.query.pipe (.derive #[(name, expr)])
  Log.write "derive" q.render
  -- derive preserves row count, no need for queryCount
  let some tbl' ← AdbcTable.requery q s.tbl.totalRows | return s
  let nCols := (TblOps.colNames tbl').size
  let some v := s.cur.rebuild tbl' (col := nCols - 1) | return s
  return s.push { v with disp := s!"={name}" }

end Tc.Derive
