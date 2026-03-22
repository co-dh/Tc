/-
  Derive: add computed column via PRQL derive.
  Press '=' → fzf prompt with column names as hints → requery with derive → push view.
-/
import Tc.Data.ADBC.Ops
import Tc.Fzf
import Tc.View

namespace Tc.Derive

-- Sample PRQL expressions by column type, showing name = expr format
private def samples (col : String) : String → String
  | "int" | "float" | "decimal" =>
    s!"d = {col} * 2 | d = {col} != null | d = math.round 2 {col}"
  | "str" => -- f-string uses literal braces (PRQL syntax)
    s!"d = {col} != null | d = f\"\{col}-\{other}\" | d = {col} | text.upper"
  | "date" =>
    s!"d = {col} != null | d = {col} | date.year | d = {col} - @2024-01-01"
  | "time" | "timestamp" =>
    s!"d = {col} != null | d = {col} | date.hour | d = {col} | date.minute"
  | "bool" =>
    s!"d = {col} != null | d = {col} == false | d = !{col}"
  | _ =>
    s!"d = {col} != null | d = {col} > 0 | d = f\"\{col}\""

-- | Build "col : type" lines with aligned ":"
private def colHints (names : Array String) (types : Array String) : String :=
  let maxLen := names.foldl (fun mx n => max mx n.length) 0
  let lines := names.mapIdx fun i n =>
    let pad := String.ofList (List.replicate (maxLen - n.length) ' ')
    s!"{n}{pad} : {types.getD i "?"}"
  "\n".intercalate lines.toList

-- | Parse "name = expr" format. Returns none if no "=" found.
private def parseDerive (input : String) : Option (String × String) :=
  match input.splitOn " = " with
  | name :: rest =>
    let n := name.trimAscii.toString
    let e := (" = ".intercalate rest).trimAscii.toString  -- rejoin in case expr contains " = "
    if n.isEmpty || e.isEmpty then none else some (n, e)
  | _ => none

-- | Derive column from expression (no fzf). Called by socket/dispatch and by `run` after fzf.
def runWith (s : ViewStack AdbcTable) (input : String) : IO (ViewStack AdbcTable) := do
  let some (name, expr) := parseDerive input | return s
  let q := s.tbl.query.pipe (.derive #[(name, expr)])
  Log.write "derive" q.render
  let some tbl' ← AdbcTable.requery q s.tbl.totalRows | return s
  let nCols := (TblOps.colNames tbl').size
  let some v := s.cur.rebuild tbl' (col := nCols - 1) | return s
  return s.push { v with disp := s!"={name}" }

-- | Prompt for name = expr via fzf, then derive.
def run (s : ViewStack AdbcTable) : IO (ViewStack AdbcTable) := do
  let names := TblOps.colNames s.tbl
  let curCol := s.cur.nav.curColIdx
  let curName := names.getD curCol ""
  let typ := TblOps.colType s.tbl curCol
  let header := s!"name = expr\n{samples curName typ}"
  let hint := colHints names s.tbl.colTypes
  let some raw ← Fzf.fzf #["--print-query", "--prompt=derive: ", s!"--header={header}"] hint | return s
  runWith s raw.trimAscii.toString

end Tc.Derive
