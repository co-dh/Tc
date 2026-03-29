/-
  PRQL rendering and compilation
  Uses common Op types from Tc/Op.lean
-/
import Tc.Types
import Tc.Util

namespace Prql

open Tc

-- | PRQL query: base table + operations (PRQL-specific base format)
structure Query where
  base : String := "from df"  -- PRQL from clause
  ops  : Array Op := #[]
  deriving Inhabited

-- | Quote column name — always backtick to avoid PRQL keyword collisions
def quote (s : String) : String := s!"`{s}`"

-- | Render sort column (asc = col, desc = -col)
def renderSort (col : String) (asc : Bool) : String :=
  let qc := quote col
  if asc then qc else s!"-{qc}"

-- | Render aggregate function name (std. prefix for PRQL)
def aggName : Agg → String
  | .count => "std.count" | .sum => "std.sum" | .avg => "std.average"
  | .min => "std.min" | .max => "std.max" | .stddev => "std.stddev"
  | .dist => "std.count_distinct"

-- | DuckDB-quote column name for use inside PRQL s-string (\" escapes)
def dqQuote (s : String) : String := "\\\"" ++ s ++ "\\\""

-- | Render single operation to PRQL string
def Op.render : Op → String
  | .filter e => s!"filter {e}"
  | .sort cols => s!"sort \{{", ".intercalate (cols.map fun (c, asc) => renderSort c asc).toList}}"
  | .sel cols => s!"select \{{", ".intercalate (cols.map quote).toList}}"
  | .exclude cols => "select s\"* EXCLUDE (" ++ ", ".intercalate (cols.map dqQuote).toList ++ ")\""
  | .derive bs => s!"derive \{{", ".intercalate (bs.map fun (n, e) => s!"{quote n} = {e}").toList}}"
  | .group keys aggs =>
    let as := aggs.map fun (fn, name, col) => s!"{name} = {aggName fn} {quote col}"
    s!"group \{{", ".intercalate (keys.map quote).toList}} (aggregate \{{", ".intercalate as.toList}})"
  | .take n => s!"take {n}"

-- | Render just the ops portion (no base/from clause)
def Query.renderOps (q : Query) : String :=
  if q.ops.isEmpty then ""
  else " | ".intercalate (q.ops.map Op.render).toList

-- | Render full query to PRQL string
def Query.render (q : Query) : String :=
  let ops := q.renderOps
  if ops.isEmpty then q.base else q.base ++ " | " ++ ops

-- | Pipe: append operation to query
def Query.pipe (q : Query) (op : Op) : Query := { q with ops := q.ops.push op }

infixl:65 " |> " => Query.pipe

-- | Filter helper
def Query.filter (q : Query) (expr : String) : Query := q.pipe (.filter expr)

-- | Common PRQL query strings (avoid duplication across modules)
def ducktabs := "from dtabs | tbl_info"
def ducktabsF := "from dtabs | tbl_info_filtered"

-- | PRQL function definitions, compiled into the binary
def funcs : String := include_str "funcs.prql"

-- | Compile PRQL to SQL using prqlc CLI
def compile (prql : String) : IO (Option String) := do
  let full := funcs ++ "\n" ++ prql
  let child ← IO.Process.spawn {
    cmd := "prqlc"
    args := #["compile", "--hide-signature-comment", "-t", "sql.duckdb"]
    stdin := .piped
    stdout := .piped
    stderr := .piped
  }
  child.stdin.putStr full
  child.stdin.flush
  let (_, child') ← child.takeStdin
  let stdout ← child'.stdout.readToEnd
  let stderr ← child'.stderr.readToEnd
  let code ← child'.wait
  if code == 0 then return some stdout
  else Log.error s!"prqlc: {stderr}"; return none

end Prql
