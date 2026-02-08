/-
  PRQL rendering and compilation
  Uses common Op types from Tc/Op.lean
-/
import Tc.Types
import Tc.Error

namespace Prql

open Tc

-- | PRQL query: base table + operations (PRQL-specific base format)
structure Query where
  base : String := "from df"  -- PRQL from clause
  ops  : Array Op := #[]
  deriving Inhabited

-- | Reserved PRQL names needing this. prefix
def reserved : Array String :=
  #["count", "sum", "avg", "min", "max", "average", "group", "sort",
    "filter", "select", "derive", "from", "take", "date", "time"]

-- | Quote column name (backticks for special chars, this. for reserved)
def quote (s : String) : String :=
  let needsBacktick := s.any fun c => !c.isAlphanum && c != '_'
  if needsBacktick then s!"`{s}`"
  else if reserved.contains s then s!"this.{s}"
  else s

-- | Render sort column (asc = col, desc = -col)
def renderSort (col : String) (asc : Bool) : String :=
  let qc := quote col
  if asc then qc else s!"-{qc}"

-- | Render aggregate function name (std. prefix for PRQL)
def aggName : Agg → String
  | .count => "std.count" | .sum => "std.sum" | .avg => "std.average"
  | .min => "std.min" | .max => "std.max" | .stddev => "std.stddev"
  | .dist => "std.count_distinct"

-- | Render single operation to PRQL string
def Op.render : Op → String
  | .filter e => s!"filter {e}"
  | .sort cols => s!"sort \{{", ".intercalate (cols.map fun (c, asc) => renderSort c asc).toList}}"
  | .sel cols => s!"select \{{", ".intercalate (cols.map quote).toList}}"
  | .derive bs => s!"derive \{{", ".intercalate (bs.map fun (n, e) => s!"{quote n} = {e}").toList}}"
  | .group keys aggs =>
    let as := aggs.map fun (fn, name, col) => s!"{name} = {aggName fn} {quote col}"
    s!"group \{{", ".intercalate (keys.map quote).toList}} (aggregate \{{", ".intercalate as.toList}})"
  | .take n => s!"take {n}"

-- | Render full query to PRQL string
def Query.render (q : Query) : String :=
  if q.ops.isEmpty then q.base
  else q.base ++ " | " ++ " | ".intercalate (q.ops.map Op.render).toList

-- | Pipe: append operation to query
def Query.pipe (q : Query) (op : Op) : Query := { q with ops := q.ops.push op }

infixl:65 " |> " => Query.pipe

-- | Filter helper
def Query.filter (q : Query) (expr : String) : Query := q.pipe (.filter expr)

-- | PRQL function definitions (prepended to all queries)
def funcs : String := include_str "funcs.prql"

-- | Compile PRQL to SQL using prqlc CLI
def compile (prql : String) : IO (Option String) := do
  let full := funcs ++ "\n" ++ prql
  let child ← IO.Process.spawn {
    cmd := "prqlc"
    args := #["compile", "--hide-signature-comment"]
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
  else Error.set s!"prqlc: {stderr}"; return none

end Prql
