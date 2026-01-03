/-
  Type-safe PRQL statement constructor
-/
import Tc.Types
import Tc.Error

namespace Prql

-- | Aggregate function (for PRQL group/agg)
inductive Agg where
  | count | sum | avg | min | max | stddev | dist
  deriving Repr, Inhabited

-- | PRQL operation (single pipe stage)
inductive Op where
  | filter (expr : String)                          -- filter <expr>
  | sort (cols : Array (String × Bool))             -- sort {col, -col2}; Bool = asc
  | sel (cols : Array String)                       -- select {a, b, c}
  | derive (bindings : Array (String × String))     -- derive {x = expr}
  | group (keys : Array String) (aggs : Array (Agg × String × String))  -- group {k} (agg {name = fn col})
  | take (n : Nat)                                  -- take n
  deriving Inhabited

-- | PRQL query: base table + operations
structure Query where
  base : String := "from df"  -- PRQL from clause (df = placeholder)
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

-- | Render aggregate function name (std. prefix to avoid column name conflicts)
def Agg.name : Agg → String
  | .count => "std.count" | .sum => "std.sum" | .avg => "std.average"
  | .min => "std.min" | .max => "std.max" | .stddev => "std.stddev"
  | .dist => "std.count_distinct"

-- | Short name for result column (no std. prefix)
def Agg.short : Agg → String
  | .count => "count" | .sum => "sum" | .avg => "average"
  | .min => "min" | .max => "max" | .stddev => "stddev" | .dist => "dist"

-- | Render single operation to PRQL string
def Op.render : Op → String
  | .filter e => s!"filter {e}"
  | .sort cols => s!"sort \{{(cols.map fun (c, asc) => renderSort c asc).join ", "}}"
  | .sel cols => s!"select \{{(cols.map quote).join ", "}}"
  | .derive bs => s!"derive \{{(bs.map fun (n, e) => s!"{quote n} = {e}").join ", "}}"
  | .group keys aggs =>
    let as := aggs.map fun (fn, name, col) => s!"{name} = {fn.name} {quote col}"
    s!"group \{{(keys.map quote).join ", "}} (aggregate \{{as.join ", "}})"
  | .take n => s!"take {n}"

-- | Render full query to PRQL string
def Query.render (q : Query) : String :=
  if q.ops.isEmpty then q.base
  else q.base ++ " | " ++ (q.ops.map Op.render).join " | "

-- | Pipe: append operation to query
def Query.pipe (q : Query) (op : Op) : Query := { q with ops := q.ops.push op }

infixl:65 " |> " => Query.pipe  -- q |> .filter "x > 5"

-- | Builder helpers
def Query.filter (q : Query) (expr : String) : Query := q.pipe (.filter expr)
def Query.select (q : Query) (cols : Array String) : Query := q.pipe (.sel cols)
def Query.derive1 (q : Query) (name expr : String) : Query := q.pipe (.derive #[(name, expr)])

-- | Aggregate query (group by keys, apply funcs to cols)
def Query.agg (q : Query) (keys : Array String) (funcs : Array Agg) (cols : Array String) : Query :=
  let aggs := funcs.flatMap fun f => cols.map fun c => (f, s!"{f.short}_{c}", c)
  q.pipe (.group keys aggs)

-- | Build PRQL filter from column names and cell values
-- Example: cols=#["a","b"], vals=#[.int 1, .str "x"] → "a == 1 && b == 'x'"
def buildFilter (cols : Array String) (vals : Array Cell) : String :=
  cols.mapIdx (fun i cn => s!"{quote cn} == {(vals.getD i .null).toPrql}")
    |>.toList |> String.intercalate " && "

-- | Parse agg function name to Agg
def Agg.parse : String → Option Agg
  | "count" => some .count | "sum" => some .sum | "average" => some .avg
  | "min" => some .min | "max" => some .max | "stddev" => some .stddev
  | "dist" => some .dist | _ => none

-- | PRQL function definitions (prepended to all queries)
-- Matches rust tv's cfg/funcs.prql (use std.count to avoid ambiguity with column named 'count')
def funcs : String := "
let freq  = func c tbl <relation> -> (from tbl | group {c} (aggregate {Cnt = std.count this}) | derive {Pct = Cnt * 100 / std.sum Cnt, Bar = s\"repeat('#', CAST({Pct} / 5 AS INTEGER))\"} | sort {-Cnt})
let cnt   = func tbl   <relation> -> (from tbl | aggregate {n = std.count this})
let uniq  = func c tbl <relation> -> (from tbl | group {c} (take 1) | select {c})
let stats = func c tbl <relation> -> (from tbl | aggregate {n = std.count this, min = std.min c, max = std.max c, avg = std.average c, std = std.stddev c})
let meta  = func c tbl <relation> -> (from tbl | aggregate {cnt = s\"COUNT({c})\", dist = std.count_distinct c, total = std.count this, min = std.min c, max = std.max c})
"

-- | Theorems: freq PRQL includes required columns
theorem funcs_has_pct : (funcs.splitOn "Pct").length > 1 := by native_decide
theorem funcs_has_bar : (funcs.splitOn "Bar").length > 1 := by native_decide

-- | Compile PRQL to SQL using prqlc CLI (stdin → stdout)
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
