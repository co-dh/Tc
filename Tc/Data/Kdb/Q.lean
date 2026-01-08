/-
  Q: Render Op to q expressions
  Uses common Op types from Tc/Op.lean
-/
import Tc.Op
import Tc.Types
import Tc.Error

namespace Q

open Tc

-- | q query: table name + operations
structure Query where
  tbl  : String       -- table name or select expr
  ops  : Array Op := #[]
  deriving Inhabited

-- | Reserved q words needing quoting
def reserved : Array String :=
  #["select", "from", "where", "by", "update", "delete", "exec",
    "insert", "upsert", "if", "do", "while", "each", "over", "scan"]

-- | Quote column name for q (backtick symbol)
def quote (s : String) : String :=
  let needsQuote := s.any fun c => !c.isAlphanum && c != '_'
  if needsQuote then s!"`$\"{s}\"" else s!"`{s}"

-- | Quote without backtick (for expressions)
def quoteExpr (s : String) : String :=
  if s.any (fun c => !c.isAlphanum && c != '_') then s!"$\"{s}\"" else s

-- | Render aggregate function name
def aggName : Agg → String
  | .count => "count" | .sum => "sum" | .avg => "avg"
  | .min => "min" | .max => "max" | .stddev => "dev" | .dist => "count distinct"

-- | Render single Op to q expression (applied to input t)
-- Returns expression that transforms t
def Op.render : Op → String
  | .filter e => s!"select from t where {e}"
  | .sort cols =>
    -- chain sorts: last col is primary, so foldr gives correct order
    cols.foldr (init := "t") fun (c, asc) acc =>
      let dir := if asc then "xasc" else "xdesc"
      s!"`{c} {dir} {acc}"
  | .sel cols =>
    let c := cols.map ("`" ++ ·) |>.toList |> String.intercalate ""
    s!"({c})#t"
  | .derive bs =>
    let d := bs.map fun (n, e) => s!"{n}:{e}"
    s!"update {d.toList |> String.intercalate ", "} from t"
  | .group keys aggs =>
    let ks := keys.map ("`" ++ ·) |>.toList |> String.intercalate ""
    let as := aggs.map fun (fn, name, col) => s!"{name}:{aggName fn} {col}"
    s!"select {as.toList |> String.intercalate ", "} by {ks} from t"
  | .take n => s!"{n}#t"

-- | Render full query to q string
-- Assigns table to t, then chains ops
def Query.render (q : Query) (_ : Nat := 0) : String :=
  let base := s!"t:{q.tbl}"
  if q.ops.isEmpty then q.tbl
  else
    let opsStr := q.ops.foldl (init := "") fun acc op =>
      s!"{acc}; t:{Op.render op}"
    s!"{base}{opsStr}; t"

-- | Pipe: append operation to query
def Query.pipe (q : Query) (op : Op) : Query := { q with ops := q.ops.push op }

infixl:65 " |> " => Query.pipe

-- | Filter helper
def Query.filter (q : Query) (expr : String) : Query := q.pipe (.filter expr)

-- | Sort helper
def Query.sort (q : Query) (col : String) (asc : Bool) : Query :=
  q.pipe (.sort #[(col, asc)])

-- | Select columns helper
def Query.sel (q : Query) (cols : Array String) : Query := q.pipe (.sel cols)

-- | Take helper
def Query.take (q : Query) (n : Nat) : Query := q.pipe (.take n)

-- | Simple take query (no ops)
def take (tbl : String) (n : Nat) : String := s!"{n}#{tbl}"

-- | Count query
def count (tbl : String) : String := s!"count {tbl}"

-- | Meta query: column names, types, nulls, etc.
def metaQ (tbl : String) : String := s!"meta {tbl}"

-- | Distinct values for column
def distinct (tbl : String) (col : String) : String :=
  s!"distinct {tbl}[{quote col}]"

-- | Frequency count (group by + count)
def freq (tbl : String) (col : String) : String :=
  s!"select cnt:count i by {col} from {tbl}"

end Q
