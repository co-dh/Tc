/-
  Common table operations (backend-agnostic)
  Used by ADBC (via PRQL), MemTable (native), future kdb/q
-/
namespace Tc

-- | Aggregate function
inductive Agg where
  | count | sum | avg | min | max | stddev | dist
  deriving Repr, Inhabited, BEq

-- | Table operation (single pipeline stage)
inductive Op where
  | filter (expr : String)                          -- filter rows
  | sort (cols : Array (String × Bool))             -- sort by cols; Bool = asc
  | sel (cols : Array String)                       -- select columns
  | derive (bindings : Array (String × String))     -- computed columns
  | group (keys : Array String) (aggs : Array (Agg × String × String))  -- group + agg
  | take (n : Nat)                                  -- limit rows
  deriving Inhabited

namespace Agg

-- | Short name for result column
def short : Agg → String
  | .count => "count" | .sum => "sum" | .avg => "avg"
  | .min => "min" | .max => "max" | .stddev => "stddev" | .dist => "dist"

end Agg

-- | Query: base table + operations
structure Query where
  base : String := "tbl"
  ops  : Array Op := #[]
  deriving Inhabited

namespace Query

-- | Append operation to query
def pipe (q : Query) (op : Op) : Query := { q with ops := q.ops.push op }

infixl:65 " |> " => pipe

-- | Builder helpers
def filter (q : Query) (expr : String) : Query := q.pipe (.filter expr)
def select (q : Query) (cols : Array String) : Query := q.pipe (.sel cols)
def sortBy (q : Query) (cols : Array (String × Bool)) : Query := q.pipe (.sort cols)
def take (q : Query) (n : Nat) : Query := q.pipe (.take n)
def derive1 (q : Query) (name expr : String) : Query := q.pipe (.derive #[(name, expr)])

-- | Aggregate query (group by keys, apply funcs to cols)
def agg (q : Query) (keys : Array String) (funcs : Array Agg) (cols : Array String) : Query :=
  let aggs := funcs.flatMap fun f => cols.map fun c => (f, s!"{f.short}_{c}", c)
  q.pipe (.group keys aggs)

end Query

-- | Execute operation on table (backend-specific)
class ExecOp (α : Type) where
  exec : α → Op → IO (Option α)

end Tc
