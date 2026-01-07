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

-- | Execute operation on table (backend-specific)
class ExecOp (α : Type) where
  exec : α → Op → IO (Option α)

end Tc
