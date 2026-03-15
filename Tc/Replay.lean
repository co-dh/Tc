/-
  Replay: generate compact PRQL ops string from current view state.
  Displayed on tab line; can be replayed with `tc file -p "ops"`.
-/
import Tc.Data.ADBC.Ops
import Tc.View

namespace Tc.Replay

-- | Extract PRQL pipeline ops from view's query, omitting the `from` clause.
-- Returns empty string if no ops applied.
def opsStr (v : View AdbcTable) : String := v.nav.tbl.query.renderOps

end Tc.Replay
