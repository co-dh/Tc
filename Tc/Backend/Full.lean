/-
  Backend lifecycle: Full build with ADBC + Kdb
-/
import Tc.Data.ADBC.Table

namespace Tc.Backend

-- | Init all backends (ADBC for parquet/folder support)
def init : IO Bool := AdbcTable.init

-- | Shutdown all backends
def shutdown : IO Unit := AdbcTable.shutdown

end Tc.Backend
