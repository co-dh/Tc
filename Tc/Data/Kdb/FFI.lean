/-
  kdb FFI - Connect to kdb server via IPC
-/

namespace Kdb

-- | Opaque query result (K object)
opaque QueryResult : Type

-- | Connect to kdb server
@[extern "lean_kdb_connect"]
opaque connect : @& String → UInt16 → IO Bool

-- | Disconnect from server
@[extern "lean_kdb_disconnect"]
opaque disconnect : IO Unit

-- | Check if connected
@[extern "lean_kdb_connected"]
opaque connected : IO Bool

-- | Execute q expression, returns table
@[extern "lean_kdb_query"]
opaque query : @& String → IO QueryResult

-- | Get column count
@[extern "lean_kdb_ncols"]
opaque ncols : @& QueryResult → IO UInt64

-- | Get row count
@[extern "lean_kdb_nrows"]
opaque nrows : @& QueryResult → IO UInt64

-- | Get column name
@[extern "lean_kdb_col_name"]
opaque colName : @& QueryResult → UInt64 → IO String

-- | Get column type char (j=long, f=float, s=symbol, etc.)
@[extern "lean_kdb_col_type"]
opaque colType : @& QueryResult → UInt64 → IO Char

-- | Get cell as string
@[extern "lean_kdb_cell_str"]
opaque cellStr : @& QueryResult → UInt64 → UInt64 → IO String

end Kdb
