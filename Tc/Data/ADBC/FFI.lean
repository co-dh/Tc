/-
  ADBC FFI - DuckDB via Arrow Database Connectivity
-/

namespace Adbc

-- | Opaque query result (Arrow batches)
opaque QueryResult : Type

-- | Init ADBC (in-memory DuckDB), returns true on success
@[extern "lean_adbc_init"]
opaque init : IO Bool

-- | Shutdown ADBC
@[extern "lean_adbc_shutdown"]
opaque shutdown : IO Unit

-- | Execute SQL query, returns QueryResult
@[extern "lean_adbc_query"]
opaque query : @& String → IO QueryResult

-- | Get column count
@[extern "lean_qr_ncols"]
opaque ncols : @& QueryResult → IO UInt64

-- | Get row count
@[extern "lean_qr_nrows"]
opaque nrows : @& QueryResult → IO UInt64

-- | Get column name
@[extern "lean_qr_col_name"]
opaque colName : @& QueryResult → UInt64 → IO String

-- | Get column format (Arrow type string: l=int64, u=utf8, g=float64, etc.)
@[extern "lean_qr_col_fmt"]
opaque colFmt : @& QueryResult → UInt64 → IO String

-- | Get cell as string
@[extern "lean_qr_cell_str"]
opaque cellStr : @& QueryResult → UInt64 → UInt64 → IO String

-- | Get cell as Int
@[extern "lean_qr_cell_int"]
opaque cellInt : @& QueryResult → UInt64 → UInt64 → IO Int

-- | Get cell as Float
@[extern "lean_qr_cell_float"]
opaque cellFloat : @& QueryResult → UInt64 → UInt64 → IO Float

-- | Check if cell is null
@[extern "lean_qr_cell_is_null"]
opaque cellIsNull : @& QueryResult → UInt64 → UInt64 → IO Bool

-- | Get column widths (max of header and data)
@[extern "lean_qr_col_widths"]
opaque colWidths : @& QueryResult → IO (Array Nat)

-- | Render table directly to terminal (batch-by-batch)
-- colIdxs: column indices in display order (key cols first)
-- nKeyCols: number of key columns
-- r0, r1: row range
-- curRow, curCol: cursor position
-- selColIdxs: selected column indices
-- selRows: selected row indices
-- styles: 14 UInt32 values (7 states × (fg|attrs, bg))
-- maxWStr, maxWOther: max column widths
-- decimals: decimal places for floats
-- returns: Array (colIdx, x, width) of visible columns
@[extern "lean_render_table"]
opaque renderTable : @& QueryResult → @& Array Nat → UInt64 → UInt64 →
                     UInt64 → UInt64 → UInt64 → UInt64 →
                     @& Array Nat → @& Array Nat →
                     @& Array UInt32 → UInt8 → UInt8 → UInt8 → IO (Array (Nat × Nat × Nat))

end Adbc
