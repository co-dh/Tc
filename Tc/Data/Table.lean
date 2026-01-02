/-
  Table typeclasses for data backends.
  ReadTable: read-only access (nRows, colNames, cell, render)
  ModifyTable: column deletion (row deletion via SQL filter)
-/

namespace Tc

-- Read-only table access
class ReadTable (α : Type) where
  nRows    : α → Nat                          -- total row count
  colNames : α → Array String                 -- column names (size = nCols)
  colWidths: α → Array Nat                    -- column widths for rendering
  cell     : α → Nat → Nat → String           -- cell at (row, col)

-- Derived: column count from colNames.size
def ReadTable.nCols [ReadTable α] (a : α) : Nat := (ReadTable.colNames a).size

-- Mutable table operations (column-only; row deletion via SQL filter)
class ModifyTable (α : Type) where
  delCols : Array String → α → α              -- delete columns by names

end Tc
