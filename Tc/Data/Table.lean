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
class ModifyTable (α : Type) extends ReadTable α where
  delCols : Array Nat → α → α              -- delete columns by indices

-- Delete columns at cursor + selections, return new table and filtered group
def ModifyTable.del [ModifyTable α] (tbl : α) (cursor : Nat) (sels : Array Nat) (grp : Array String)
    : α × Array String :=
  let idxs := if sels.contains cursor then sels else sels.push cursor
  let names := ReadTable.colNames tbl
  let delNames := idxs.map (names.getD · "")
  (delCols idxs tbl, grp.filter (!delNames.contains ·))

end Tc
