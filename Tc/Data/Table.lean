/-
  Table typeclasses for data backends.
  ReadTable: read-only access (nRows, colNames, cell, render)
  ModifyTable: column deletion (row deletion via SQL filter)
-/

namespace Tc

-- Read-only table access
class ReadTable (α : Type) where
  nRows     : α → Nat                         -- row count in current view
  colNames  : α → Array String                -- column names (size = nCols)
  totalRows : α → Nat := nRows                -- total rows (for ADBC: actual count)

-- Derived: column count from colNames.size
def ReadTable.nCols [ReadTable α] (a : α) : Nat := (ReadTable.colNames a).size

-- Mutable table operations (column-only; row deletion via SQL filter)
class ModifyTable (α : Type) extends ReadTable α where
  delCols : Array Nat → α → α              -- delete columns by indices
  sortBy  : Array Nat → Bool → α → α       -- sort by column indices, asc/desc

-- Delete columns at cursor + selections, return new table and filtered group
def ModifyTable.del [ModifyTable α] (tbl : α) (cursor : Nat) (sels : Array Nat) (grp : Array String)
    : α × Array String :=
  let idxs := if sels.contains cursor then sels else sels.push cursor
  let names := ReadTable.colNames tbl
  let delNames := idxs.map (names.getD · "")
  (delCols idxs tbl, grp.filter (!delNames.contains ·))

-- Sort table by group indices (asc) then cursor column
def ModifyTable.sort [ModifyTable α] (tbl : α) (cursor : Nat) (grpIdxs : Array Nat) (asc : Bool) : α :=
  sortBy (grpIdxs.push cursor) asc tbl

end Tc
