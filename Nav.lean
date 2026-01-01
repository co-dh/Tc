/-
  Typeclass-based navigation state for tabular viewer.

  Key abstractions:
  - NavState: dims + row/col navigation + group
  - NavAxis: cursor (Fin) + selections (Array)
  - CurOps: typeclass for cursor operations
-/
import Tc.Offset

-- Toggle element in array: remove if present, append if not
namespace Array
def toggle [BEq α] (x : α) (a : Array α) : Array α :=
  if a.contains x then a.erase x else a.push x
end Array

-- Clamp Fin by delta, staying in [0, n)
namespace Fin
def clamp (f : Fin n) (d : Int) : Fin n :=
  let v := ((f.val : Int) + d).toNat
  let v' := min v (n - 1)
  ⟨v', Nat.lt_of_le_of_lt (Nat.min_le_right _ _) (Nat.sub_lt f.pos Nat.one_pos)⟩
end Fin

namespace Tc

/-! ## Classes -/

-- CurOps: cursor movement
-- α = state type, bound = max position, elem = element type (for find)
class CurOps (α : Type) (bound : Nat) (elem : Type) where
  pos    : α → Fin bound                           -- get cursor
  setPos : Fin bound → α → α                       -- set cursor
  move   : Int → α → α := fun d a => setPos ((pos a).clamp d) a  -- default
  find   : (elem → Bool) → α → α := fun _ a => a   -- default no-op

/-! ## Structures -/

-- NavAxis: cursor + selection for one axis (row or col)
structure NavAxis (n : Nat) (elem : Type) [BEq elem] where
  cur  : Fin n              -- cursor position
  sels : Array elem := #[]  -- selected elements

-- Default NavAxis for n > 0
def NavAxis.default [BEq elem] (h : n > 0) : NavAxis n elem := ⟨⟨0, h⟩, {}⟩

-- Type aliases
abbrev RowNav (m : Nat) := NavAxis m Nat
abbrev ColNav (n : Nat) := NavAxis n String

-- Compute display order: group first, then rest
def dispOrder (group : Array String) (colNames : Array String) : Array String :=
  group ++ colNames.filter (!group.contains ·)

-- dispOrder preserves size when group ⊆ colNames
theorem dispOrder_size (group colNames : Array String)
    (h : group.all (colNames.contains ·)) :
    (dispOrder group colNames).size = colNames.size := by
  simp only [dispOrder, Array.size_append]
  sorry -- filter removes exactly group.size elements

-- Get column name at display index (group ⊆ colNames required)
def colAt (group colNames : Array String) (i : Fin colNames.size)
    (h : group.all (colNames.contains ·)) : String :=
  (dispOrder group colNames)[i.val]'(by simp [dispOrder_size group colNames h]; exact i.isLt)

-- NavState: query dims + row/column navigation
structure NavState (n : Nat) where
  nRows    : Nat                      -- total row count
  colNames : Array String             -- column names in data order
  hNames   : colNames.size = n        -- proof names matches n
  row      : RowNav nRows
  col      : ColNav n
  group    : Array String := #[]      -- group columns (displayed first)
  hGroup   : group.all (colNames.contains ·) := by decide  -- group ⊆ colNames

namespace NavState

-- Number of grouped columns
def nKeys (nav : NavState n) : Nat := nav.group.size

-- Selected row indices
def selRows (nav : NavState n) : Array Nat := nav.row.sels

-- Selected column indices (in original order)
def selColIdxs (nav : NavState n) : Array Nat :=
  nav.col.sels.filterMap fun name => nav.colNames.findIdx? (· == name)

-- Current column name
def curColName (nav : NavState n) : String :=
  colAt nav.group nav.colNames (nav.hNames ▸ nav.col.cur) nav.hGroup

-- Current column index (in original order)
def curColIdx (nav : NavState n) : Nat :=
  nav.colNames.findIdx? (· == nav.curColName) |>.getD 0

-- Column indices in display order
def dispColIdxs (nav : NavState n) : Array Nat :=
  (dispOrder nav.group nav.colNames).filterMap fun name => nav.colNames.findIdx? (· == name)

end NavState

/-! ## Instances -/

-- NavAxis CurOps (covers RowNav and ColNav)
instance [BEq elem] : CurOps (NavAxis n elem) n elem where
  pos    := (·.cur)
  setPos := fun f a => { a with cur := f }

/-! ## Dispatch -/

-- Apply verb to cursor (pg = page size)
def curVerb (α : Type) (bound : Nat) (elem : Type) [CurOps α bound elem]
    (pg : Nat) (v : Char) (a : α) : α :=
  let p := (@CurOps.pos α bound elem _ a).val
  match v with
  | '+' => @CurOps.move α bound elem _ (1 : Int) a
  | '-' => @CurOps.move α bound elem _ (-1 : Int) a
  | '<' => @CurOps.move α bound elem _ (-(pg : Int)) a
  | '>' => @CurOps.move α bound elem _ (pg : Int) a
  | '0' => @CurOps.move α bound elem _ (-(p : Int)) a
  | '$' => @CurOps.move α bound elem _ (bound - 1 - p : Int) a
  | _   => a

-- Dispatch 2-char command (object + verb) to NavState
-- rowPg/colPg = half screen for page up/down
def dispatch {n : Nat} (cmd : String) (nav : NavState n) (rowPg colPg : Nat) : NavState n :=
  let chars := cmd.toList
  if h : chars.length = 2 then
    let obj := chars[0]'(by omega)
    let v := chars[1]'(by omega)
    let curCol := colAt nav.group nav.colNames (nav.hNames ▸ nav.col.cur) nav.hGroup
    match obj with
    | 'r' => { nav with row := curVerb (RowNav nav.nRows) nav.nRows Nat rowPg v nav.row }
    | 'c' => { nav with col := curVerb (ColNav n) n String colPg v nav.col }
    | 'R' => { nav with row := { nav.row with sels := nav.row.sels.toggle nav.row.cur.val } }
    | 'C' => { nav with col := { nav.col with sels := nav.col.sels.toggle curCol } }
    | 'G' => { nav with group := nav.group.toggle curCol, hGroup := sorry }
    | _   => nav
  else nav

end Tc
