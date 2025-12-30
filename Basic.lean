/-
  Typeclass-based navigation state for tabular viewer.

  Key abstractions:
  - Table: dimensions from data source (nRows, colNames)
  - OrdSet: ordered set with invert flag for selections
  - RowNav/ColNav: cursor + offset + selection for row/col
  - Cursor: get/set/move/search typeclass
  - NavState: composes RowNav + ColNav
-/

/-! ## Classes -/

-- OrdSetOps: typeclass for ordered set operations
-- α = element type, β = set implementation type
class OrdSetOps (α : Type) (β : outParam Type) where
  add    : Array α → β → β      -- add elements (dedup)
  remove : Array α → β → β      -- remove elements
  toggle : α → β → β            -- add if absent, remove if present
  clear  : β → β                -- remove all
  invert : β → β                -- flip inv flag (select all except)
  mem    : α → β → Bool         -- membership test (respects inv)

-- Cursor: typeclass for cursor operations
-- α = nav type, bound = max position, elem = element type for search
class Cursor (α : Type) (bound : Nat) (elem : Type) where
  get    : α → Nat                    -- current position
  set    : Nat → α → α                -- set position (clamped)
  move   : Int → α → α                -- relative move (clamped)
  search : (elem → Bool) → α → α      -- move to next match

/-! ## Private helpers -/

-- Clamp value to [0, bound)
private def clamp (n : Int) (bound : Nat) : Nat :=
  if bound = 0 then 0
  else if n < 0 then 0
  else if n.toNat >= bound then bound - 1
  else n.toNat

-- Remove duplicates keeping first occurrence
private def dedup [BEq α] (a : Array α) : Array α :=
  a.foldl (fun acc x => if acc.contains x then acc else acc.push x) #[]

/-! ## Structures -/

-- Table: query result dimensions from data source
structure Table where
  nRows : Nat              -- total row count
  colNames : Array String  -- column names in data order

def Table.nCols (t : Table) : Nat := t.colNames.size

-- OrdSet: concrete ordered set with invert flag
-- inv=true means "all except arr" (avoids materializing large inversions)
structure OrdSet (α : Type) [BEq α] where
  arr : Array α := #[]   -- selected elements
  inv : Bool := false    -- true = inverted (all except arr)

-- RowNav: row cursor, offset, and selection
structure RowNav where
  cur  : Nat := 0           -- cursor position
  off  : Nat := 0           -- first visible row (scroll offset)
  sels : OrdSet Nat := {}   -- selected row indices

-- DispIdx: index into display order (group first, then rest)
-- Newtype for type safety - can't mix with raw Nat
structure DispIdx where
  val : Nat := 0
  deriving BEq, Repr

-- Clamp DispIdx to [0, bound)
private def clampDisp (n : Int) (bound : Nat) : DispIdx :=
  ⟨clamp n bound⟩

-- ColNav: column cursor, offset, selections, and group columns
-- Display order = group.arr ++ (colNames - group)
structure ColNav where
  cur   : DispIdx := {}         -- cursor position in display order
  off   : DispIdx := {}         -- first visible col (scroll offset)
  sels  : OrdSet String := {}   -- selected column names
  group : OrdSet String := {}   -- group columns (displayed first)

-- Compute display order: group first, then rest
def ColNav.dispOrder (c : ColNav) (colNames : Array String) : Array String :=
  c.group.arr ++ colNames.filter (!c.group.arr.contains ·)

-- Get column name at display index
def ColNav.colAt (c : ColNav) (colNames : Array String) (i : DispIdx) : Option String :=
  (c.dispOrder colNames)[i.val]?

-- NavState: composes row and column navigation
structure NavState (t : Table) where
  row : RowNav := {}
  col : ColNav := {}

-- Row: map from column name to cell value
abbrev Row := String → String

/-! ## Instances -/

-- OrdSet implements OrdSetOps
instance [BEq α] : OrdSetOps α (OrdSet α) where
  add    := fun xs s => { s with arr := dedup (s.arr ++ xs) }
  remove := fun xs s => { s with arr := s.arr.filter (!xs.contains ·) }
  toggle := fun x s => if s.arr.contains x
                       then { s with arr := s.arr.erase x }
                       else { s with arr := s.arr.push x }
  clear  := fun _ => ⟨#[], false⟩
  invert := fun s => { s with inv := !s.inv }
  mem    := fun x s => s.arr.contains x != s.inv  -- XOR with inv

-- RowNav Cursor: operates on row.cur, searches over Row data
instance : Cursor RowNav bound Row where
  get    := fun r => r.cur
  set    := fun n r => { r with cur := clamp n bound }
  move   := fun d r => { r with cur := clamp (r.cur + d) bound }
  search := fun _ r => r  -- TODO

-- ColNav Cursor: operates on col.cur, searches over column names
instance : Cursor ColNav bound String where
  get    := fun c => c.cur.val
  set    := fun n c => { c with cur := clampDisp n bound }
  move   := fun d c => { c with cur := clampDisp (c.cur.val + d) bound }
  search := fun _ c => c  -- TODO

/-! ## Theorems -/

-- clamp always returns value < bound (when bound > 0)
theorem clamp_lt_bound (n : Int) (bound : Nat) (h : bound > 0) : clamp n bound < bound := by
  unfold clamp
  split
  · omega
  · split
    · omega
    · split
      · omega
      · omega

-- invert twice returns to original
theorem OrdSet.invert_invert [BEq α] (s : OrdSet α) :
    (OrdSetOps.invert (α := α) (OrdSetOps.invert (α := α) s)) = s := by
  simp only [OrdSetOps.invert]
  cases s with
  | mk arr inv => simp [Bool.not_not]

-- clear produces empty set
theorem OrdSet.clear_empty [BEq α] (s : OrdSet α) :
    (OrdSetOps.clear (α := α) s) = ({} : OrdSet α) := by
  rfl

-- group columns are at front of display order
theorem ColNav.group_at_front (c : ColNav) (colNames : Array String) (i : Nat)
    (h : i < c.group.arr.size) :
    (c.dispOrder colNames)[i]? = c.group.arr[i]? := by
  simp only [dispOrder]
  rw [Array.getElem?_append_left h]
