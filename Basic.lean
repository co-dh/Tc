/-
  Typeclass-based navigation state for tabular viewer.

  Key abstractions:
  - Table: dimensions from data source (nRows, colNames)
  - OrdSet: ordered set with invert flag for selections
  - RowNav/ColNav: cursor + offset + selection for row/col
  - Cursor: get/set/move/search typeclass
  - NavState: composes RowNav + ColNav
-/

-- Table: query result dimensions from data source
structure Table where
  nRows : Nat              -- total row count
  colNames : Array String  -- column names in data order

def Table.nCols (t : Table) : Nat := t.colNames.size

-- OrdSetOps: typeclass for ordered set operations
-- α = element type, β = set implementation type
-- Caller gets element from cursor, passes to toggle/add/remove
class OrdSetOps (α : Type) (β : outParam Type) where
  empty  : β                    -- empty set
  add    : Array α → β → β      -- add elements (dedup)
  remove : Array α → β → β      -- remove elements
  toggle : α → β → β            -- add if absent, remove if present
  clear  : β → β                -- remove all
  invert : β → β                -- flip inv flag (select all except)
  mem    : α → β → Bool         -- membership test (respects inv)

-- OrdSet: concrete ordered set with invert flag
-- inv=true means "all except arr" (avoids materializing large inversions)
structure OrdSet (α : Type) [BEq α] where
  arr : Array α := #[]   -- selected elements
  inv : Bool := false    -- true = inverted (all except arr)

-- Remove duplicates keeping first occurrence
def Array.dedup [BEq α] (a : Array α) : Array α :=
  a.foldl (fun acc x => if acc.contains x then acc else acc.push x) #[]

-- OrdSet implements OrdSetOps
instance [BEq α] : OrdSetOps α (OrdSet α) where
  empty  := ⟨#[], false⟩
  add    := fun xs s => { s with arr := (s.arr ++ xs).dedup }
  remove := fun xs s => { s with arr := s.arr.filter (!xs.contains ·) }
  toggle := fun x s => if s.arr.contains x
                       then { s with arr := s.arr.erase x }
                       else { s with arr := s.arr.push x }
  clear  := fun _ => ⟨#[], false⟩
  invert := fun s => { s with inv := !s.inv }
  mem    := fun x s => s.arr.contains x != s.inv  -- XOR with inv

-- RowNav: row cursor, offset, and selection
structure RowNav where
  cur  : Nat := 0           -- cursor position
  off  : Nat := 0           -- first visible row (scroll offset)
  sels : OrdSet Nat := {}   -- selected row indices

-- DispIdx: index into display order (keys first, then rest)
-- Newtype for type safety - can't mix with raw Nat
structure DispIdx where
  val : Nat := 0
  deriving BEq, Repr

-- ColNav: column cursor, offset, selections, and key columns
-- Display order = keys.arr ++ (colNames - keys)
structure ColNav where
  cur  : DispIdx := {}         -- cursor position in display order
  off  : DispIdx := {}         -- first visible col (scroll offset)
  sels : OrdSet String := {}   -- selected column names
  keys : OrdSet String := {}   -- key columns (displayed first)

-- Compute display order: keys first, then rest
def ColNav.dispOrder (c : ColNav) (colNames : Array String) : Array String :=
  c.keys.arr ++ colNames.filter (!c.keys.arr.contains ·)

-- Get column name at display index
def ColNav.colAt (c : ColNav) (colNames : Array String) (i : DispIdx) : Option String :=
  (c.dispOrder colNames)[i.val]?

-- NavState: composes row and column navigation
structure NavState (t : Table) where
  row : RowNav := {}
  col : ColNav := {}

-- Row: map from column name to cell value
abbrev Row := String → String

-- Cursor: typeclass for cursor operations
-- α = nav type, bound = max position, elem = element type for search
class Cursor (α : Type) (bound : Nat) (elem : Type) where
  get    : α → Nat                    -- current position
  set    : Nat → α → α                -- set position (clamped)
  move   : Int → α → α                -- relative move (clamped)
  search : (elem → Bool) → α → α      -- move to next match

-- Clamp value to [0, bound)
def clamp (n : Int) (bound : Nat) : Nat :=
  if bound = 0 then 0
  else if n < 0 then 0
  else if n.toNat >= bound then bound - 1
  else n.toNat

-- RowNav Cursor: operates on row.cur, searches over Row data
instance : Cursor RowNav bound Row where
  get    := fun r => r.cur
  set    := fun n r => { r with cur := clamp n bound }
  move   := fun d r => { r with cur := clamp (r.cur + d) bound }
  search := fun _ r => r  -- TODO: find next row matching predicate

-- Clamp DispIdx to [0, bound)
def clampDisp (n : Int) (bound : Nat) : DispIdx :=
  ⟨clamp n bound⟩

-- ColNav Cursor: operates on col.cur, searches over column names
instance : Cursor ColNav bound String where
  get    := fun c => c.cur.val
  set    := fun n c => { c with cur := clampDisp n bound }
  move   := fun d c => { c with cur := clampDisp (c.cur.val + d) bound }
  search := fun _ c => c  -- TODO: find next col matching predicate
