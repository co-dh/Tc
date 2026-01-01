/-
  Typeclass-based navigation state for tabular viewer.

  Key abstractions:
  - Nav: dimensions from data source (nRows, colNames)
  - OrdSet: ordered set with invert flag for selections
  - RowNav/ColNav: cursor + selection for row/col (no offset - that's view state)
  - Cursor: get/set/move/search typeclass
  - NavState: composes RowNav + ColNav
-/
import Tc.Offset

namespace Tc

/-! ## Classes -/

-- CurOps: cursor movement
-- α = state type, bound = max position, elem = element type (for find)
class CurOps (α : Type) (bound : Nat) (elem : Type) where
  move : Int → α → α                  -- move by delta, clamped to [0, bound)
  find : (elem → Bool) → α → α        -- / : search

-- SetOps: set operations
-- α = set type, elem = element type
class SetOps (α : Type) (elem : Type) where
  add    : elem → α → α               -- + : add elem
  remove : elem → α → α               -- - : remove elem
  clear  : α → α                      -- 0 : clear
  all    : α → α                      -- $ : select all
  toggle : elem → α → α               -- ^ : toggle elem
  invert : α → α                      -- ~ : invert

/-! ## Private helpers -/

-- Remove duplicates keeping first occurrence
private def dedup [BEq α] (a : Array α) : Array α :=
  a.foldl (fun acc x => if acc.contains x then acc else acc.push x) #[]

/-! ## Structures -/

-- Nav: query result dimensions from data source, parameterized by column count
structure Nav (nCols : Nat) where
  nRows : Nat                            -- total row count
  colNames : Array String                -- column names in data order
  hNames : colNames.size = nCols         -- proof names matches nCols

-- OrdSet: concrete ordered set with invert flag
-- inv=true means "all except arr" (avoids materializing large inversions)
structure OrdSet (α : Type) [BEq α] where
  arr : Array α := #[]   -- selected elements
  inv : Bool := false    -- true = inverted (all except arr)

-- Row navigation: cursor + selection (offset is view state)
structure RowNav where
  cur  : Nat := 0           -- cursor position
  sels : OrdSet Nat := {}   -- selected row indices

-- Column navigation: cursor as Fin n + selection (offset is view state)
structure ColNav (n : Nat) where
  cur  : Fin n              -- cursor position in display order
  sels : OrdSet String := {}  -- selected column names

-- Default ColNav for n > 0
def ColNav.default (h : n > 0) : ColNav n := ⟨⟨0, h⟩, {}⟩

-- Compute display order: group first, then rest
def dispOrder (group : OrdSet String) (colNames : Array String) : Array String :=
  group.arr ++ colNames.filter (!group.arr.contains ·)

-- Get column name at display index
def colAt (group : OrdSet String) (colNames : Array String) (i : Nat) : Option String :=
  (dispOrder group colNames)[i]?

-- NavState: composes row and column navigation
structure NavState (n : Nat) (t : Nav n) where
  row   : RowNav := {}
  col   : ColNav n
  group : OrdSet String := {}  -- group columns (displayed first)

-- Row: map from column name to cell value
abbrev Row := String → String

/-! ## Instances -/

-- RowNav CurOps: move by delta, clamped to [0, bound)
instance : CurOps RowNav bound Nat where
  move := fun d r =>
    let v := ((r.cur : Int) + d).toNat  -- neg -> 0
    { r with cur := clamp v 0 bound }
  find := fun _ r => r

-- ColNav CurOps: move by delta, clamped to [0, n)
instance : CurOps (ColNav n) n String where
  move := fun d c =>
    let v := ((c.cur.val : Int) + d).toNat
    let v' := min v (n - 1)
    { c with cur := ⟨v', Nat.lt_of_le_of_lt (Nat.min_le_right _ _) (Nat.sub_lt c.cur.pos Nat.one_pos)⟩ }
  find := fun _ c => c

-- OrdSet SetOps: set operations
instance [BEq α] : SetOps (OrdSet α) α where
  add    := fun x s => { s with arr := dedup (s.arr.push x) }
  remove := fun x s => { s with arr := s.arr.erase x }
  clear  := fun _ => ⟨#[], false⟩
  all    := fun _ => ⟨#[], true⟩   -- inverted empty = all
  toggle := fun x s => if s.arr.contains x
                       then { s with arr := s.arr.erase x }
                       else { s with arr := s.arr.push x }
  invert := fun s => { s with inv := !s.inv }

-- OrdSet membership (not in class, used internally)
def OrdSet.mem [BEq α] (x : α) (s : OrdSet α) : Bool := s.arr.contains x != s.inv

/-! ## Theorems -/

-- invert twice returns to original
theorem OrdSet.invert_invert [BEq α] (s : OrdSet α) :
    @SetOps.invert (OrdSet α) α _ (@SetOps.invert (OrdSet α) α _ s) = s := by
  simp only [SetOps.invert, Bool.not_not]

-- clear produces empty set
theorem OrdSet.clear_empty [BEq α] (s : OrdSet α) :
    @SetOps.clear (OrdSet α) α _ s = ({} : OrdSet α) := by
  rfl

-- group columns are at front of display order
theorem group_at_front (g : OrdSet String) (colNames : Array String) (i : Nat)
    (h : i < g.arr.size) :
    (dispOrder g colNames)[i]? = g.arr[i]? := by
  simp only [dispOrder]
  rw [Array.getElem?_append_left h]

/-! ## Dispatch -/

-- Apply verb to RowNav cursor (pg = half screen)
def rowVerb (bound : Nat) (pg : Nat) (v : Char) (r : RowNav) : RowNav :=
  match v with
  | '+' => @CurOps.move RowNav bound Nat _ 1 r
  | '-' => @CurOps.move RowNav bound Nat _ (-1) r
  | '<' => @CurOps.move RowNav bound Nat _ (-(pg : Int)) r
  | '>' => @CurOps.move RowNav bound Nat _ pg r
  | '0' => @CurOps.move RowNav bound Nat _ (-(r.cur : Int)) r
  | '$' => @CurOps.move RowNav bound Nat _ (bound - 1 - r.cur : Int) r
  | _   => r

-- Apply verb to ColNav cursor (pg = half visible cols)
def colVerb {n : Nat} (pg : Nat) (v : Char) (c : ColNav n) : ColNav n :=
  match v with
  | '+' => @CurOps.move (ColNav n) n String _ 1 c
  | '-' => @CurOps.move (ColNav n) n String _ (-1) c
  | '<' => @CurOps.move (ColNav n) n String _ (-(pg : Int)) c
  | '>' => @CurOps.move (ColNav n) n String _ pg c
  | '0' => @CurOps.move (ColNav n) n String _ (-(c.cur.val : Int)) c
  | '$' => @CurOps.move (ColNav n) n String _ (n - 1 - c.cur.val : Int) c
  | _   => c

-- Apply verb to OrdSet
def setVerb [BEq α] (v : Char) (e : α) (s : OrdSet α) : OrdSet α :=
  match v with
  | '+' => @SetOps.add (OrdSet α) α _ e s
  | '-' => @SetOps.remove (OrdSet α) α _ e s
  | '0' => @SetOps.clear (OrdSet α) α _ s
  | '$' => @SetOps.all (OrdSet α) α _ s
  | '^' => @SetOps.toggle (OrdSet α) α _ e s
  | '~' => @SetOps.invert (OrdSet α) α _ s
  | _   => s

-- Dispatch 2-char command (object + verb) to NavState
-- rowPg/colPg = half screen for page up/down
def dispatch {n : Nat} (cmd : String) (t : Nav n) (nav : NavState n t)
    (rowPg colPg : Nat) : NavState n t :=
  let chars := cmd.toList
  if h : chars.length = 2 then
    let obj := chars[0]'(by omega)
    let v := chars[1]'(by omega)
    let curCol := colAt nav.group t.colNames nav.col.cur.val
    match obj with
    | 'r' => { nav with row := rowVerb t.nRows rowPg v nav.row }
    | 'c' => { nav with col := colVerb colPg v nav.col }
    | 'R' => { nav with row := { nav.row with sels := setVerb v nav.row.cur nav.row.sels } }
    | 'C' => match curCol with
             | some c => { nav with col := { nav.col with sels := setVerb v c nav.col.sels } }
             | none => nav
    | 'G' => match curCol with
             | some c => { nav with group := setVerb v c nav.group }
             | none => nav
    | _   => nav
  else nav

end Tc
