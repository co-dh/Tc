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

-- SetOps: set operations (just toggle)
class SetOps (α : Type) (elem : Type) where
  toggle : elem → α → α               -- ~ : toggle elem

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

-- Row navigation: cursor as Fin m + selection (offset is view state)
structure RowNav (m : Nat) where
  cur  : Fin m              -- cursor position bounded by nRows
  sels : OrdSet Nat := {}   -- selected row indices

-- Default RowNav for m > 0
def RowNav.default (h : m > 0) : RowNav m := ⟨⟨0, h⟩, {}⟩

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
  row   : RowNav t.nRows
  col   : ColNav n
  group : OrdSet String := {}  -- group columns (displayed first)

-- Row: map from column name to cell value
abbrev Row := String → String

end Tc

-- Clamp Fin by delta, staying in [0, n)
namespace Fin
def clamp (f : Fin n) (d : Int) : Fin n :=
  let v := ((f.val : Int) + d).toNat
  let v' := min v (n - 1)
  ⟨v', Nat.lt_of_le_of_lt (Nat.min_le_right _ _) (Nat.sub_lt f.pos Nat.one_pos)⟩
end Fin

namespace Tc

/-! ## Instances -/

-- RowNav CurOps: move by delta, clamped to [0, m)
instance : CurOps (RowNav m) m Nat where
  move := fun d r => { r with cur := r.cur.clamp d }
  find := fun _ r => r

-- ColNav CurOps: move by delta, clamped to [0, n)
instance : CurOps (ColNav n) n String where
  move := fun d c => { c with cur := c.cur.clamp d }
  find := fun _ c => c

-- OrdSet SetOps: toggle only
instance [BEq α] : SetOps (OrdSet α) α where
  toggle := fun x s => if s.arr.contains x
                       then { s with arr := s.arr.erase x }
                       else { s with arr := s.arr.push x }

-- OrdSet membership (not in class, used internally)
def OrdSet.mem [BEq α] (x : α) (s : OrdSet α) : Bool := s.arr.contains x != s.inv

/-! ## Theorems -/

-- group columns are at front of display order
theorem group_at_front (g : OrdSet String) (colNames : Array String) (i : Nat)
    (h : i < g.arr.size) :
    (dispOrder g colNames)[i]? = g.arr[i]? := by
  simp only [dispOrder]
  rw [Array.getElem?_append_left h]

/-! ## Dispatch -/

-- Apply verb to RowNav cursor (pg = half screen)
def rowVerb {m : Nat} (pg : Nat) (v : Char) (r : RowNav m) : RowNav m :=
  match v with
  | '+' => @CurOps.move (RowNav m) m Nat _ (1 : Int) r
  | '-' => @CurOps.move (RowNav m) m Nat _ (-1 : Int) r
  | '<' => @CurOps.move (RowNav m) m Nat _ (-(pg : Int)) r
  | '>' => @CurOps.move (RowNav m) m Nat _ (pg : Int) r
  | '0' => @CurOps.move (RowNav m) m Nat _ (-(r.cur.val : Int)) r
  | '$' => @CurOps.move (RowNav m) m Nat _ (m - 1 - r.cur.val : Int) r
  | _   => r

-- Apply verb to ColNav cursor (pg = half visible cols)
def colVerb {n : Nat} (pg : Nat) (v : Char) (c : ColNav n) : ColNav n :=
  match v with
  | '+' => @CurOps.move (ColNav n) n String _ (1 : Int) c
  | '-' => @CurOps.move (ColNav n) n String _ (-1 : Int) c
  | '<' => @CurOps.move (ColNav n) n String _ (-(pg : Int)) c
  | '>' => @CurOps.move (ColNav n) n String _ (pg : Int) c
  | '0' => @CurOps.move (ColNav n) n String _ (-(c.cur.val : Int)) c
  | '$' => @CurOps.move (ColNav n) n String _ (n - 1 - c.cur.val : Int) c
  | _   => c

-- Apply verb to OrdSet (toggle only)
def setVerb [BEq α] (v : Char) (e : α) (s : OrdSet α) : OrdSet α :=
  match v with
  | '~' => @SetOps.toggle (OrdSet α) α _ e s
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
    | 'r' => { nav with row := rowVerb rowPg v nav.row }
    | 'c' => { nav with col := colVerb colPg v nav.col }
    | 'R' => { nav with row := { nav.row with sels := setVerb v nav.row.cur.val nav.row.sels } }
    | 'C' => match curCol with
             | some c => { nav with col := { nav.col with sels := setVerb v c nav.col.sels } }
             | none => nav
    | 'G' => match curCol with
             | some c => { nav with group := setVerb v c nav.group }
             | none => nav
    | _   => nav
  else nav

end Tc
