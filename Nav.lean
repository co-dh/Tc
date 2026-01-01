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

-- SetOps: set operations (just toggle)
class SetOps (α : Type) (elem : Type) where
  toggle : elem → α → α               -- ~ : toggle elem

/-! ## Structures -/

-- Nav: query result dimensions from data source, parameterized by column count
structure Nav (nCols : Nat) where
  nRows : Nat                            -- total row count
  colNames : Array String                -- column names in data order
  hNames : colNames.size = nCols         -- proof names matches nCols

-- OrdSet: ordered set of selected elements
structure OrdSet (α : Type) [BEq α] where
  arr : Array α := #[]   -- selected elements

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

/-! ## Instances -/

-- RowNav CurOps
instance : CurOps (RowNav m) m Nat where
  pos    := (·.cur)
  setPos := fun f r => { r with cur := f }

-- ColNav CurOps
instance : CurOps (ColNav n) n String where
  pos    := (·.cur)
  setPos := fun f c => { c with cur := f }

-- OrdSet SetOps: toggle only
instance [BEq α] : SetOps (OrdSet α) α where
  toggle := fun x s => if s.arr.contains x
                       then { s with arr := s.arr.erase x }
                       else { s with arr := s.arr.push x }

/-! ## Theorems -/

-- group columns are at front of display order
theorem group_at_front (g : OrdSet String) (colNames : Array String) (i : Nat)
    (h : i < g.arr.size) :
    (dispOrder g colNames)[i]? = g.arr[i]? := by
  simp only [dispOrder]
  rw [Array.getElem?_append_left h]

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
    | 'r' => { nav with row := curVerb (RowNav t.nRows) t.nRows Nat rowPg v nav.row }
    | 'c' => { nav with col := curVerb (ColNav n) n String colPg v nav.col }
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
