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

-- Ops: unified verb-based operations for navigation
-- α = state type, elem = element type
class Ops (α : Type) (elem : Type) where
  plus   : elem → α → α               -- + : move +1 / add elem
  minus  : elem → α → α               -- - : move -1 / remove elem
  pageUp : Nat → α → α                -- < : move -page
  pageDn : Nat → α → α                -- > : move +page
  home   : α → α                      -- 0 : first / clear
  end_   : Nat → α → α                -- $ : last / all (takes bound)
  find   : (elem → Bool) → α → α      -- / : search
  toggle : elem → α → α               -- ^ : toggle elem
  invert : α → α                      -- ~ : invert
  mem    : elem → α → Bool            -- ? : membership (internal)

/-! ## Private helpers -/

-- Remove duplicates keeping first occurrence
private def dedup [BEq α] (a : Array α) : Array α :=
  a.foldl (fun acc x => if acc.contains x then acc else acc.push x) #[]

/-! ## Structures -/

-- Nav: query result dimensions from data source, parameterized by column count
structure Nav (nCols : Nat) where
  nRows : Nat                            -- total row count
  colNames : Array String                -- column names in data order
  colWidths : Array Nat                  -- column widths from data
  hNames : colNames.size = nCols         -- proof names matches nCols
  hWidths : colWidths.size = nCols       -- proof widths matches nCols

-- Get CumW from Nav
def Nav.cumW {n : Nat} (t : Nav n) : CumW n :=
  t.hWidths ▸ mkCumW t.colWidths

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

-- RowNav Ops: cursor movement (bound passed to end_)
instance : Ops RowNav Nat where
  plus   := fun _ r => { r with cur := r.cur + 1 }
  minus  := fun _ r => { r with cur := r.cur - 1 }
  pageUp := fun pg r => { r with cur := r.cur - pg }
  pageDn := fun pg r => { r with cur := r.cur + pg }
  home   := fun r => { r with cur := 0 }
  end_   := fun bound r => { r with cur := bound - 1 }
  find   := fun _ r => r
  toggle := fun _ r => r
  invert := fun r => r
  mem    := fun _ _ => false

-- ColNav Ops: cursor movement (n is column count)
instance : Ops (ColNav n) String where
  plus   := fun _ c => if h : c.cur.val + 1 < n
    then { c with cur := ⟨c.cur.val + 1, h⟩ } else c
  minus  := fun _ c => if c.cur.val > 0
    then { c with cur := ⟨c.cur.val - 1, Nat.lt_of_le_of_lt (Nat.sub_le _ _) c.cur.isLt⟩ } else c
  pageUp := fun pg c =>
    let v := c.cur.val - pg
    { c with cur := ⟨v, Nat.lt_of_le_of_lt (Nat.sub_le _ _) c.cur.isLt⟩ }
  pageDn := fun pg c =>
    let v := min (c.cur.val + pg) (n - 1)
    { c with cur := ⟨v, Nat.lt_of_le_of_lt (Nat.min_le_right _ _) (Nat.sub_lt c.cur.pos Nat.one_pos)⟩ }
  home   := fun c => if h : n > 0
    then { c with cur := ⟨0, h⟩ } else c
  end_   := fun _ c => { c with cur := ⟨n - 1, Nat.sub_lt c.cur.pos Nat.one_pos⟩ }
  find   := fun _ c => c
  toggle := fun _ c => c
  invert := fun c => c
  mem    := fun _ _ => false

-- OrdSet Ops: set operations
instance [BEq α] : Ops (OrdSet α) α where
  plus   := fun x s => { s with arr := dedup (s.arr.push x) }
  minus  := fun x s => { s with arr := s.arr.erase x }
  pageUp := fun _ s => s  -- no-op for set
  pageDn := fun _ s => s  -- no-op for set
  home   := fun _ => ⟨#[], false⟩  -- clear
  end_   := fun _ _ => ⟨#[], true⟩   -- select all (inverted empty)
  find   := fun _ s => s  -- no-op for set
  toggle := fun x s => if s.arr.contains x
                       then { s with arr := s.arr.erase x }
                       else { s with arr := s.arr.push x }
  invert := fun s => { s with inv := !s.inv }
  mem    := fun x s => s.arr.contains x != s.inv

/-! ## Theorems -/

-- invert twice returns to original
theorem OrdSet.invert_invert [BEq α] (s : OrdSet α) :
    @Ops.invert (OrdSet α) α _ (@Ops.invert (OrdSet α) α _ s) = s := by
  simp only [Ops.invert, Bool.not_not]

-- home (clear) produces empty set
theorem OrdSet.home_empty [BEq α] (s : OrdSet α) :
    @Ops.home (OrdSet α) α _ s = ({} : OrdSet α) := by
  rfl

-- group columns are at front of display order
theorem group_at_front (g : OrdSet String) (colNames : Array String) (i : Nat)
    (h : i < g.arr.size) :
    (dispOrder g colNames)[i]? = g.arr[i]? := by
  simp only [dispOrder]
  rw [Array.getElem?_append_left h]

/-! ## Dispatch -/

-- Apply verb to RowNav cursor
def rowVerb (bound : Nat) (v : Char) (r : RowNav) : RowNav :=
  match v with
  | '+' => (Ops.plus (α := RowNav) (elem := Nat)) 0 r
  | '-' => (Ops.minus (α := RowNav) (elem := Nat)) 0 r
  | '<' => (Ops.pageUp (α := RowNav) (elem := Nat)) 10 r
  | '>' => (Ops.pageDn (α := RowNav) (elem := Nat)) 10 r
  | '0' => (Ops.home (α := RowNav) (elem := Nat)) r
  | '$' => (Ops.end_ (α := RowNav) (elem := Nat)) bound r
  | _   => r

-- Apply verb to ColNav cursor
def colVerb {n : Nat} (v : Char) (c : ColNav n) : ColNav n :=
  match v with
  | '+' => (Ops.plus (α := ColNav n) (elem := String)) "" c
  | '-' => (Ops.minus (α := ColNav n) (elem := String)) "" c
  | '<' => (Ops.pageUp (α := ColNav n) (elem := String)) 10 c
  | '>' => (Ops.pageDn (α := ColNav n) (elem := String)) 10 c
  | '0' => (Ops.home (α := ColNav n) (elem := String)) c
  | '$' => (Ops.end_ (α := ColNav n) (elem := String)) n c
  | _   => c

-- Apply verb to OrdSet
def setVerb [BEq α] (v : Char) (e : α) (s : OrdSet α) : OrdSet α :=
  match v with
  | '+' => (Ops.plus (α := OrdSet α) (elem := α)) e s
  | '-' => (Ops.minus (α := OrdSet α) (elem := α)) e s
  | '0' => (Ops.home (α := OrdSet α) (elem := α)) s
  | '$' => (Ops.end_ (α := OrdSet α) (elem := α)) 0 s
  | '^' => (Ops.toggle (α := OrdSet α) (elem := α)) e s
  | '~' => (Ops.invert (α := OrdSet α) (elem := α)) s
  | _   => s

-- Dispatch 2-char command (object + verb) to NavState
def dispatch {n : Nat} (cmd : String) (t : Nav n) (nav : NavState n t) : NavState n t :=
  let chars := cmd.toList
  if h : chars.length = 2 then
    let obj := chars[0]'(by omega)
    let v := chars[1]'(by omega)
    let curCol := colAt nav.group t.colNames nav.col.cur.val
    match obj with
    | 'r' => { nav with row := rowVerb t.nRows v nav.row }
    | 'c' => { nav with col := colVerb v nav.col }
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
