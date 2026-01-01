/-
  Typeclass-based navigation state for tabular viewer.

  Key abstractions:
  - Table: dimensions from data source (nRows, colNames)
  - OrdSet: ordered set with invert flag for selections
  - RowNav/ColNav: cursor + offset + selection for row/col
  - Cursor: get/set/move/search typeclass
  - NavState: composes RowNav + ColNav
-/

namespace Tc

/-! ## Classes -/

-- Ops: unified verb-based operations for navigation
-- α = state type, bound = max position (for cursors), elem = element type
-- page = visible items on screen (for offset adjustment in cursor ops)
class Ops (α : Type) (bound : Nat) (elem : Type) where
  plus   : elem → Nat → α → α         -- + : move +1 / add elem
  minus  : elem → Nat → α → α         -- - : move -1 / remove elem
  pageUp : Nat → α → α                -- < : move -page
  pageDn : Nat → α → α                -- > : move +page
  home   : Nat → α → α                -- 0 : first / clear
  end_   : Nat → α → α                -- $ : last / all
  find   : (elem → Bool) → α → α      -- / : search
  toggle : elem → α → α               -- ^ : toggle elem
  invert : α → α                      -- ~ : invert
  mem    : elem → α → Bool            -- ? : membership (internal)

/-! ## Private helpers -/

-- Clamp value to [lo, hi)
def clamp (val lo hi : Nat) : Nat :=
  if hi ≤ lo then lo else max lo (min val (hi - 1))

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

-- Nav: cursor, offset, and selection (generic over elem type)
structure Nav (α : Type) [BEq α] where
  cur  : Nat := 0           -- cursor position
  off  : Nat := 0           -- first visible (scroll offset)
  sels : OrdSet α := {}     -- selected elements

-- Row/Col nav aliases
abbrev RowNav := Nav Nat
abbrev ColNav := Nav String

-- Compute display order: group first, then rest
def dispOrder (group : OrdSet String) (colNames : Array String) : Array String :=
  group.arr ++ colNames.filter (!group.arr.contains ·)

-- Get column name at display index
def colAt (group : OrdSet String) (colNames : Array String) (i : Nat) : Option String :=
  (dispOrder group colNames)[i]?

-- NavState: composes row and column navigation
structure NavState (t : Table) where
  row   : RowNav := {}
  col   : ColNav := {}
  group : OrdSet String := {}  -- group columns (displayed first)

-- Adjust offset to keep cursor visible: off ≤ cur < off + page
def adjOff (cur off page : Nat) : Nat :=
  clamp off (cur + 1 - page) (cur + 1)

-- Row: map from column name to cell value
abbrev Row := String → String

/-! ## Instances -/

-- Nav Ops: cursor + offset adjustment (works for RowNav and ColNav)
instance [BEq α] : Ops (Nav α) bound α where
  plus   := fun _ pg n => move bound pg n (n.cur + 1)
  minus  := fun _ pg n => move bound pg n (n.cur - 1)
  pageUp := fun pg n => move bound pg n (n.cur - pg)
  pageDn := fun pg n => move bound pg n (n.cur + pg)
  home   := fun pg n => move bound pg n 0
  end_   := fun pg n => move bound pg n (bound - 1)
  find   := fun _ n => n  -- TODO
  toggle := fun _ n => n  -- no-op for cursor
  invert := fun n => n    -- no-op for cursor
  mem    := fun _ _ => false
where
  move (bound pg : Nat) (n : Nav α) (newCur : Nat) : Nav α :=
    let c := clamp newCur 0 bound
    { n with cur := c, off := adjOff c n.off pg }

-- OrdSet Ops: set operations (page ignored)
instance [BEq α] : Ops (OrdSet α) 0 α where
  plus   := fun x _ s => { s with arr := dedup (s.arr.push x) }
  minus  := fun x _ s => { s with arr := s.arr.erase x }
  pageUp := fun _ s => s  -- no-op for set
  pageDn := fun _ s => s  -- no-op for set
  home   := fun _ _ => ⟨#[], false⟩  -- clear
  end_   := fun _ _ => ⟨#[], true⟩   -- select all (inverted empty)
  find   := fun _ s => s  -- no-op for set
  toggle := fun x s => if s.arr.contains x
                       then { s with arr := s.arr.erase x }
                       else { s with arr := s.arr.push x }
  invert := fun s => { s with inv := !s.inv }
  mem    := fun x s => s.arr.contains x != s.inv

/-! ## Theorems -/

-- clamp returns value in [lo, hi)
theorem clamp_bounds (val lo hi : Nat) (h : lo < hi) :
    lo ≤ clamp val lo hi ∧ clamp val lo hi < hi := by
  unfold clamp; simp [Nat.not_le.mpr h]; omega

-- adjOff keeps cursor visible: off' ≤ cur < off' + page
theorem adjOff_visible (cur off page : Nat) (hp : 0 < page) :
    let off' := adjOff cur off page
    off' ≤ cur ∧ cur < off' + page := by
  simp only [adjOff, clamp]
  split
  · -- hi ≤ lo: impossible when page > 0
    omega
  · -- normal case
    constructor <;> omega

-- invert twice returns to original
theorem OrdSet.invert_invert [BEq α] (s : OrdSet α) :
    @Ops.invert (OrdSet α) 0 α _ (@Ops.invert (OrdSet α) 0 α _ s) = s := by
  simp only [Ops.invert, Bool.not_not]

-- home (clear) produces empty set
theorem OrdSet.home_empty [BEq α] (s : OrdSet α) :
    @Ops.home (OrdSet α) 0 α _ 0 s = ({} : OrdSet α) := by
  rfl

-- group columns are at front of display order
theorem group_at_front (g : OrdSet String) (colNames : Array String) (i : Nat)
    (h : i < g.arr.size) :
    (dispOrder g colNames)[i]? = g.arr[i]? := by
  simp only [dispOrder]
  rw [Array.getElem?_append_left h]

/-! ## Dispatch -/

-- Apply verb to Nav cursor (elem ignored, just for typeclass)
def navVerb [BEq α] [Inhabited α] (bound pg : Nat) (v : Char) (n : Nav α) : Nav α :=
  match v with
  | '+' => @Ops.plus (Nav α) bound α _ default pg n
  | '-' => @Ops.minus (Nav α) bound α _ default pg n
  | '<' => @Ops.pageUp (Nav α) bound α _ pg n
  | '>' => @Ops.pageDn (Nav α) bound α _ pg n
  | '0' => @Ops.home (Nav α) bound α _ pg n
  | '$' => @Ops.end_ (Nav α) bound α _ pg n
  | _   => n

-- Apply verb to OrdSet
def setVerb [BEq α] (v : Char) (e : α) (s : OrdSet α) : OrdSet α :=
  match v with
  | '+' => @Ops.plus (OrdSet α) 0 α _ e 0 s
  | '-' => @Ops.minus (OrdSet α) 0 α _ e 0 s
  | '0' => @Ops.home (OrdSet α) 0 α _ 0 s
  | '$' => @Ops.end_ (OrdSet α) 0 α _ 0 s
  | '^' => @Ops.toggle (OrdSet α) 0 α _ e s
  | '~' => @Ops.invert (OrdSet α) 0 α _ s
  | _   => s

-- Dispatch 2-char command (object + verb) to NavState
def dispatch (cmd : String) (t : Table) (nav : NavState t) (page : Nat := 10) : NavState t :=
  let chars := cmd.toList
  if h : chars.length = 2 then
    let obj := chars[0]'(by omega)
    let v := chars[1]'(by omega)
    let nr := t.nRows
    let nc := (dispOrder nav.group t.colNames).size
    let curCol := colAt nav.group t.colNames nav.col.cur
    match obj with
    | 'r' => { nav with row := navVerb nr page v nav.row }
    | 'c' => { nav with col := navVerb nc page v nav.col }
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
