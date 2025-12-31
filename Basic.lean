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
class Ops (α : Type) (bound : Nat) (elem : Type) where
  plus   : Option elem → α → α        -- + : move +1 / add elem
  minus  : Option elem → α → α        -- - : move -1 / remove elem
  pageUp : Nat → α → α                -- < : move -page
  pageDn : Nat → α → α                -- > : move +page
  home   : α → α                      -- 0 : first / clear
  end_   : α → α                      -- $ : last / all
  find   : (elem → Bool) → α → α      -- / : search
  toggle : Option elem → α → α        -- ^ : toggle elem
  invert : α → α                      -- ~ : invert
  mem    : elem → α → Bool            -- ? : membership (internal)

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

-- Adjust offset to keep cursor on screen: off <= cur < off + page
-- page = screen capacity (e.g., terminal has 20 rows, page=20)
def adjOff (cur off page : Nat) : Nat :=
  min cur (max off (cur + 1 - page))

-- Row: map from column name to cell value
abbrev Row := String → String

/-! ## Instances -/

-- RowNav Ops: cursor operations (elem ignored for +/-/^)
instance : Ops RowNav bound Row where
  plus   := fun _ r => { r with cur := clamp (r.cur + 1) bound }
  minus  := fun _ r => { r with cur := clamp (r.cur - 1) bound }
  pageUp := fun n r => { r with cur := clamp (r.cur - n) bound }
  pageDn := fun n r => { r with cur := clamp (r.cur + n) bound }
  home   := fun r => { r with cur := 0 }
  end_   := fun r => { r with cur := clamp (bound - 1) bound }
  find   := fun _ r => r  -- TODO
  toggle := fun _ r => r  -- no-op for cursor
  invert := fun r => r    -- no-op for cursor
  mem    := fun _ _ => false

-- ColNav Ops: cursor operations (elem ignored for +/-/^)
instance : Ops ColNav bound String where
  plus   := fun _ c => { c with cur := clampDisp (c.cur.val + 1) bound }
  minus  := fun _ c => { c with cur := clampDisp (c.cur.val - 1) bound }
  pageUp := fun n c => { c with cur := clampDisp (c.cur.val - n) bound }
  pageDn := fun n c => { c with cur := clampDisp (c.cur.val + n) bound }
  home   := fun c => { c with cur := ⟨0⟩ }
  end_   := fun c => { c with cur := clampDisp (bound - 1) bound }
  find   := fun _ c => c  -- TODO
  toggle := fun _ c => c  -- no-op for cursor
  invert := fun c => c    -- no-op for cursor
  mem    := fun _ _ => false

-- OrdSet Ops: set operations (bound=0 unused, elem required for +/-/^)
instance [BEq α] : Ops (OrdSet α) 0 α where
  plus   := fun e s => match e with
    | some x => { s with arr := dedup (s.arr.push x) }
    | none => s
  minus  := fun e s => match e with
    | some x => { s with arr := s.arr.erase x }
    | none => s
  pageUp := fun _ s => s  -- no-op for set
  pageDn := fun _ s => s  -- no-op for set
  home   := fun _ => ⟨#[], false⟩  -- clear
  end_   := fun _ => ⟨#[], true⟩   -- select all (inverted empty)
  find   := fun _ s => s  -- no-op for set
  toggle := fun e s => match e with
    | some x => if s.arr.contains x
                then { s with arr := s.arr.erase x }
                else { s with arr := s.arr.push x }
    | none => s
  invert := fun s => { s with inv := !s.inv }
  mem    := fun x s => s.arr.contains x != s.inv

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
    @Ops.invert (OrdSet α) 0 α _ (@Ops.invert (OrdSet α) 0 α _ s) = s := by
  simp only [Ops.invert, Bool.not_not]

-- home (clear) produces empty set
theorem OrdSet.home_empty [BEq α] (s : OrdSet α) :
    @Ops.home (OrdSet α) 0 α _ s = ({} : OrdSet α) := by
  rfl

-- group columns are at front of display order
theorem ColNav.group_at_front (c : ColNav) (colNames : Array String) (i : Nat)
    (h : i < c.group.arr.size) :
    (c.dispOrder colNames)[i]? = c.group.arr[i]? := by
  simp only [dispOrder]
  rw [Array.getElem?_append_left h]

/-! ## Dispatch -/

-- Apply verb to cursor (generic)
def curVerb (α : Type) (bound : Nat) (elem : Type) [inst : Ops α bound elem]
    (v : Char) (page : Nat) (x : α) : α :=
  match v with
  | '+' => @Ops.plus α bound elem inst none x
  | '-' => @Ops.minus α bound elem inst none x
  | '<' => @Ops.pageUp α bound elem inst page x
  | '>' => @Ops.pageDn α bound elem inst page x
  | '0' => @Ops.home α bound elem inst x
  | '$' => @Ops.end_ α bound elem inst x
  | _   => x

-- Apply verb to OrdSet
def setVerb [BEq α] (v : Char) (e : Option α) (s : OrdSet α) : OrdSet α :=
  match v with
  | '+' => @Ops.plus (OrdSet α) 0 α _ e s
  | '-' => @Ops.minus (OrdSet α) 0 α _ e s
  | '0' => @Ops.home (OrdSet α) 0 α _ s
  | '$' => @Ops.end_ (OrdSet α) 0 α _ s
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
    let nc := (nav.col.dispOrder t.colNames).size
    let curCol := nav.col.colAt t.colNames nav.col.cur
    match obj with
    | 'r' => { nav with row := curVerb RowNav nr Row v page nav.row }
    | 'c' => { nav with col := curVerb ColNav nc String v page nav.col }
    | 'R' => { nav with row := { nav.row with sels := setVerb v (some nav.row.cur) nav.row.sels } }
    | 'C' => { nav with col := { nav.col with sels := setVerb v curCol nav.col.sels } }
    | 'G' => { nav with col := { nav.col with group := setVerb v curCol nav.col.group } }
    | _   => nav
  else nav

end Tc
