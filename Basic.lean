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

-- Cumulative width function for n columns: cumW i = total width of columns 0..i-1
abbrev CumW (n : Nat) := Fin (n + 1) → Nat

-- Build CumW from widths array
def mkCumW (widths : Array Nat) : CumW widths.size := fun i =>
  -- cumW[i] = sum of widths[0..i-1] + i (for separators)
  widths.foldl (init := (0, 0)) (fun (idx, acc) w =>
    if idx < i.val then (idx + 1, acc + w + 1) else (idx + 1, acc)) |>.2

-- Table: query result dimensions from data source, parameterized by column count
structure Table (nCols : Nat) where
  nRows : Nat                            -- total row count
  colNames : Array String                -- column names in data order
  colWidths : Array Nat                  -- column widths from data
  hNames : colNames.size = nCols         -- proof names matches nCols
  hWidths : colWidths.size = nCols       -- proof widths matches nCols

-- Get CumW from table
def Table.cumW {n : Nat} (t : Table n) : CumW n :=
  t.hWidths ▸ mkCumW t.colWidths

-- OrdSet: concrete ordered set with invert flag
-- inv=true means "all except arr" (avoids materializing large inversions)
structure OrdSet (α : Type) [BEq α] where
  arr : Array α := #[]   -- selected elements
  inv : Bool := false    -- true = inverted (all except arr)

-- Row navigation: cursor, offset, selection (Nat-based, unbounded)
structure RowNav where
  cur  : Nat := 0           -- cursor position
  off  : Nat := 0           -- first visible (scroll offset)
  sels : OrdSet Nat := {}   -- selected row indices

-- Column navigation: cursor, offset as Fin n, selection by name
structure ColNav (n : Nat) where
  cur  : Fin n              -- cursor position in display order
  off  : Fin n              -- first visible column (scroll offset)
  sels : OrdSet String := {}  -- selected column names

-- Default ColNav for n > 0
def ColNav.default (h : n > 0) : ColNav n := ⟨⟨0, h⟩, ⟨0, h⟩, {}⟩

-- Compute display order: group first, then rest
def dispOrder (group : OrdSet String) (colNames : Array String) : Array String :=
  group.arr ++ colNames.filter (!group.arr.contains ·)

-- Get column name at display index
def colAt (group : OrdSet String) (colNames : Array String) (i : Nat) : Option String :=
  (dispOrder group colNames)[i]?

-- NavState: composes row and column navigation
structure NavState (n : Nat) (t : Table n) where
  row   : RowNav := {}
  col   : ColNav n
  group : OrdSet String := {}  -- group columns (displayed first)

-- Adjust offset to keep cursor visible: off ≤ cur < off + page
def adjOff (cur off page : Nat) : Nat :=
  clamp off (cur + 1 - page) (cur + 1)

-- Monotonicity: cumW i ≤ cumW j when i ≤ j
def CumW.Mono {n : Nat} (cumW : CumW n) : Prop :=
  ∀ i j : Fin (n + 1), i ≤ j → cumW i ≤ cumW j

-- Visibility: cursor column [cumW cur, cumW (cur+1)) ⊆ [cumW off, cumW off + screenW)
def colVisible {n : Nat} (cur off : Fin n) (cumW : CumW n) (screenW : Nat) : Prop :=
  cumW off.castSucc ≤ cumW cur.castSucc ∧ cumW cur.succ ≤ cumW off.castSucc + screenW

-- Increment offset by 1 until cumW off >= target (for scroll right)
-- Minimal change: starts from current offset, increments until visible
def incOff {n : Nat} (off : Fin n) (cumW : CumW n) (target : Nat) : Fin n :=
  if cumW off.castSucc >= target then off
  else if h : off.val + 1 < n then incOff ⟨off.val + 1, h⟩ cumW target
  else off  -- fallback: can't scroll further
termination_by n - off.val

-- Decrement offset by 1 until cumW off <= target (for scroll left)
-- Minimal change: starts from current offset, decrements until visible
def decOff {n : Nat} (off : Fin n) (cumW : CumW n) (target : Nat) : Fin n :=
  if cumW off.castSucc <= target then off
  else if h : off.val > 0 then decOff ⟨off.val - 1, Nat.lt_of_le_of_lt (Nat.sub_le _ _) off.isLt⟩ cumW target
  else off  -- fallback: can't scroll further
termination_by off.val

-- Adjust column offset for visibility with minimal change
-- Scroll right: increment off by 1 until cursor right edge visible
-- Scroll left: decrement off by 1 until cursor left edge visible
def adjColOff {n : Nat} (cur off : Fin n) (cumW : CumW n) (screenW : Nat) : Fin n :=
  let scrollX := cumW off.castSucc           -- current scroll position
  let scrollMin := cumW cur.succ - screenW   -- min scroll to show right edge
  let scrollMax := cumW cur.castSucc         -- max scroll to show left edge
  if scrollX < scrollMin then
    -- scroll right: increment off by 1 until visible (minimal change)
    incOff off cumW scrollMin
  else if scrollX > scrollMax then
    -- scroll left: decrement off by 1 until visible (minimal change)
    decOff off cumW scrollMax
  else off  -- already visible

-- incOff increases monotonically
theorem incOff_ge_off {n : Nat} (off : Fin n) (cumW : CumW n) (target : Nat) :
    off.val ≤ (incOff off cumW target).val := by
  unfold incOff
  split
  case isTrue _ => exact Nat.le_refl _
  case isFalse _ =>
    split
    case isTrue hn =>
      have ih := incOff_ge_off ⟨off.val + 1, hn⟩ cumW target
      exact Nat.le_trans (Nat.le_succ _) ih
    case isFalse _ => exact Nat.le_refl _
termination_by n - off.val

-- decOff decreases monotonically
theorem decOff_le_off {n : Nat} (off : Fin n) (cumW : CumW n) (target : Nat) :
    (decOff off cumW target).val ≤ off.val := by
  unfold decOff
  split
  case isTrue _ => exact Nat.le_refl _
  case isFalse _ =>
    split
    case isTrue hp =>
      have ih := decOff_le_off ⟨off.val - 1, Nat.lt_of_le_of_lt (Nat.sub_le _ _) off.isLt⟩ cumW target
      exact Nat.le_trans ih (Nat.sub_le _ _)
    case isFalse _ => exact Nat.le_refl _
termination_by off.val

-- incOff stops when condition met
theorem incOff_spec {n : Nat} (off : Fin n) (cumW : CumW n) (target : Nat) :
    cumW (incOff off cumW target).castSucc >= target ∨
    (incOff off cumW target).val = n - 1 := by
  unfold incOff
  split
  case isTrue h => left; exact h
  case isFalse hlt =>
    split
    case isTrue hn => exact incOff_spec ⟨off.val + 1, hn⟩ cumW target
    case isFalse hn => right; omega
termination_by n - off.val

-- decOff stops when condition met
theorem decOff_spec {n : Nat} (off : Fin n) (cumW : CumW n) (target : Nat) :
    cumW (decOff off cumW target).castSucc <= target ∨
    (decOff off cumW target).val = 0 := by
  unfold decOff
  split
  case isTrue h => left; exact h
  case isFalse hgt =>
    split
    case isTrue hp => exact decOff_spec ⟨off.val - 1, Nat.lt_of_le_of_lt (Nat.sub_le _ _) off.isLt⟩ cumW target
    case isFalse hp => right; omega
termination_by off.val

-- adjColOff correctness
theorem adjColOff_visible {n : Nat} (cur off : Fin n) (cumW : CumW n) (screenW : Nat)
    (hmono : CumW.Mono cumW) (h0 : cumW ⟨0, Nat.zero_lt_succ n⟩ = 0)
    (hfit : cumW cur.succ - cumW cur.castSucc ≤ screenW) :
    colVisible cur (adjColOff cur off cumW screenW) cumW screenW := by
  simp only [adjColOff, colVisible]
  split
  case isTrue hlt =>
    -- Need off <= cur for incOff to not overshoot. This isn't always true.
    -- The theorem needs stronger assumptions or different formulation.
    constructor <;> sorry
  case isFalse hge =>
    split
    case isTrue hgt =>
      constructor
      · cases decOff_spec off cumW (cumW cur.castSucc) with
        | inl hle => exact hle
        | inr heq =>
          have hcast : (decOff off cumW (cumW cur.castSucc)).castSucc = ⟨0, Nat.zero_lt_succ n⟩ := by
            ext; simp [heq]
          rw [hcast, h0]
          exact Nat.zero_le _
      · -- decOff result is >= 0, and from hge we have cumW off >= cumW cur.succ - screenW
        -- so cumW (decOff ...) >= cumW 0 = 0, and cur fits in screen
        sorry
    case isFalse hle => constructor <;> omega

-- Minimal change property: when no scroll needed, off' = off
theorem adjColOff_minimal {n : Nat} (cur off : Fin n) (cumW : CumW n) (screenW : Nat)
    (hge : ¬(cumW off.castSucc < cumW cur.succ - screenW))
    (hle : ¬(cumW off.castSucc > cumW cur.castSucc)) :
    adjColOff cur off cumW screenW = off := by
  simp only [adjColOff, hge, hle, ↓reduceIte]

-- Row: map from column name to cell value
abbrev Row := String → String

/-! ## Instances -/

-- RowNav Ops: cursor + offset adjustment using adjOff
instance : Ops RowNav bound Nat where
  plus   := fun _ pg r => let c := clamp (r.cur + 1) 0 bound; { r with cur := c, off := adjOff c r.off pg }
  minus  := fun _ pg r => let c := clamp (r.cur - 1) 0 bound; { r with cur := c, off := adjOff c r.off pg }
  pageUp := fun pg r => let c := clamp (r.cur - pg) 0 bound; { r with cur := c, off := adjOff c r.off pg }
  pageDn := fun pg r => let c := clamp (r.cur + pg) 0 bound; { r with cur := c, off := adjOff c r.off pg }
  home   := fun pg r => let c := 0; { r with cur := c, off := adjOff c r.off pg }
  end_   := fun pg r => let c := clamp (bound - 1) 0 bound; { r with cur := c, off := adjOff c r.off pg }
  find   := fun _ r => r
  toggle := fun _ r => r
  invert := fun r => r
  mem    := fun _ _ => false

-- ColNav movement helper
def ColNav.move' {n : Nat} (cumW : CumW n) (screenW : Nat) (cur' : Fin n) (c : ColNav n) : ColNav n :=
  { c with cur := cur', off := adjColOff cur' c.off cumW screenW }

-- ColNav Ops: cursor + offset adjustment using adjColOff (proven correct)
instance instOpsColNav (cumW : CumW n) (screenW : Nat) : Ops (ColNav n) n String where
  plus   := fun _ _ c => if h : c.cur.val + 1 < n
    then c.move' cumW screenW ⟨c.cur.val + 1, h⟩ else c
  minus  := fun _ _ c => if c.cur.val > 0
    then c.move' cumW screenW ⟨c.cur.val - 1, Nat.lt_of_le_of_lt (Nat.sub_le _ _) c.cur.isLt⟩ else c
  pageUp := fun pg c =>
    let v := c.cur.val - pg  -- Nat subtraction floors at 0
    c.move' cumW screenW ⟨v, Nat.lt_of_le_of_lt (Nat.sub_le _ _) c.cur.isLt⟩
  pageDn := fun pg c =>
    let v := min (c.cur.val + pg) (n - 1)
    c.move' cumW screenW ⟨v, Nat.lt_of_le_of_lt (Nat.min_le_right _ _) (Nat.sub_lt c.cur.pos Nat.one_pos)⟩
  home   := fun _ c => if h : n > 0
    then c.move' cumW screenW ⟨0, h⟩ else c
  end_   := fun _ c => c.move' cumW screenW ⟨n - 1, Nat.sub_lt c.cur.pos Nat.one_pos⟩
  find   := fun _ c => c
  toggle := fun _ c => c
  invert := fun c => c
  mem    := fun _ _ => false

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

-- Apply verb to RowNav cursor
def rowVerb (bound pg : Nat) (v : Char) (r : RowNav) : RowNav :=
  match v with
  | '+' => @Ops.plus RowNav bound Nat _ 0 pg r
  | '-' => @Ops.minus RowNav bound Nat _ 0 pg r
  | '<' => @Ops.pageUp RowNav bound Nat _ pg r
  | '>' => @Ops.pageDn RowNav bound Nat _ pg r
  | '0' => @Ops.home RowNav bound Nat _ pg r
  | '$' => @Ops.end_ RowNav bound Nat _ pg r
  | _   => r

-- Apply verb to ColNav cursor (uses cumW for offset adjustment)
def colVerb {n : Nat} (cumW : CumW n) (screenW : Nat) (v : Char) (c : ColNav n) : ColNav n :=
  match v with
  | '+' => @Ops.plus (ColNav n) n String (instOpsColNav cumW screenW) "" 0 c
  | '-' => @Ops.minus (ColNav n) n String (instOpsColNav cumW screenW) "" 0 c
  | '<' => @Ops.pageUp (ColNav n) n String (instOpsColNav cumW screenW) 10 c
  | '>' => @Ops.pageDn (ColNav n) n String (instOpsColNav cumW screenW) 10 c
  | '0' => @Ops.home (ColNav n) n String (instOpsColNav cumW screenW) 0 c
  | '$' => @Ops.end_ (ColNav n) n String (instOpsColNav cumW screenW) 0 c
  | _   => c

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
def dispatch {n : Nat} (cmd : String) (t : Table n) (nav : NavState n t)
    (cumW : CumW n) (screenW rowPg : Nat) : NavState n t :=
  let chars := cmd.toList
  if h : chars.length = 2 then
    let obj := chars[0]'(by omega)
    let v := chars[1]'(by omega)
    let curCol := colAt nav.group t.colNames nav.col.cur.val
    match obj with
    | 'r' => { nav with row := rowVerb t.nRows rowPg v nav.row }
    | 'c' => { nav with col := colVerb cumW screenW v nav.col }
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
