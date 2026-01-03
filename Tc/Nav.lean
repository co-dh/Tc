/-
  Typeclass-based navigation state for tabular viewer.

  Key abstractions:
  - NavState: generic over table type + navigation state
  - NavAxis: cursor (Fin) + selections (Array)
  - NavCmd: type-safe navigation commands
-/
import Tc.Offset
import Tc.Data.Table
import Tc.Types

-- Verb: movement + toggle + del + sort
inductive Verb where
  | next | prev | pgNext | pgPrev | home | end_  -- movement
  | toggle                                        -- toggle
  | del                                           -- delete
  | sortAsc | sortDesc                            -- sort
  deriving Repr, BEq, DecidableEq

namespace Verb
-- Verb to char: n/p/N/P/h/e/t/d/[/]
def toChar : Verb → Char
  | .next => 'n' | .prev => 'p' | .pgNext => 'N' | .pgPrev => 'P'
  | .home => 'h' | .end_ => 'e' | .toggle => 't' | .del => 'd'
  | .sortAsc => '[' | .sortDesc => ']'

-- Char to verb
def ofChar? : Char → Option Verb
  | 'n' => some .next | 'p' => some .prev | 'N' => some .pgNext | 'P' => some .pgPrev
  | 'h' => some .home | 'e' => some .end_ | 't' => some .toggle | 'd' => some .del
  | '[' => some .sortAsc | ']' => some .sortDesc
  | _ => none

-- Isomorphism: ofChar? ∘ toChar = some
theorem ofChar_toChar (v : Verb) : ofChar? (toChar v) = some v := by
  cases v <;> rfl
end Verb

-- Command: Obj Verb pattern
inductive Cmd where
  | row (v : Verb)     -- row next/prev/...
  | col (v : Verb)     -- col next/prev/.../del
  | rowSel (v : Verb)  -- rowSel toggle
  | colSel (v : Verb)  -- colSel toggle/sortAsc/sortDesc (sort by selected cols)
  | grp (v : Verb)     -- grp toggle
  deriving Repr, BEq, DecidableEq

namespace Cmd
-- Cmd to string: "rn" "cp" "C[" etc
def toString : Cmd → String
  | .row v    => s!"r{v.toChar}"
  | .col v    => s!"c{v.toChar}"
  | .rowSel v => s!"R{v.toChar}"
  | .colSel v => s!"C{v.toChar}"
  | .grp v    => s!"g{v.toChar}"

-- String to cmd: "rn" → row next, "C[" → colSel sortAsc
def ofString? (s : String) : Option Cmd :=
  if s.length != 2 then none else
  let obj := s.toList[0]!
  let vrb := s.toList[1]!
  match Verb.ofChar? vrb with
  | none => none
  | some v => match obj with
    | 'r' => some (.row v)
    | 'c' => some (.col v)
    | 'R' => some (.rowSel v)
    | 'C' => some (.colSel v)
    | 'g' => some (.grp v)
    | _ => none

-- Parse space-separated command string
def parseMany (s : String) : Array Cmd :=
  (s.splitOn " ").toArray.filterMap ofString?

-- Isomorphism: ofString? ∘ toString = some
theorem ofString_toString (c : Cmd) : ofString? (toString c) = some c := by
  cases c with
  | row v => cases v <;> native_decide
  | col v => cases v <;> native_decide
  | rowSel v => cases v <;> native_decide
  | colSel v => cases v <;> native_decide
  | grp v => cases v <;> native_decide
end Cmd

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

-- NavAxis CurOps (covers RowNav and ColNav)
instance [BEq elem] : CurOps (NavAxis n elem) n elem where
  pos    := (·.cur)
  setPos := fun f a => { a with cur := f }

-- Type aliases: Row uses Nat (index), Col uses String (name, stable across deletion)
abbrev RowNav (m : Nat) := NavAxis m Nat
abbrev ColNav (n : Nat) := NavAxis n String

-- Find index of element in array (O(n) linear scan)
def Array.idxOf? [BEq α] (a : Array α) (x : α) : Option Nat :=
  a.findIdx? (· == x)

-- Compute display order: group names first, then rest (by name lookup)
def dispOrder (group : Array String) (names : Array String) : Array Nat :=
  let gIdxs := group.filterMap names.idxOf?
  gIdxs ++ (Array.range names.size).filter (!gIdxs.contains ·)

-- dispOrder preserves size (modulo valid group names)
theorem dispOrder_size (group : Array String) (names : Array String) :
    (dispOrder group names).size = names.size := by
  simp only [dispOrder, Array.size_append]
  sorry -- filter removes exactly valid group.size elements

-- Get column index at display position
def colIdxAt (group : Array String) (names : Array String) (i : Nat) : Nat :=
  (dispOrder group names).getD i 0

-- NavState: generic over table type + navigation state
-- nRows/nCols are type params (not phantom) because Fin needs compile-time bounds.
-- ReadTable.nRows tbl is runtime (depends on tbl field), can't use directly in Fin.
-- So we lift values to type level, then prove they match table via hRows/hCols.
structure NavState (nRows nCols : Nat) (t : Type) [ReadTable t] where
  private mk ::
  private tbl_   : t                                              -- underlying table
  private hRows_ : ReadTable.nRows tbl_ = nRows                   -- row count matches
  private hCols_ : (ReadTable.colNames tbl_).size = nCols         -- col count matches
  private row_   : RowNav nRows
  private col_   : ColNav nCols
  private group_ : Array String := #[]                            -- grouped column names (stable)

namespace NavState

variable {t : Type} [ReadTable t]

-- Accessors
def tbl (nav : NavState nRows nCols t) : t := nav.tbl_
def curRow (nav : NavState nRows nCols t) : Nat := nav.row_.cur.val
def curDispCol (nav : NavState nRows nCols t) : Nat := nav.col_.cur.val  -- display index
def colNames (nav : NavState nRows nCols t) : Array String := ReadTable.colNames nav.tbl_
def nKeys (nav : NavState nRows nCols t) : Nat := nav.group_.size
def group (nav : NavState nRows nCols t) : Array String := nav.group_
def selRows (nav : NavState nRows nCols t) : Array Nat := nav.row_.sels

-- Selected column indices (convert names to indices via lookup)
def selColIdxs (nav : NavState nRows nCols t) : Array Nat :=
  nav.col_.sels.filterMap nav.colNames.idxOf?

-- Column indices in display order
def dispColIdxs (nav : NavState nRows nCols t) : Array Nat :=
  dispOrder nav.group_ nav.colNames

-- Current column index (in original order)
def curColIdx (nav : NavState nRows nCols t) : Nat :=
  colIdxAt nav.group_ nav.colNames nav.col_.cur.val

-- Current column name in display order
def curColName (nav : NavState nRows nCols t) : String :=
  nav.colNames.getD nav.curColIdx ""

-- Constructor for external use
def new (tbl : t) (hRows : ReadTable.nRows tbl = nRows) (hCols : (ReadTable.colNames tbl).size = nCols)
    (hr : nRows > 0) (hc : nCols > 0) : NavState nRows nCols t :=
  ⟨tbl, hRows, hCols, NavAxis.default hr, NavAxis.default hc, #[]⟩

-- Constructor with initial column cursor and group (clamped to valid range)
def newAt (tbl : t) (hRows : ReadTable.nRows tbl = nRows) (hCols : (ReadTable.colNames tbl).size = nCols)
    (hr : nRows > 0) (hc : nCols > 0) (col : Nat) (grp : Array String := #[]) : NavState nRows nCols t :=
  let c := min col (nCols - 1)
  have hlt : c < nCols := Nat.lt_of_le_of_lt (Nat.min_le_right ..) (Nat.sub_lt hc Nat.one_pos)
  ⟨tbl, hRows, hCols, NavAxis.default hr, ⟨⟨c, hlt⟩, #[]⟩, grp⟩

-- Move cursor by verb (generic over axis)
private def move (cur : Fin n) (v : Verb) (pg : Nat) : Fin n :=
  match v with
  | .next   => cur.clamp 1
  | .prev   => cur.clamp (-1)
  | .pgNext => cur.clamp pg
  | .pgPrev => cur.clamp (-(pg : Int))
  | .home   => cur.clamp (-(cur.val : Int))
  | .end_   => cur.clamp (n - 1 - cur.val : Int)
  | _       => cur  -- toggle/del not applicable

-- Dispatch Cmd: object first, then verb
def dispatch (cmd : Cmd) (nav : NavState nRows nCols t) (rowPg colPg : Nat) : NavState nRows nCols t :=
  match cmd with
  | .row v    => { nav with row_ := { nav.row_ with cur := move nav.row_.cur v rowPg } }
  | .col v    => match v with
    | .del => nav  -- handled in App.lean
    | _ => { nav with col_ := { nav.col_ with cur := move nav.col_.cur v colPg } }
  | .rowSel .toggle => { nav with row_ := { nav.row_ with sels := nav.row_.sels.toggle nav.row_.cur.val } }
  | .colSel .toggle => { nav with col_ := { nav.col_ with sels := nav.col_.sels.toggle nav.curColName } }
  | .grp .toggle    => { nav with group_ := nav.group_.toggle nav.curColName }
  | _ => nav

end NavState

end Tc
