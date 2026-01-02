/-
  Typeclass-based navigation state for tabular viewer.

  Key abstractions:
  - NavState: generic over table type + navigation state
  - NavAxis: cursor (Fin) + selections (Array)
  - CurOps: typeclass for cursor operations
-/
import Tc.Offset
import Tc.Data.Table
import Tc.Types

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
def curCol (nav : NavState nRows nCols t) : Nat := nav.col_.cur.val  -- display index
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

-- Apply verb to cursor (pg = page size)
private def curVerb (α : Type) (bound : Nat) (elem : Type) [CurOps α bound elem]
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

-- Dispatch 2-char command (object + verb) to NavState
-- rowPg/colPg = half screen for page up/down
def dispatch (cmd : String) (nav : NavState nRows nCols t) (rowPg colPg : Nat) : NavState nRows nCols t :=
  let chars := cmd.toList
  if h : chars.length = 2 then
    let obj := chars[0]'(by omega)
    let v := chars[1]'(by omega)
    match obj with
    | 'r' => { nav with row_ := curVerb (RowNav nRows) nRows Nat rowPg v nav.row_ }
    | 'c' => { nav with col_ := curVerb (ColNav nCols) nCols String colPg v nav.col_ }
    | 'R' => { nav with row_ := { nav.row_ with sels := nav.row_.sels.toggle nav.row_.cur.val } }
    | 'C' => { nav with col_ := { nav.col_ with sels := nav.col_.sels.toggle nav.curColName } }
    | 'G' => { nav with group_ := nav.group_.toggle nav.curColName }
    | _   => nav
  else nav

end NavState

end Tc
