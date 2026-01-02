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

-- Type aliases
abbrev RowNav (m : Nat) := NavAxis m Nat
abbrev ColNav (n : Nat) := NavAxis n String

-- Compute display order: group first, then rest
def dispOrder (group : Array String) (colNames : Array String) : Array String :=
  group ++ colNames.filter (!group.contains ·)

-- dispOrder preserves size when group ⊆ colNames
theorem dispOrder_size (group colNames : Array String)
    (h : group.all (colNames.contains ·)) :
    (dispOrder group colNames).size = colNames.size := by
  simp only [dispOrder, Array.size_append]
  sorry -- filter removes exactly group.size elements

-- Get column name at display index (group ⊆ colNames required)
def colAt (group colNames : Array String) (i : Fin colNames.size)
    (h : group.all (colNames.contains ·)) : String :=
  (dispOrder group colNames)[i.val]'(by simp [dispOrder_size group colNames h])

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
  private group_ : Array String := #[]                            -- grouped columns
  private hGroup_: group_.all ((ReadTable.colNames tbl_).contains ·) := by decide

namespace NavState

variable {t : Type} [ReadTable t]

-- Accessors
def tbl (nav : NavState nRows nCols t) : t := nav.tbl_
def curRow (nav : NavState nRows nCols t) : Nat := nav.row_.cur.val
def curCol (nav : NavState nRows nCols t) : Nat := nav.col_.cur.val  -- display index
def colNames (nav : NavState nRows nCols t) : Array String := ReadTable.colNames nav.tbl_
def nKeys (nav : NavState nRows nCols t) : Nat := nav.group_.size
def selRows (nav : NavState nRows nCols t) : Array Nat := nav.row_.sels

-- Selected column names
def selCols (nav : NavState nRows nCols t) : Array String := nav.col_.sels

-- Selected column indices (in original order)
def selColIdxs (nav : NavState nRows nCols t) : Array Nat :=
  nav.col_.sels.filterMap fun name => nav.colNames.findIdx? (· == name)

-- Current column name in display order
def curColName (nav : NavState nRows nCols t) : String :=
  let i : Fin (ReadTable.colNames nav.tbl_).size := ⟨nav.col_.cur.val, nav.hCols_.symm ▸ nav.col_.cur.isLt⟩
  colAt nav.group_ nav.colNames i nav.hGroup_

-- Current column index (in original order)
def curColIdx (nav : NavState nRows nCols t) : Nat :=
  nav.colNames.findIdx? (· == nav.curColName) |>.getD 0

-- Column indices in display order
def dispColIdxs (nav : NavState nRows nCols t) : Array Nat :=
  (dispOrder nav.group_ nav.colNames).filterMap fun name => nav.colNames.findIdx? (· == name)

-- Constructor for external use
def new (tbl : t) (hRows : ReadTable.nRows tbl = nRows) (hCols : (ReadTable.colNames tbl).size = nCols)
    (hr : nRows > 0) (hc : nCols > 0) : NavState nRows nCols t :=
  ⟨tbl, hRows, hCols, NavAxis.default hr, NavAxis.default hc, #[], Array.all_empty⟩

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
    let curCol := nav.curColName
    match obj with
    | 'r' => { nav with row_ := curVerb (RowNav nRows) nRows Nat rowPg v nav.row_ }
    | 'c' => { nav with col_ := curVerb (ColNav nCols) nCols String colPg v nav.col_ }
    | 'R' => { nav with row_ := { nav.row_ with sels := nav.row_.sels.toggle nav.row_.cur.val } }
    | 'C' => { nav with col_ := { nav.col_ with sels := nav.col_.sels.toggle curCol } }
    | 'G' => { nav with group_ := nav.group_.toggle curCol, hGroup_ := sorry }
    | _   => nav
  else nav

end NavState

end Tc
