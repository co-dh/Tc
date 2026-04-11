/-
  Typeclass-based navigation state for tabular viewer.

  Key abstractions:
  - NavState: generic over table type + navigation state
  - NavAxis: cursor (Nat) + selections (Array)
-/
import Tc.Types
import Tc.Lens

namespace Tc
-- Clamp value to [lo, hi)
def clamp (val lo hi : Nat) : Nat := if hi ≤ lo then lo else max lo (min val (hi - 1))
-- Adjust offset to keep cursor visible: off ≤ cur < off + page
def adjOff (cur off page : Nat) : Nat := clamp off (cur + 1 - page) (cur + 1)

-- | Shift a Nat cursor by Int delta, then clamp to [0, n). `n = 0` is defensive only —
-- `NavState.newAt` refuses to build empty navs, so all live cursors have `n > 0`.
@[inline] def clampShift (cur : Nat) (d : Int) (n : Nat) : Nat :=
  if n == 0 then 0 else min (((cur : Int) + d).toNat) (n - 1)

/-! ## Structures -/

-- NavAxis: cursor + selection for one axis (row or col)
structure NavAxis (elem : Type) [BEq elem] where
  cur  : Nat := 0           -- cursor position (clamped at call sites)
  sels : Array elem := #[]  -- selected elements

-- | Field lenses for NavAxis — auto-generated, enable composable nested updates via `∘ₗ`.
namespace NavAxis
variable {elem : Type} [BEq elem]
gen_lenses (NavAxis elem) where cur, sels
end NavAxis

-- Type aliases: Row uses Nat (index), Col uses String (name, stable across deletion)
abbrev RowNav := NavAxis Nat
abbrev ColNav := NavAxis String

-- Find index of element in array (O(n) linear scan)
def Array.idxOf? [BEq α] (a : Array α) (x : α) : Option Nat :=
  a.findIdx? (· == x)

-- Compute display order: group names first (in grp array order), then rest
-- Group order matters for join key ordering (Shift+Arrow reorders grp)
def dispOrder (group : Array String) (names : Array String) : Array Nat :=
  let n := names.size
  let isGrp := fun i => group.contains (names.getD i "")
  -- Sort group indices by position in grp array (respects grp add/reorder order)
  let grpSorted := Array.range n |>.filter isGrp |>.qsort fun a b =>
    (group.idxOf? (names.getD a "")).getD 0 < (group.idxOf? (names.getD b "")).getD 0
  grpSorted ++ (Array.range n |>.filter (!isGrp ·))

-- When a column is grouped, its index appears first in dispOrder
theorem dispOrder_grp_first :
    (dispOrder #["c1"] #["c0", "c1", "c2"]).getD 0 999 = 1 := by native_decide

-- Get column index at display position
def colIdxAt (group : Array String) (names : Array String) (i : Nat) : Nat :=
  (dispOrder group names).getD i 0

-- | NavState: table + navigation cursors + column visibility.
-- `nRows`/`nCols` are cached so the render/keystroke hot path avoids recomputing
-- `(TblOps.colNames tbl).size`. `newAt` enforces `nRows, nCols > 0`, so `nRows - 1`
-- is safe without re-checking.
structure NavState (t : Type) [TblOps t] where
  tbl      : t
  nRows    : Nat
  nCols    : Nat
  row      : RowNav := {}
  col      : ColNav := {}
  grp      : Array String := #[]              -- grouped column names (stable across reorder)
  hidden   : Array String := #[]              -- hidden column names (width=1)
  dispIdxs : Array Nat := dispOrder grp (TblOps.colNames tbl)

namespace NavState

variable {t : Type} [TblOps t]

-- | Column names from table
@[inline] def colNames (nav : NavState t) : Array String := TblOps.colNames nav.tbl

-- | Current column index in data order
@[inline] def curColIdx (nav : NavState t) : Nat := colIdxAt nav.grp nav.colNames nav.col.cur

-- | Current column name
@[inline] def curColName (nav : NavState t) : String := nav.colNames.getD nav.curColIdx ""

-- | Current column type
@[inline] def curColType (nav : NavState t) : ColType := TblOps.colType nav.tbl nav.curColIdx

-- | Column names in display order (grouped first, then rest)
def dispColNames (nav : NavState t) : Array String :=
  nav.grp ++ (nav.colNames |>.filter (!nav.grp.contains ·))

-- | Selected column indices
def selColIdxs (nav : NavState t) : Array Nat := nav.col.sels |>.filterMap nav.colNames.idxOf?

-- | Hidden column indices (for C render)
def hiddenIdxs (nav : NavState t) : Array Nat := nav.hidden |>.filterMap nav.colNames.idxOf?

-- | Constructor: empty table → none
def newAt (tbl : t) (col : Nat := 0) (grp : Array String := #[]) (row : Nat := 0)
    : Option (NavState t) :=
  let names := TblOps.colNames tbl
  let nCols := names.size
  let nRows := TblOps.nRows tbl
  if nCols == 0 || nRows == 0 then none
  else some {
    tbl, nRows, nCols,
    row := { cur := min row (nRows - 1) },
    col := { cur := min col (nCols - 1) },
    grp, dispIdxs := dispOrder grp names }

-- | Field lenses for NavState — auto-generated. Used to express nested `row.cur`/`col.cur`
-- updates as single-line compositions rather than nested `{ nav with X := { nav.X with Y := ... } }`.
-- (tbl, nRows, nCols skipped — they're construction-time invariants, not updated during navigation.)
gen_lenses (NavState t) where row, col, grp, hidden, dispIdxs

-- | Composite lenses: cursor through row/col axis
def rowCurL  : Lens' (NavState t) Nat            := rowL ∘ₗ NavAxis.curL
def colCurL  : Lens' (NavState t) Nat            := colL ∘ₗ NavAxis.curL
def rowSelsL : Lens' (NavState t) (Array Nat)    := rowL ∘ₗ NavAxis.selsL
def colSelsL : Lens' (NavState t) (Array String) := colL ∘ₗ NavAxis.selsL

-- Execute by command, no (obj,verb) chars
def exec (h : Cmd) (nav : NavState t) (rowPg : Nat) : Option (NavState t) :=
  let r d := rowCurL.modify (clampShift · d nav.nRows) nav |> some
  let c d := colCurL.modify (clampShift · d nav.nCols) nav |> some
  match h with
  | .rowInc  => r 1              | .rowDec  => r (-1)
  | .colInc  => c 1              | .colDec  => c (-1)
  | .rowPgdn => r rowPg          | .rowPgup => r (-rowPg)
  | .rowBot  => r (nav.nRows - 1 - nav.row.cur) | .rowTop => r (-nav.row.cur)
  | .colFirst => c (-nav.col.cur) | .colLast => c (nav.nCols - 1 - nav.col.cur)
  | .rowSel  => rowSelsL.modify (·.toggle nav.row.cur) nav |> some
  | .colGrp  =>
    let newGrp := nav.grp.toggle nav.curColName
    some { nav with grp := newGrp, dispIdxs := dispOrder newGrp nav.colNames }
  | .colHide => hiddenL.modify (·.toggle nav.curColName) nav |> some
  | .colShiftL | .colShiftR =>
    let name := nav.curColName
    match nav.grp.idxOf? name with
    | some i =>
      let fwd := h == .colShiftR
      if fwd && i + 1 ≥ nav.grp.size then none
      else if !fwd && i == 0 then none
      else
        let j := if fwd then i + 1 else i - 1
        let gi := nav.grp.getD i ""
        let gj := nav.grp.getD j ""
        let newGrp := nav.grp.set! i gj |>.set! j gi
        let d := if fwd then (1 : Int) else -1
        colCurL.modify (clampShift · d nav.nCols)
          { nav with grp := newGrp, dispIdxs := dispOrder newGrp nav.colNames } |> some
    | none => none
  | _ => none

end NavState

end Tc
