/-
  Typeclass-based navigation state for tabular viewer.

  Key abstractions:
  - NavState: generic over table type + navigation state
  - NavAxis: cursor (Fin) + selections (Array)
-/
import Tc.Types
import Tc.Cmd

namespace Tc
-- Clamp value to [lo, hi)
def clamp (val lo hi : Nat) : Nat := if hi ≤ lo then lo else max lo (min val (hi - 1))
-- Adjust offset to keep cursor visible: off ≤ cur < off + page
def adjOff (cur off page : Nat) : Nat := clamp off (cur + 1 - page) (cur + 1)
end Tc

-- Clamp Fin by delta, staying in [0, n)
namespace Fin
def clamp (f : Fin n) (d : Int) : Fin n :=
  let v := ((f.val : Int) + d).toNat
  let v' := min v (n - 1)
  ⟨v', Nat.lt_of_le_of_lt (Nat.min_le_right _ _) (Nat.sub_lt f.pos Nat.one_pos)⟩
end Fin

namespace Tc

/-! ## Structures -/

-- NavAxis: cursor + selection for one axis (row or col)
structure NavAxis (n : Nat) (elem : Type) [BEq elem] where
  cur  : Fin n              -- cursor position
  sels : Array elem := #[]  -- selected elements

-- Default NavAxis for n > 0
def NavAxis.default [BEq elem] (h : n > 0) : NavAxis n elem := ⟨⟨0, h⟩, {}⟩

-- Type aliases: Row uses Nat (index), Col uses String (name, stable across deletion)
abbrev RowNav (m : Nat) := NavAxis m Nat
abbrev ColNav (n : Nat) := NavAxis n String

-- Find index of element in array (O(n) linear scan)
def Array.idxOf? [BEq α] (a : Array α) (x : α) : Option Nat :=
  a.findIdx? (· == x)

-- Compute display order: group names first (in grp array order), then rest
-- Group order matters for join key ordering (Shift+Arrow reorders grp)
-- Uses filter partition (preserves size proof) + qsort by grp position
def dispOrder (group : Array String) (names : Array String) : Array Nat :=
  let n := names.size
  let isGrp := fun i => group.contains (names.getD i "")
  let grpIdxs := (Array.range n).filter isGrp
  -- Sort group indices by position in grp array (respects grp add/reorder order)
  let grpSorted := grpIdxs.qsort fun a b =>
    (group.idxOf? (names.getD a "")).getD 0 < (group.idxOf? (names.getD b "")).getD 0
  grpSorted ++ (Array.range n).filter (!isGrp ·)

-- Helper: list filter partition
private theorem list_filter_partition (p : α → Bool) (l : List α) :
    (l.filter p).length + (l.filter (!p ·)).length = l.length := by
  induction l with
  | nil => simp
  | cons h t ih => simp only [List.filter_cons]; split <;> simp_all <;> omega

-- qsort preserves size (Vector round-trip)
private theorem qsort_size (a : Array α) (f : α → α → Bool) : (a.qsort f).size = a.size := by
  simp [Array.qsort]; split <;> simp [Vector.size_toArray]

-- dispOrder preserves size (filter partition of range n, qsort preserves size)
theorem dispOrder_size (group : Array String) (names : Array String) :
    (dispOrder group names).size = names.size := by
  simp only [dispOrder]
  rw [Array.size_append, qsort_size]
  let isGrp := fun i => group.contains (names.getD i "")
  have s1 : (Array.filter isGrp (Array.range names.size)).size =
      (List.filter isGrp (List.range names.size)).length := by
    rw [Array.size_eq_length_toList, Array.toList_filter, Array.toList_range]
  have s2 : (Array.filter (!isGrp ·) (Array.range names.size)).size =
      (List.filter (!isGrp ·) (List.range names.size)).length := by
    rw [Array.size_eq_length_toList, Array.toList_filter, Array.toList_range]
  rw [s1, s2]
  exact list_filter_partition isGrp (List.range names.size) |>.symm ▸ by simp [List.length_range]

-- When a column is grouped, its index appears first in dispOrder
theorem dispOrder_grp_first :
    (dispOrder #["c1"] #["c0", "c1", "c2"]).getD 0 999 = 1 := by native_decide

-- Get column index at display position
def colIdxAt (group : Array String) (names : Array String) (i : Nat) : Nat :=
  (dispOrder group names).getD i 0

-- NavState: generic over table type + navigation state
-- nRows/nCols are type params (not phantom) because Fin needs compile-time bounds.
-- TblOps.nRows tbl is runtime (depends on tbl field), can't use directly in Fin.
-- So we lift values to type level, then prove they match table via hRows/hCols.
structure NavState (nRows nCols : Nat) (t : Type) [TblOps t] where
  tbl      : t                                              -- underlying table
  hRows    : TblOps.nRows tbl = nRows                    -- row count matches
  hCols    : (TblOps.colNames tbl).size = nCols          -- col count matches
  row      : RowNav nRows
  col      : ColNav nCols
  grp      : Array String := #[]                            -- grouped column names (stable)
  hidden   : Array String := #[]                            -- hidden column names (width=1)
  dispIdxs : Array Nat := dispOrder grp (TblOps.colNames tbl)  -- cached display order

namespace NavState

variable {t : Type} [TblOps t]

-- | Column names from table
def colNames (nav : NavState nRows nCols t) : Array String := TblOps.colNames nav.tbl

-- | Current column index in data order
def curColIdx (nav : NavState nRows nCols t) : Nat := colIdxAt nav.grp nav.colNames nav.col.cur.val

-- | Current column name
def curColName (nav : NavState nRows nCols t) : String := nav.colNames.getD nav.curColIdx ""

-- | Selected column indices
def selColIdxs (nav : NavState nRows nCols t) : Array Nat := nav.col.sels.filterMap nav.colNames.idxOf?

-- | Hidden column indices (for C render)
def hiddenIdxs (nav : NavState nRows nCols t) : Array Nat := nav.hidden.filterMap nav.colNames.idxOf?

-- Constructor for external use
def new (tbl : t) (hRows : TblOps.nRows tbl = nRows) (hCols : (TblOps.colNames tbl).size = nCols)
    (hr : nRows > 0) (hc : nCols > 0) : NavState nRows nCols t :=
  ⟨tbl, hRows, hCols, NavAxis.default hr, NavAxis.default hc, #[], #[], dispOrder #[] (TblOps.colNames tbl)⟩

-- Constructor with initial row/col cursor and group (clamped to valid range)
def newAt (tbl : t) (hRows : TblOps.nRows tbl = nRows) (hCols : (TblOps.colNames tbl).size = nCols)
    (hr : nRows > 0) (hc : nCols > 0) (col : Nat) (grp : Array String := #[]) (row : Nat := 0)
    : NavState nRows nCols t :=
  let c := min col (nCols - 1)
  let r := min row (nRows - 1)
  have hltc : c < nCols := Nat.lt_of_le_of_lt (Nat.min_le_right ..) (Nat.sub_lt hc Nat.one_pos)
  have hltr : r < nRows := Nat.lt_of_le_of_lt (Nat.min_le_right ..) (Nat.sub_lt hr Nat.one_pos)
  ⟨tbl, hRows, hCols, ⟨⟨r, hltr⟩, #[]⟩, ⟨⟨c, hltc⟩, #[]⟩, grp, #[], dispOrder grp (TblOps.colNames tbl)⟩

-- Execute Cmd, returns Option NavState (always some for nav commands)
def exec (cmd : Cmd) (nav : NavState nRows nCols t) (rowPg colPg : Nat) : Option (NavState nRows nCols t) :=
  let r d := some { nav with row := { nav.row with cur := nav.row.cur.clamp d } }
  let c d := some { nav with col := { nav.col with cur := nav.col.cur.clamp d } }
  match cmd with
  | .row .inc   => r 1            | .row .dec   => r (-1)
  | .col .inc   => c 1            | .col .dec   => c (-1)
  | .row .rbr   => r rowPg        | .row .lbr   => r (-rowPg)
  | .row .rbc   => r (nRows - 1 - nav.row.cur.val) | .row .lbc => r (-nav.row.cur.val)
  | .col .lbc   => c (-nav.col.cur.val) | .col .rbc => c (nCols - 1 - nav.col.cur.val)
  | .row .ent => some { nav with row := { nav.row with sels := nav.row.sels.toggle nav.row.cur.val } }
  | .col .ent =>
    let newGrp := nav.grp.toggle nav.curColName
    some { nav with grp := newGrp, dispIdxs := dispOrder newGrp nav.colNames }
  | .col .filter =>  -- c\: hide column
    some { nav with hidden := nav.hidden.toggle nav.curColName }
  -- c-/c+: swap key column with neighbor in grp array, cursor follows
  | .col .del | .col .dup =>
    let name := nav.curColName
    match nav.grp.idxOf? name with
    | some i =>
      let fwd := cmd == .col .dup
      if fwd && i + 1 ≥ nav.grp.size then none
      else if !fwd && i == 0 then none
      else
        let j := if fwd then i + 1 else i - 1
        let gi := nav.grp.getD i ""
        let gj := nav.grp.getD j ""
        let newGrp := nav.grp.set! i gj |>.set! j gi
        let d := if fwd then (1 : Int) else -1
        some { nav with grp := newGrp, dispIdxs := dispOrder newGrp nav.colNames,
                        col := { nav.col with cur := nav.col.cur.clamp d } }
    | none => none
  | _ => none

-- | Pure update: wrap exec to return Effect
def update (cmd : Cmd) (nav : NavState nRows nCols t) (rowPg colPg : Nat)
    : Option (NavState nRows nCols t × Effect) :=
  (exec cmd nav rowPg colPg).map (·, .none)

end NavState

end Tc
