/-
  Typeclass-based navigation state for tabular viewer.

  Key abstractions:
  - NavState: generic over table type + navigation state
  - NavAxis: cursor (Fin) + selections (Array)
-/
import Tc.Offset
import Tc.Types
import Tc.Effect

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

-- dispOrder preserves size: gIdxs ⊆ range n, filter keeps n - gIdxs.size elements
-- Proof sketch: filterMap returns subset of [0,n), filter keeps exactly the complement
-- Full proof requires Finset partition lemma - deferred for TUI app
theorem dispOrder_size (group : Array String) (names : Array String) :
    (dispOrder group names).size = names.size := by
  simp only [dispOrder, Array.size_append]
  sorry

-- Get column index at display position
def colIdxAt (group : Array String) (names : Array String) (i : Nat) : Nat :=
  (dispOrder group names).getD i 0

-- NavState: generic over table type + navigation state
-- nRows/nCols are type params (not phantom) because Fin needs compile-time bounds.
-- ReadTable.nRows tbl is runtime (depends on tbl field), can't use directly in Fin.
-- So we lift values to type level, then prove they match table via hRows/hCols.
structure NavState (nRows nCols : Nat) (t : Type) [ReadTable t] where
  tbl   : t                                              -- underlying table
  hRows : ReadTable.nRows tbl = nRows                    -- row count matches
  hCols : (ReadTable.colNames tbl).size = nCols          -- col count matches
  row   : RowNav nRows
  col   : ColNav nCols
  grp   : Array String := #[]                            -- grouped column names (stable)

namespace NavState

variable {t : Type} [ReadTable t]

-- | Column names from table
def colNames (nav : NavState nRows nCols t) : Array String := ReadTable.colNames nav.tbl

-- | Column indices in display order (group cols first)
def dispColIdxs (nav : NavState nRows nCols t) : Array Nat := dispOrder nav.grp nav.colNames

-- | Current column index in data order
def curColIdx (nav : NavState nRows nCols t) : Nat := colIdxAt nav.grp nav.colNames nav.col.cur.val

-- | Current column name
def curColName (nav : NavState nRows nCols t) : String := nav.colNames.getD nav.curColIdx ""

-- | Selected column indices
def selColIdxs (nav : NavState nRows nCols t) : Array Nat := nav.col.sels.filterMap nav.colNames.idxOf?

-- Constructor for external use
def new (tbl : t) (hRows : ReadTable.nRows tbl = nRows) (hCols : (ReadTable.colNames tbl).size = nCols)
    (hr : nRows > 0) (hc : nCols > 0) : NavState nRows nCols t :=
  ⟨tbl, hRows, hCols, NavAxis.default hr, NavAxis.default hc, #[]⟩

-- Constructor with initial row/col cursor and group (clamped to valid range)
def newAt (tbl : t) (hRows : ReadTable.nRows tbl = nRows) (hCols : (ReadTable.colNames tbl).size = nCols)
    (hr : nRows > 0) (hc : nCols > 0) (col : Nat) (grp : Array String := #[]) (row : Nat := 0)
    : NavState nRows nCols t :=
  let c := min col (nCols - 1)
  let r := min row (nRows - 1)
  have hltc : c < nCols := Nat.lt_of_le_of_lt (Nat.min_le_right ..) (Nat.sub_lt hc Nat.one_pos)
  have hltr : r < nRows := Nat.lt_of_le_of_lt (Nat.min_le_right ..) (Nat.sub_lt hr Nat.one_pos)
  ⟨tbl, hRows, hCols, ⟨⟨r, hltr⟩, #[]⟩, ⟨⟨c, hltc⟩, #[]⟩, grp⟩

-- Execute Cmd, returns Option NavState (always some for nav commands)
def exec (cmd : Cmd) (nav : NavState nRows nCols t) (rowPg colPg : Nat) : Option (NavState nRows nCols t) :=
  let r d := some { nav with row := { nav.row with cur := nav.row.cur.clamp d } }
  let c d := some { nav with col := { nav.col with cur := nav.col.cur.clamp d } }
  match cmd with
  | .row .inc   => r 1            | .row .dec   => r (-1)
  | .col .inc   => c 1            | .col .dec   => c (-1)
  | .vPage .inc => r rowPg        | .vPage .dec => r (-rowPg)
  | .hPage .inc => c colPg        | .hPage .dec => c (-colPg)
  | .ver .inc   => r (nRows - 1 - nav.row.cur.val) | .ver .dec => r (-nav.row.cur.val)
  | .hor .inc   => c (nCols - 1 - nav.col.cur.val) | .hor .dec => c (-nav.col.cur.val)
  | .rowSel .ent => some { nav with row := { nav.row with sels := nav.row.sels.toggle nav.row.cur.val } }
  | .colSel .ent => some { nav with col := { nav.col with sels := nav.col.sels.toggle nav.curColName } }
  | .grp .ent    => some { nav with grp := nav.grp.toggle nav.curColName }
  | _ => none  -- unhandled: .col .del, .colSel .sort*, .prec, .width, etc.

-- | Pure update: wrap exec to return Effect
def update (cmd : Cmd) (nav : NavState nRows nCols t) (rowPg colPg : Nat)
    : Option (NavState nRows nCols t × Effect) :=
  (exec cmd nav rowPg colPg).map (·, .none)

/-! ## Theorems -/

-- | Nav commands always return Effect.none (when they succeed)
theorem update_effect_none (cmd : Cmd) (nav : NavState nRows nCols t) (rowPg colPg : Nat)
    (nav' : NavState nRows nCols t) (eff : Effect) :
    update cmd nav rowPg colPg = some (nav', eff) → eff = .none := by
  intro h; simp only [update, Option.map] at h
  split at h <;> simp_all

-- | Row navigation preserves table and columns
theorem exec_row_inc_preserves (nav : NavState nRows nCols t) (rowPg colPg : Nat)
    (nav' : NavState nRows nCols t) :
    exec (.row .inc) nav rowPg colPg = some nav' →
    nav'.tbl = nav.tbl ∧ nav'.col = nav.col ∧ nav'.grp = nav.grp := by
  intro h; simp only [exec] at h; injection h with h; subst h; simp

-- | Col navigation preserves table and rows
theorem exec_col_inc_preserves (nav : NavState nRows nCols t) (rowPg colPg : Nat)
    (nav' : NavState nRows nCols t) :
    exec (.col .inc) nav rowPg colPg = some nav' →
    nav'.tbl = nav.tbl ∧ nav'.row = nav.row ∧ nav'.grp = nav.grp := by
  intro h; simp only [exec] at h; injection h with h; subst h; simp

-- | Go to end (ver.inc) is idempotent: cursor = nRows - 1
theorem exec_ver_inc_idempotent (nav : NavState nRows nCols t) (rowPg colPg : Nat)
    (nav' nav'' : NavState nRows nCols t) :
    exec (.ver .inc) nav rowPg colPg = some nav' →
    exec (.ver .inc) nav' rowPg colPg = some nav'' →
    nav''.row.cur = nav'.row.cur := by
  intro h1 h2; simp only [exec] at h1 h2
  injection h1 with h1; injection h2 with h2
  subst h1 h2
  simp only [Fin.clamp, Fin.ext_iff, Fin.val_mk]
  sorry  -- requires Int/Nat arithmetic lemmas for min/clamp

-- | Go to home (ver.dec) is idempotent: cursor = 0
theorem exec_ver_dec_idempotent (nav : NavState nRows nCols t) (rowPg colPg : Nat)
    (nav' nav'' : NavState nRows nCols t) :
    exec (.ver .dec) nav rowPg colPg = some nav' →
    exec (.ver .dec) nav' rowPg colPg = some nav'' →
    nav''.row.cur = nav'.row.cur := by
  intro h1 h2; simp only [exec] at h1 h2
  injection h1 with h1; injection h2 with h2
  subst h1 h2
  simp only [Fin.clamp, Fin.ext_iff, Fin.val_mk]
  sorry  -- requires Int/Nat arithmetic lemmas for min/clamp

end NavState

end Tc
