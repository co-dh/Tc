/-
  Offset adjustment logic for cursor visibility
-/
namespace Tc

-- Clamp value to [lo, hi)
def clamp (val lo hi : Nat) : Nat :=
  if hi ≤ lo then lo else max lo (min val (hi - 1))

-- Cumulative width function for n columns: cumW i = total width of columns 0..i-1
abbrev CumW (n : Nat) := Fin (n + 1) → Nat

-- Build CumW from widths array
def mkCumW (widths : Array Nat) : CumW widths.size := fun i =>
  -- cumW[i] = sum of widths[0..i-1] + i (for separators)
  widths.foldl (init := (0, 0)) (fun (idx, acc) w =>
    if idx < i.val then (idx + 1, acc + w + 1) else (idx + 1, acc)) |>.2

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

end Tc
