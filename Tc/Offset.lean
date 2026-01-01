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

end Tc
