/-
  Offset adjustment logic for cursor visibility
-/
namespace Tc

-- Clamp value to [lo, hi)
def clamp (val lo hi : Nat) : Nat :=
  if hi ≤ lo then lo else max lo (min val (hi - 1))

-- Adjust row offset to keep cursor visible: off ≤ cur < off + page
def adjOff (cur off page : Nat) : Nat :=
  clamp off (cur + 1 - page) (cur + 1)

end Tc
