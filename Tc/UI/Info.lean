/-
  Info overlay: context-specific key hints per view kind.
-/
import Tc.Cmd
import Tc.Term
import Tc.View

namespace Tc.UI.Info

-- | Info state
structure State where
  vis : Bool := true

namespace State

-- | Pure update: returns (new state, effect)
def update (s : State) (cmd : Cmd) : Option (State × Effect) :=
  match cmd with
  | .info .ent => some ({ s with vis := !s.vis }, .none)
  | _ => none

instance : Update State where update := update

end State

-- | Context-specific key hints per view (no common navigation)
def viewHints : ViewKind → Array (String × String)
  | .colMeta => #[("M0", "select nulls"), ("M1", "select unique"), ("⏎", "set as key"), ("q", "back")]
  | .freqV _ _ => #[("⏎", "filter by val"), ("q", "back")]
  | .fld _ _ => #[("⏎", "open"), ("D-", "trash"), ("D<", "less depth"), ("D>", "more depth")]
  | .tbl => #[
    ("T", "toggle row sel"),
    ("!", "group by"), ("c\\", "hide column"),
    ("S-←→", "reorder cols"), ("SPC", "command menu")]

-- | Render info overlay at bottom-right
def render (screenH screenW : Nat) (vk : ViewKind) : IO Unit := do
  let hints := viewHints vk
  let nRows := hints.size
  let keyW : Nat := 5; let hintW : Nat := 14
  let boxW := keyW + 1 + hintW
  let x0 := screenW - boxW - 2
  let y0 := screenH - nRows - 3
  for i in [:nRows] do
    let (k, d) := hints.getD i ("", "")
    let kpad := "".pushn ' ' (keyW - k.length) ++ k
    let dpad := (d.take hintW).toString ++ "".pushn ' ' (hintW - min d.length hintW)
    Term.print x0.toUInt32 (y0 + i).toUInt32 Term.black Term.yellow (kpad ++ " " ++ dpad)

end Tc.UI.Info
