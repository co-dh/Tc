/-
  Info overlay (Core): key hints display
  Uses Tc.Types for ViewKind instead of Tc.View
-/
import Tc.Term
import Tc.Types

namespace Tc.Info

-- | View-specific key hints
def viewHints : ViewKind → Array (String × String)
  | .colMeta => #[("0", "sel null"), ("1", "sel single"), ("⏎", "set key"), ("q", "back")]
  | .freqV _ => #[("⏎", "filter"), ("q", "back")]
  | .fld _ _ => #[("⏎", "enter"), ("d", "trash")]
  | .tbl => #[("M", "meta"), ("F", "freq")]

-- | Common key hints
def commonHints : Array (String × String) := #[
  ("j/k", "up/down"), ("h/l", "left/right"),
  ("g/G", "top/end"), ("^D/^U", "page"),
  ("[/]", "sort"), ("/", "search"),
  ("!", "key col"), ("t/T", "sel"), ("d", "delete"),
  ("s", "col jump"), ("S", "stack"),
  ("I", "info"), ("q", "pop"), ("Q", "quit")
]

-- | Render info overlay
def render (vk : ViewKind) : IO Unit := do
  let screenW ← Term.width
  let screenH ← Term.height
  let hints := viewHints vk ++ commonHints
  let nRows := hints.size
  let keyW : Nat := 5; let hintW : Nat := 10
  let boxW := keyW + 1 + hintW
  let x0 := screenW.toNat - boxW - 2
  let y0 := screenH.toNat - nRows - 3
  for i in [:nRows] do
    let (k, d) := hints.getD i ("", "")
    let kpad := "".pushn ' ' (keyW - k.length) ++ k
    let dpad := d.take hintW ++ "".pushn ' ' (hintW - min d.length hintW)
    Term.print x0.toUInt32 (y0 + i).toUInt32 Term.black Term.yellow (kpad ++ " " ++ dpad)

end Tc.Info
