/-
  Info overlay: key hints display
-/
import Tc.Term

namespace Tc.UI.Info

-- | Key hints for info overlay (key | description)
def keyHints : Array (String × String) := #[
  ("j/k", "up/down"), ("h/l", "left/right"),
  ("g/G", "top/end"), ("^D/^U", "page"),
  ("0/$", "first/last"), ("[/]", "sort"),
  ("/", "search"), ("\\", "filter"),
  ("F", "freq"), ("M", "meta"),
  ("d", "delete"), ("t/T", "sel"),
  ("!", "key col"), ("s", "col jump"),
  ("+/-", "adjust"), ("S", "stack"),
  ("I", "info"), ("q", "pop"), ("Q", "quit")
]

-- | Render info overlay at bottom-right
def render (screenH screenW : Nat) : IO Unit := do
  let nRows := keyHints.size
  let keyW : Nat := 5; let hintW : Nat := 10
  let boxW := keyW + 1 + hintW
  let x0 := screenW - boxW - 2
  let y0 := screenH - nRows - 3
  for i in [:nRows] do
    let (k, d) := keyHints.getD i ("", "")
    let kpad := "".pushn ' ' (keyW - k.length) ++ k
    let dpad := d.take hintW ++ "".pushn ' ' (hintW - min d.length hintW)
    Term.print x0.toUInt32 (y0 + i).toUInt32 Term.black Term.yellow (kpad ++ " " ++ dpad)

end Tc.UI.Info
