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
  | .info .inc => some ({ s with vis := true }, .none)
  | .info .dec => some ({ s with vis := false }, .none)
  | .info .ent => some ({ s with vis := !s.vis }, .none)
  | _ => none

instance : Update State where update := update

end State

-- | Context-specific key hints per view (no common navigation)
def viewHints : ViewKind → Array (String × String)
  | .colMeta => #[("0", "sel null"), ("1", "sel single"), ("⏎", "set key"), ("q", "back")]
  | .freqV _ _ => #[("⏎", "filter"), ("q", "back")]
  | .fld _ _ => #[("⏎", "enter"), ("d", "trash"), (",d", "depth-"), (".d", "depth+")]
  | .tbl => #[
    ("=", "derive"), ("e", "export"), ("m", "heatmap"), ("Z", "sparkline"),
    (".", "line plot"), ("P,", "bar plot"),
    ("t/T", "sel"), ("H", "hide"), ("!", "group"), ("S-←→", "key order"),
    ("\\", "filter"), ("s", "col jump"), ("X", "transpose"),
    ("M", "meta"), ("F", "freq"), ("D", "folder"),
    ("W", "save sess"), ("L", "load sess"),
    ("I", "info"), ("SPC", "cmd mode")]

-- | Render info overlay at bottom-right
def render (screenH screenW : Nat) (vk : ViewKind) : IO Unit := do
  let hints := viewHints vk
  let nRows := hints.size
  let keyW : Nat := 5; let hintW : Nat := 10
  let boxW := keyW + 1 + hintW
  let x0 := screenW - boxW - 2
  let y0 := screenH - nRows - 3
  for i in [:nRows] do
    let (k, d) := hints.getD i ("", "")
    let kpad := "".pushn ' ' (keyW - k.length) ++ k
    let dpad := (d.take hintW).toString ++ "".pushn ' ' (hintW - min d.length hintW)
    Term.print x0.toUInt32 (y0 + i).toUInt32 Term.black Term.yellow (kpad ++ " " ++ dpad)

end Tc.UI.Info
