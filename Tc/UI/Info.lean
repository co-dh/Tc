/-
  Info overlay: key hints display
-/
import Tc.Effect
import Tc.Term

namespace Tc.UI.Info

-- | Info state
structure State where
  vis : Bool := false

namespace State

-- | Pure update: returns (new state, effect)
def update (s : State) (cmd : Cmd) : Option (State × Effect) :=
  match cmd with
  | .info .inc => some ({ s with vis := true }, .none)
  | .info .dec => some ({ s with vis := false }, .none)
  | .info .ent => some ({ s with vis := !s.vis }, .none)
  | _ => none

instance : Update State where update := update

-- | IO wrapper (for backward compat)
def exec (s : State) (cmd : Cmd) : IO (Option State) := pure <|
  match update s cmd with
  | some (s', _) => some s'
  | none => none

instance : Exec State where exec := exec

end State

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
