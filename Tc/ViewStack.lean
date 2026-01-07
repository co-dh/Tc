/-
  ViewStack: stack command execution
-/
import Tc.View

namespace Tc.ViewStack

-- | Pure update: returns (new stack, effect)
-- q on empty stack → quit
def update (s : ViewStack) (cmd : Cmd) : Option (ViewStack × Effect) :=
  match cmd with
  | .stk .inc | .stk .dup => some (s.dup, .none)
  | .stk .dec => match s.pop with
    | some s' => some (s', .none)
    | none => some (s, .quit)  -- can't pop → quit
  | .stk .ent => some (s.swap, .none)
  | _ => none

instance : Update ViewStack where update := update

-- | IO wrapper (for backward compat)
def exec (s : ViewStack) (cmd : Cmd) : IO (Option ViewStack) := pure <|
  match update s cmd with
  | some (s', _) => some s'
  | none => none

instance : Exec ViewStack where exec := exec

end Tc.ViewStack
