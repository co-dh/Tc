/-
  ViewStack: stack command execution
-/
import Tc.View

namespace Tc.ViewStack

variable {T : Type} [TblOps T]

-- | Pure update: returns (new stack, effect)
-- q on empty stack → quit
def update (s : ViewStack T) (cmd : Cmd) : Option (ViewStack T × Effect) :=
  match cmd with
  | .stk .inc | .stk .dup => some (s.dup, .none)
  | .stk .dec => match s.pop with
    | some s' => some (s', .none)
    | none => some (s, .quit)  -- can't pop → quit
  | .stk .ent => some (s.swap, .none)
  | _ => none

instance : Update (ViewStack T) where update := update

-- | IO wrapper (for backward compat)
def exec (s : ViewStack T) (cmd : Cmd) : IO (Option (ViewStack T)) := pure <|
  match update s cmd with
  | some (s', _) => some s'
  | none => none

instance : Exec (ViewStack T) where exec := exec

end Tc.ViewStack
