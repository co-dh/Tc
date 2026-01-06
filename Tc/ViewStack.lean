/-
  ViewStack: stack command execution
-/
import Tc.View

namespace Tc.ViewStack

-- | Execute stack command
def exec (s : ViewStack) (cmd : Cmd) : IO (Option ViewStack) := pure <|
  match cmd with
  | .stk .inc | .stk .dup => some s.dup
  | .stk .dec => s.pop
  | .stk .ent => some s.swap
  | _ => none

instance : Exec ViewStack where exec := exec

end Tc.ViewStack
