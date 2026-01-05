/-
  ViewStack operations: exec dispatches commands
  Core ViewStack type is in View.lean
-/
import Tc.Filter
import Tc.Meta
import Tc.Freq

namespace Tc.ViewStack

-- | Execute stack command
def execStk (s : ViewStack) (v : Verb) : Option ViewStack :=
  match v with
  | .inc | .dup => some s.dup
  | .dec => s.pop
  | .ent => some s.swap
  | _ => some s

-- | Execute view-specific command (chains vkind handlers)
def execView (s : ViewStack) (v : Verb) : IO (Option ViewStack) := do
  if let some s' := Meta.viewExec s v then return some s'
  if let some s' ← Freq.viewExec s v then return some s'
  pure (some s)

-- | Execute Cmd, dispatches to object exec functions
def exec (s : ViewStack) (cmd : Cmd) (rowPg colPg : Nat) : IO (Option ViewStack) := do
  match cmd with
  | .stk v => pure (execStk s v)
  | .view v => execView s v
  | .metaV v => Meta.exec s v
  | .freq v => Freq.exec s v
  | _ => match ← Filter.exec s cmd with
    | some s' => pure (some s')
    | none => match ← s.cur.exec cmd rowPg colPg with
      | some v' => pure (some (s.setCur v'))
      | none => pure none

end Tc.ViewStack
