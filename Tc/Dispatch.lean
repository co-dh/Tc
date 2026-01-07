/-
  Dispatch: route commands to appropriate handlers
  Pure update returns Effect; exec runs IO for backward compat.
-/
import Tc.Filter
import Tc.Folder
import Tc.Meta
import Tc.Freq
import Tc.Theme
import Tc.UI.Info
import Tc.ViewStack

namespace Tc

-- | App state: view stack + render state + theme + info
structure AppState where
  stk   : ViewStack
  vs    : ViewState
  theme : Theme.State
  info  : UI.Info.State

namespace AppState

-- | Commands that reset ViewState
def resetsVS (cmd : Cmd) : Bool :=
  cmd matches .stk .dec | .colSel .del | .colSel _ | .metaV _ | .freq _ | .fld _
    | .col .ent | .rowSel .inc | .rowSel .dec

-- | Update stk, reset vs if needed
def withStk (a : AppState) (cmd : Cmd) (s' : ViewStack) : AppState :=
  { a with stk := s', vs := if resetsVS cmd then .default else a.vs }

-- | Lift stack update to AppState (Kleisli helper for <|> chain)
private def liftStk (a : AppState) (cmd : Cmd) (r : Option (ViewStack × Effect)) : Option (AppState × Effect) :=
  r.map fun (s', eff) => (withStk a cmd s', eff)

-- | Pure update: chain handlers with <|> (Alternative), return (new state, effect)
-- Uses Kleisli-style composition: try each handler, first Some wins
-- Priority: theme > info > stack > folder > meta > freq > filter > view
def update (a : AppState) (cmd : Cmd) : Option (AppState × Effect) :=
  (a.theme.update cmd |>.map fun (t', eff) => ({ a with theme := t' }, eff))
  <|> (a.info.update cmd |>.map fun (i', eff) => ({ a with info := i' }, eff))
  <|> (liftStk a cmd (ViewStack.update a.stk cmd))
  <|> (liftStk a cmd (Folder.update a.stk cmd))
  <|> (liftStk a cmd (Meta.update a.stk cmd))
  <|> (liftStk a cmd (Freq.update a.stk cmd))
  <|> (liftStk a cmd (Filter.update a.stk cmd))
  <|> (View.update a.stk.cur cmd 20 |>.map fun (v', eff) => (withStk a cmd (a.stk.setCur v'), eff))

instance : Update AppState where update := update

-- | Execute Cmd on AppState (IO version for backward compat)
def exec (a : AppState) (cmd : Cmd) : IO (Option AppState) := do
  if let some t' ← a.theme.exec cmd then return some { a with theme := t' }
  if let some i' ← a.info.exec cmd then return some { a with info := i' }
  if let some s' ← a.stk.exec cmd then return some (withStk a cmd s')
  if let some s' ← Folder.exec a.stk cmd then return some (withStk a cmd s')
  if let some s' ← Meta.exec a.stk cmd then return some (withStk a cmd s')
  if let some s' ← Freq.exec a.stk cmd then return some (withStk a cmd s')
  if let some s' ← Filter.exec a.stk cmd then return some (withStk a cmd s')
  match ← a.stk.cur.exec cmd with
  | some v' => pure (some (withStk a cmd (a.stk.setCur v')))
  | none => pure none

instance : Exec AppState where exec := exec

end AppState
end Tc
