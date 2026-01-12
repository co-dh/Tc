/-
  Dispatch: route commands to appropriate handlers
  Pure update returns Effect; exec runs IO for backward compat.
-/
import Tc.Filter
import Tc.Folder
import Tc.Meta
import Tc.Freq
import Tc.UI.Info
import Tc.ViewStack

namespace Tc

-- | AppState for full tc (Table = MemTable | AdbcTable | KdbTable)
abbrev AppState := GAppState Table

namespace AppState

-- | Lift stack update to AppState (Kleisli helper for <|> chain)
private def liftStk (a : AppState) (cmd : Cmd) (r : Option (ViewStack × Effect)) : Option (AppState × Effect) :=
  r.map fun (s', eff) => (a.withStk cmd s', eff)

-- | Pure update: chain handlers with <|> (Alternative), return (new state, effect)
-- Priority: theme > info > stack > folder > meta > freq > filter > view
def update (a : AppState) (cmd : Cmd) : Option (AppState × Effect) :=
  (a.theme.update cmd |>.map fun (t', eff) => ({ a with theme := t' }, eff))
  <|> (UI.Info.State.update ⟨a.info.vis⟩ cmd |>.map fun (i', eff) => ({ a with info := ⟨i'.vis⟩ }, eff))
  <|> (liftStk a cmd (ViewStack.update a.stk cmd))
  <|> (liftStk a cmd (Folder.update a.stk cmd))
  <|> (liftStk a cmd (Meta.update a.stk cmd))
  <|> (liftStk a cmd (Freq.update a.stk cmd))
  <|> (liftStk a cmd (Filter.update a.stk cmd))
  <|> (View.update a.stk.cur cmd 20 |>.map fun (v', eff) => (a.withStk cmd (a.stk.setCur v'), eff))

instance : Update AppState where update := update

-- | Execute Cmd on AppState (IO version for backward compat)
def exec (a : AppState) (cmd : Cmd) : IO (Option AppState) := do
  if let some t' ← a.theme.exec cmd then return some { a with theme := t' }
  if let some i' ← UI.Info.State.exec ⟨a.info.vis⟩ cmd then return some { a with info := ⟨i'.vis⟩ }
  if let some s' ← Tc.ViewStack.exec a.stk cmd then return some (a.withStk cmd s')
  if let some s' ← Folder.exec a.stk cmd then return some (a.withStk cmd s')
  if let some s' ← Meta.exec a.stk cmd then return some (a.withStk cmd s')
  if let some s' ← Freq.exec a.stk cmd then return some (a.withStk cmd s')
  if let some s' ← Filter.exec a.stk cmd then return some (a.withStk cmd s')
  match ← GView.exec a.stk.cur cmd with
  | some v' => pure (some (a.withStk cmd (a.stk.setCur v')))
  | none => pure none

instance : Exec AppState where exec := exec

end AppState
end Tc
