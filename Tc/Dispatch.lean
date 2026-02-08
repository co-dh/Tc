/-
  Dispatch: route commands to appropriate handlers
  Pure update returns Effect; exec runs IO for backward compat.
-/
import Tc.Filter
import Tc.Folder
import Tc.Meta
import Tc.Freq
import Tc.Theme
import Tc.Plot
import Tc.UI.Info

namespace Tc

-- | App state: view stack + render state + theme + info
structure AppState where
  stk   : ViewStack Table
  vs    : ViewState
  theme : Theme.State
  info  : UI.Info.State

namespace AppState

-- | Commands that reset ViewState
def resetsVS (cmd : Cmd) : Bool :=
  cmd matches .stk .dec | .colSel .del | .colSel _ | .metaV _ | .freq _ | .fld _
    | .col .ent | .rowSel .inc | .rowSel .dec

-- | Update stk, reset vs if needed
def withStk (a : AppState) (cmd : Cmd) (s' : ViewStack Table) : AppState :=
  { a with stk := s', vs := if resetsVS cmd then .default else a.vs }

-- | Lift stack update to AppState (Kleisli helper for <|> chain)
private def liftStk (a : AppState) (cmd : Cmd) (r : Option (ViewStack Table × Effect)) : Option (AppState × Effect) :=
  r.map fun (s', eff) => (withStk a cmd s', eff)

-- | Route by Cmd discriminant, fallback to view for nav/selection
def update (a : AppState) (cmd : Cmd) : Option (AppState × Effect) :=
  let viewUp := View.update a.stk.cur cmd 20 |>.map fun (v', e) => (withStk a cmd (a.stk.setCur v'), e)
  match cmd with
  | .thm _    => a.theme.update cmd |>.map fun (t', e) => ({ a with theme := t' }, e)
  | .info _   => a.info.update cmd |>.map fun (i', e) => ({ a with info := i' }, e)
  | .stk _    => liftStk a cmd (ViewStack.update a.stk cmd)
  | .fld _    => liftStk a cmd (Folder.update a.stk cmd) <|> viewUp
  | .metaV _  => liftStk a cmd (Meta.update a.stk cmd) <|> viewUp
  | .freq _   => liftStk a cmd (Freq.update a.stk cmd) <|> viewUp
  | .plot _   => liftStk a cmd (Plot.update a.stk cmd)
  | .col .ent | .rowSel _ => liftStk a cmd (Filter.update a.stk cmd) <|> viewUp
  | .grp .inc | .grp .dec => liftStk a cmd (Filter.update a.stk cmd)
  | .colSel .del => liftStk a cmd (Folder.update a.stk cmd) <|> viewUp
  | _ => viewUp

instance : Update (AppState) where update := update

end AppState
end Tc
