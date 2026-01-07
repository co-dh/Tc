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

-- | Pure update: chain handlers, return (new state, effect)
-- Priority: theme > info > stack > folder > meta > freq > filter > view
def update (a : AppState) (cmd : Cmd) : Option (AppState × Effect) :=
  -- theme
  if let some (t', eff) := a.theme.update cmd
  then some ({ a with theme := t' }, eff)
  -- info
  else if let some (i', eff) := a.info.update cmd
  then some ({ a with info := i' }, eff)
  -- stack ops
  else if let some (s', eff) := ViewStack.update a.stk cmd
  then some (withStk a cmd s', eff)
  -- folder
  else if let some (s', eff) := Folder.update a.stk cmd
  then some (withStk a cmd s', eff)
  -- meta
  else if let some (s', eff) := Meta.update a.stk cmd
  then some (withStk a cmd s', eff)
  -- freq
  else if let some (s', eff) := Freq.update a.stk cmd
  then some (withStk a cmd s', eff)
  -- filter
  else if let some (s', eff) := Filter.update a.stk cmd
  then some (withStk a cmd s', eff)
  -- view (nav, prec, width, sort, delete)
  else
    let rowPg := 20  -- default, will use actual height in runner
    match View.update a.stk.cur cmd rowPg with
    | some (v', eff) => some (withStk a cmd (a.stk.setCur v'), eff)
    | none => none

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
