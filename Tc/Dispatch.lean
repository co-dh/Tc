/-
  Dispatch: route commands to appropriate handlers
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
    | .col .search | .row .search | .col .filter | .row .filter

-- | Update stk, reset vs if needed
def withStk (a : AppState) (cmd : Cmd) (s' : ViewStack) : AppState :=
  { a with stk := s', vs := if resetsVS cmd then .default else a.vs }

-- | Execute Cmd on AppState
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
