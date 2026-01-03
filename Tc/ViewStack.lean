/-
  ViewStack: non-empty view stack
-/
import Tc.View

namespace Tc

-- | Non-empty view stack
structure ViewStack where
  cur : View                 -- current view (always exists)
  parents : Array View := #[]

namespace ViewStack

-- | All views (current first)
def views (s : ViewStack) : Array View := #[s.cur] ++ s.parents

-- | Stack depth
def depth (s : ViewStack) : Nat := 1 + s.parents.size

-- | Has parent?
def hasParent (s : ViewStack) : Bool := s.parents.size > 0

-- | Update current view
def setCur (s : ViewStack) (v : View) : ViewStack := { s with cur := v }

-- | Push new view (current becomes parent)
def push (s : ViewStack) (v : View) : ViewStack :=
  { s with cur := v, parents := #[s.cur] ++ s.parents }

-- | Pop view (returns to parent, or none if no parent)
def pop (s : ViewStack) : Option ViewStack :=
  if h : s.parents.size > 0 then
    some { cur := s.parents[0], parents := s.parents.extract 1 s.parents.size }
  else none

-- | Swap top two views
def swap (s : ViewStack) : ViewStack :=
  if h : s.parents.size > 0 then
    { cur := s.parents[0], parents := #[s.cur] ++ s.parents.extract 1 s.parents.size }
  else s

-- | Duplicate current view
def dup (s : ViewStack) : ViewStack :=
  { s with parents := #[s.cur] ++ s.parents }

-- | Tab names for display (current first)
def tabNames (s : ViewStack) : Array String := s.views.map (·.tabName)

-- | Execute stk command, returns Option ViewStack (none = quit)
def exec (s : ViewStack) (v : Verb) : Option ViewStack :=
  match v with
  | .inc    => some s.dup   -- push (dup for now)
  | .dec    => s.pop        -- pop, none = quit
  | .toggle => some s.swap  -- swap
  | .dup    => some s.dup   -- dup
  | _       => some s       -- ignore other verbs

end ViewStack
end Tc
