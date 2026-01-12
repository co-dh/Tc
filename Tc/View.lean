/-
  View: re-exports View/Generic for generic view operations.
  For Table-specific View/ViewStack, import Tc.Table instead.
-/
import Tc.View.Generic

-- Re-export everything from Generic
namespace Tc
export GView (new tabName doRender fromTbl update exec)
export GViewStack (cur views hasParent setCur push pop swap dup tabNames moveRowTo moveColTo)
export GAppState (resetsVS withStk)
end Tc
