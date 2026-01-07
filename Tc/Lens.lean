/-
  Lens: simple optics for nested state updates
  Provides get/set/modify for cleaner composition
-/
namespace Tc

-- | Lens: accessor pair for nested state
structure Lens (s a : Type) where
  get : s → a
  set : a → s → s

namespace Lens

-- | Modify via function (get → apply → set)
@[inline] def modify (l : Lens s a) (f : a → a) (x : s) : s :=
  l.set (f (l.get x)) x

-- | Compose two lenses (outer.inner)
@[inline] def comp (l1 : Lens a b) (l2 : Lens b c) : Lens a c where
  get := l2.get ∘ l1.get
  set := fun c a => l1.modify (l2.set c) a

-- | Infix notation for lens composition
infixl:90 " ∘ₗ " => comp

-- | Identity lens
def id : Lens a a where
  get := fun x => x
  set := fun x _ => x

-- | Lens laws (for documentation, not enforced):
-- 1. get (set a s) = a            (get after set returns what was set)
-- 2. set (get s) s = s            (setting current value is no-op)
-- 3. set a (set b s) = set a s    (set twice = set once)

end Lens

end Tc
