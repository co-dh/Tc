/-!
Minimal category-theory primitives: `Cat`, `Func`, `NatTrans`, plus the
infix notation we use elsewhere (`⟶` for Hom, `≫` for composition,
`⇒` for functor, `⋙` for diagrammatic functor composition, `⇛` for
natural transformation).

Everything here is universe-monomorphic (Type 0 / Type 0) — the
"small category" setting.  Files that need to talk about the category
of types or full Kan extensions should reach for Mathlib instead.
-/

/-- A small category. -/
structure Cat where
  Obj : Type
  Hom : Obj → Obj → Type
  idH : (c : Obj) → Hom c c
  comp : {a b c : Obj} → Hom a b → Hom b c → Hom a c
  id_comp : ∀ {a b : Obj} (m : Hom a b), comp (idH a) m = m
  comp_id : ∀ {a b : Obj} (m : Hom a b), comp m (idH b) = m
  assoc : ∀ {a b c d : Obj} (p : Hom a b) (q : Hom b c) (r : Hom c d),
            comp (comp p q) r = comp p (comp q r)

/-- Infix for the morphism type: `X ⟶ Y = C.Hom X Y` (Mathlib's `\hom`). -/
@[reducible] def Cat.hom {C : Cat} (X Y : C.Obj) : Type := C.Hom X Y

infixr:10 " ⟶ " => Cat.hom

/-- Infix for category composition: `f ≫ g = C.comp f g` (diagrammatic order). -/
@[reducible] def Cat.then {C : Cat} {a b c : C.Obj}
    (f : a ⟶ b) (g : b ⟶ c) : a ⟶ c := C.comp f g

infixl:80 " ≫ " => Cat.then

/-- A functor between two categories. `o` = object map, `f` = morphism map. -/
structure Func (S T : Cat) where
  o : S.Obj → T.Obj
  f : {a b : S.Obj} → (a ⟶ b) → (o a ⟶ o b)
  f_id : ∀ (a : S.Obj), f (S.idH a) = T.idH (o a)
  f_comp : ∀ {a b c : S.Obj} (p : a ⟶ b) (q : b ⟶ c),
              f (p ≫ q) = f p ≫ f q

/-- Notation for functors: `S ⇒ T` (typed `\Rightarrow`). -/
infixr:25 " ⇒ " => Func

/-- Func composition (math order: `compose G F = G ∘ F`). -/
def Func.compose {S T U : Cat} (G : T ⇒ U) (F : S ⇒ T) : S ⇒ U where
  o x  := G.o (F.o x)
  f p  := G.f (F.f p)
  f_id a := by simp [F.f_id, G.f_id]
  f_comp p q := by simp [F.f_comp, G.f_comp]

/-- Diagrammatic functor composition: `F ⋙ G` = "F then G" = `G ∘ F`. -/
infixl:80 " ⋙ " => fun F G => Func.compose G F

/-- Natural transformation between two functors `F, G : S ⇒ T`. -/
structure NatTrans {S T : Cat} (F G : S ⇒ T) where
  app : (a : S.Obj) → (F.o a ⟶ G.o a)
  naturality : ∀ {a b : S.Obj} (m : a ⟶ b),
    F.f m ≫ app b = app a ≫ G.f m

/-- Notation for natural transformations: `F ⇛ G` (typed `\Rrightarrow`). -/
infixr:25 " ⇛ " => NatTrans
