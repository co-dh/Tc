/-
Cones for Kan extensions, Milewski Part 1 direction.

  I  ──K──▶  𝟙  ──F(c)──▶  C
   ╲                       ▲
    ╲       D              │
     ╲────────────────────╱
              ⇓ ε  (cone)

Naming follows Milewski's blog:
  · I is the indexing category (here: a commutative triangle X→Y→Z, X→Z)
  · 𝟙 is the terminal category
  · C is the target category
  · K : I ⇒ 𝟙 collapses everything to the unique object
  · F(c) : 𝟙 ⇒ C picks `c ∈ C.Obj` as the apex
  · Δ c := K ⋙ F(c) is the constant diagram at `c`
  · D : I ⇒ C is any (triangle-shaped) diagram in C
  · ε : (Δ c) ⇛ D is a cone over D with apex c
-/

-- I: the indexing category (a commutative triangle).
namespace I
inductive Obj | X | Y | Z
inductive Hom : Obj → Obj → Type
  | id : (o : Obj) → Hom o o
  | f  : Hom .X .Y
  | g  : Hom .Y .Z
  | h  : Hom .X .Z   -- = g ∘ f

def Hom.comp : ∀ {a b c : Obj}, Hom a b → Hom b c → Hom a c
  | _, _, _, .id _, m => m
  | _, _, _, m,     .id _ => m
  | _, _, _, .f,    .g => .h
end I

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

/-- Infix for category composition: `f ≫ g = C.comp f g` (diagrammatic order). -/
@[reducible] def Cat.then {C : Cat} {a b c : C.Obj}
    (f : C.Hom a b) (g : C.Hom b c) : C.Hom a c := C.comp f g

infixl:80 " ≫ " => Cat.then

/-- A functor between two categories. `o` = object map, `f` = morphism map. -/
structure Func (S T : Cat) where
  o : S.Obj → T.Obj
  f : {a b : S.Obj} → S.Hom a b → T.Hom (o a) (o b)
  f_id : ∀ (a : S.Obj), f (S.idH a) = T.idH (o a)
  f_comp : ∀ {a b c : S.Obj} (p : S.Hom a b) (q : S.Hom b c),
              f (S.comp p q) = T.comp (f p) (f q)

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
  app : (a : S.Obj) → T.Hom (F.o a) (G.o a)
  naturality : ∀ {a b : S.Obj} (m : S.Hom a b),
    T.comp (F.f m) (app b) = T.comp (app a) (G.f m)

/-- Notation for natural transformations: `F ⇛ G` (typed `\Rrightarrow`). -/
infixr:25 " ⇛ " => NatTrans

-- I packaged as a Cat (so it can be source of a Func).
def Icat : Cat where
  Obj := I.Obj
  Hom := I.Hom
  idH := I.Hom.id
  comp := I.Hom.comp
  id_comp m := by cases m <;> rfl
  comp_id m := by cases m <;> rfl
  assoc α β γ := by cases α <;> cases β <;> cases γ <;> rfl

-- The terminal category, encoded as a sub-category of Type:
-- one object Fin 1, one morphism (the identity function on Fin 1).
def Terminal : Cat where
  Obj := Fin 1
  Hom _ _ := Fin 1 → Fin 1
  idH _ := id
  comp p q := q ∘ p
  id_comp _ := rfl
  comp_id _ := rfl
  assoc _ _ _ := rfl

/-- Notation: `𝟙` for the terminal category. -/
notation:max "𝟙" => Terminal

-- K : I ⇒ 𝟙, the unique collapsing functor.
def K : Icat ⇒ 𝟙 where
  o _ := ⟨0, by decide⟩
  f _ := id
  f_id _ := rfl
  f_comp _ _ := rfl

section
variable {C : Cat}

/-- `F c : 𝟙 ⇒ C` — the functor picking the unique 𝟙-object out at `c`.
Strictly, `F` is a *family* of functors (one per `c`); `F c` is the
single functor in Milewski's diagram. -/
def F (c : C.Obj) : 𝟙 ⇒ C where
  o _ := c
  f _ := C.idH c
  f_id _ := rfl
  f_comp _ _ := (C.id_comp (C.idH c)).symm

/-- The constant diagram at `c`, i.e. `K ⋙ F c`. Sometimes written `Δc`. -/
def Δ (c : C.Obj) : Icat ⇒ C := K ⋙ F c

/-- A **cone** over `D` with apex `c` is a natural transformation
`Δc ⇛ D`. (Cone direction = Milewski's Part 1 / Ran.) -/
abbrev Cone (D : Icat ⇒ C) (c : C.Obj) : Type := (Δ c) ⇛ D

/-- The data of a cone over `D` with apex `c`: three legs to the diagram's
three objects, plus the three compatibility equations. -/
structure ConeData (D : Icat ⇒ C) (c : C.Obj) where
  lx : C.Hom c (D.o .X)
  ly : C.Hom c (D.o .Y)
  lz : C.Hom c (D.o .Z)
  hxy : lx ≫ D.f .f = ly
  hyz : ly ≫ D.f .g = lz
  hxz : lx ≫ D.f .h = lz

/-- Build a cone `ε` from a `ConeData`. The result is a real natural
transformation `(Δ c) ⇛ D`; naturality follows from the legs and the
three compatibility equations. -/
def ε {D : Icat ⇒ C} {c : C.Obj} (cd : ConeData D c) : (Δ c) ⇛ D where
  app
    | .X => cd.lx
    | .Y => cd.ly
    | .Z => cd.lz
  naturality m := match m with
    | .id .X => by
        show C.idH c ≫ cd.lx = cd.lx ≫ D.f (I.Hom.id I.Obj.X)
        have h : D.f (I.Hom.id I.Obj.X) = C.idH (D.o I.Obj.X) := D.f_id _
        rw [h]; exact (C.id_comp _).trans (C.comp_id _).symm
    | .id .Y => by
        show C.idH c ≫ cd.ly = cd.ly ≫ D.f (I.Hom.id I.Obj.Y)
        have h : D.f (I.Hom.id I.Obj.Y) = C.idH (D.o I.Obj.Y) := D.f_id _
        rw [h]; exact (C.id_comp _).trans (C.comp_id _).symm
    | .id .Z => by
        show C.idH c ≫ cd.lz = cd.lz ≫ D.f (I.Hom.id I.Obj.Z)
        have h : D.f (I.Hom.id I.Obj.Z) = C.idH (D.o I.Obj.Z) := D.f_id _
        rw [h]; exact (C.id_comp _).trans (C.comp_id _).symm
    | .f     => (C.id_comp _).trans cd.hxy.symm
    | .g     => (C.id_comp _).trans cd.hyz.symm
    | .h     => (C.id_comp _).trans cd.hxz.symm

/-- **Universal property** of a limit cone over `D` with apex `c`:
for every other cone `ε'` (apex `c'`), there is a *unique* morphism
`c' → c` through which `ε'` factors — i.e. each leg of `ε'` equals the
corresponding leg of `ε` post-composed with that morphism. -/
structure IsLimit {D : Icat ⇒ C} {c : C.Obj} (ε : Cone D c) where
  /-- The unique factoring map from any cone's apex to `c`. -/
  lift : ∀ {c' : C.Obj}, Cone D c' → C.Hom c' c
  /-- The factoring map makes the legs commute. -/
  factors : ∀ {c' : C.Obj} (ε' : Cone D c') (i : I.Obj),
    ε'.app i = lift ε' ≫ ε.app i
  /-- The factoring map is unique with this property. -/
  uniq : ∀ {c' : C.Obj} (ε' : Cone D c') (u : C.Hom c' c),
    (∀ (i : I.Obj), ε'.app i = u ≫ ε.app i) → u = lift ε'

/-! ## Worked example: the limit of any triangle diagram is its leftmost vertex

For `D : Icat ⇒ C`, the limit object is `D.o .X` and the limit cone has
legs `idH (D.o .X), D.f .f, D.f .h`. Any other cone `(c', ε')` factors
uniquely through `D.o .X` via its own X-leg `ε'.app .X`.
-/

/-- The limit cone's data: apex `D.o .X`, legs `idH X, D.f .f, D.f .h`. -/
def limCD (D : Icat ⇒ C) : ConeData D (D.o .X) where
  lx := C.idH (D.o .X)
  ly := D.f .f
  lz := D.f .h
  hxy := C.id_comp _                              -- idH X ≫ D.f .f = D.f .f
  hyz := (D.f_comp I.Hom.f I.Hom.g).symm          -- D.f .f ≫ D.f .g = D.f .h
  hxz := C.id_comp _                              -- idH X ≫ D.f .h = D.f .h

/-- Witness that the cone built from `limCD` is a limit. -/
def limIsLimit (D : Icat ⇒ C) : IsLimit (ε (limCD D)) where
  lift ε' := ε'.app .X
  factors ε' i := match i with
    | .X => (C.comp_id _).symm
    | .Y => by
        have h := ε'.naturality I.Hom.f
        rw [show (Δ _).f I.Hom.f = C.idH _ from rfl, C.id_comp] at h
        exact h
    | .Z => by
        have h := ε'.naturality I.Hom.h
        rw [show (Δ _).f I.Hom.h = C.idH _ from rfl, C.id_comp] at h
        exact h
  uniq ε' u huniv := by
    have h := huniv .X
    -- (ε (limCD D)).app .X = C.idH (D.o .X) by rfl, so:
    -- h : ε'.app .X = u ≫ C.idH (D.o .X) = u (by comp_id)
    rw [show (ε (limCD D)).app I.Obj.X = C.idH (D.o I.Obj.X) from rfl] at h
    exact (h.trans (C.comp_id u)).symm

end
