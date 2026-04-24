/-!
# Σ ⊣ Δ ⊣ Π as a triple of functor adjunctions
# (Fong & Spivak, *Seven Sketches in Compositionality*, Ch. 3)

Setting. Take `X` and `Y` as **discrete** categories (types), so the instance
categories `[X, Type]` and `[Y, Type]` are just type families, and a natural
transformation `A ⟶ B` is a pointwise family `(x : X) → A x → B x`.

A function `f : X → Y` induces three functors between `[X, Type]` and
`[Y, Type]`:

  Σ_f : [X, Type] ⟶ [Y, Type]     (dependent sum / left Kan extension)
  Δ_f : [Y, Type] ⟶ [X, Type]     (precomposition by `f`)
  Π_f : [X, Type] ⟶ [Y, Type]     (dependent product / right Kan extension)

We prove:

1. **Each is a functor**: an action on morphisms that preserves `id` and
   `∘` (`Δ_id`, `Δ_comp`, `Sg_id`, `Sg_comp`, `Pr_id`, `Pr_comp`).
2. **`Σ_f ⊣ Δ_f`**: a bijection `Hom(Σ_f A, B) ≃ Hom(A, Δ_f B)` with both
   round-trips (`sd_left_inv`, `sd_right_inv`).
3. **`Δ_f ⊣ Π_f`**: a bijection `Hom(Δ_f B, A) ≃ Hom(B, Π_f A)` with both
   round-trips (`dp_left_inv`, `dp_right_inv`).
4. **Naturality** of both bijections in each variable
   (`sd_nat_left`, `sd_nat_right`, `dp_nat_left`, `dp_nat_right`).

`Σ_f A` is encoded as an indexed inductive so its adjunction isomorphism
holds definitionally. `Δ`, `Π`, `Hom` are `abbrev`s so Lean unfolds them
when checking `rfl`/`cases` goals.
-/

namespace Fong

/-! ## The `[X, Type]` category -/

abbrev Hom {X : Type} (A B : X → Type) : Type := (x : X) → A x → B x

namespace Hom
abbrev id' {X : Type} (A : X → Type) : Hom A A := fun _ a => a
abbrev comp {X : Type} {A B C : X → Type}
    (β : Hom B C) (α : Hom A B) : Hom A C := fun x a => β x (α x a)
end Hom

/-! ## The three functors -/

section Functors
variable {X Y : Type} (f : X → Y)

/-- `Δ_f` on objects: precomposition. -/
abbrev Δ (B : Y → Type) : X → Type := fun x => B (f x)

/-- `Δ_f` on morphisms. -/
def Δmap {B B' : Y → Type} (β : Hom B B') : Hom (Δ f B) (Δ f B') :=
  fun x => β (f x)

theorem Δ_id (B : Y → Type) : Δmap f (Hom.id' B) = Hom.id' (Δ f B) := rfl

theorem Δ_comp {B B' B'' : Y → Type} (β : Hom B B') (γ : Hom B' B'') :
    Δmap f (Hom.comp γ β) = Hom.comp (Δmap f γ) (Δmap f β) := rfl

/-- `Σ_f A` as an indexed inductive: `Sg.mk x a : Sg f A (f x)`. -/
inductive Sg (A : X → Type) : Y → Type where
  | mk (x : X) (a : A x) : Sg A (f x)

/-- `Σ_f` on morphisms. -/
def Sgmap {A A' : X → Type} (α : Hom A A') : Hom (Sg f A) (Sg f A')
  | _, Sg.mk x a => Sg.mk x (α x a)

theorem Sg_id (A : X → Type) : Sgmap f (Hom.id' A) = Hom.id' (Sg f A) := by
  funext _ p; cases p; rfl

theorem Sg_comp {A A' A'' : X → Type} (α : Hom A A') (γ : Hom A' A'') :
    Sgmap f (Hom.comp γ α) = Hom.comp (Sgmap f γ) (Sgmap f α) := by
  funext _ p; cases p; rfl

/-- `Π_f` on objects. -/
abbrev Pr (A : X → Type) : Y → Type := fun y => (x : X) → f x = y → A x

/-- `Π_f` on morphisms. -/
def Prmap {A A' : X → Type} (α : Hom A A') : Hom (Pr f A) (Pr f A') :=
  fun _ h x hxy => α x (h x hxy)

theorem Pr_id (A : X → Type) : Prmap f (Hom.id' A) = Hom.id' (Pr f A) := rfl

theorem Pr_comp {A A' A'' : X → Type} (α : Hom A A') (γ : Hom A' A'') :
    Prmap f (Hom.comp γ α) = Hom.comp (Prmap f γ) (Prmap f α) := rfl

end Functors

/-! ## Σ ⊣ Δ : bijection `Hom(Σ_f A, B) ≃ Hom(A, Δ_f B)` -/

section SigmaDelta
variable {X Y : Type} (f : X → Y)

/-- Transpose right to left. -/
def sdTo {A : X → Type} {B : Y → Type} (h : Hom (Sg f A) B) : Hom A (Δ f B) :=
  fun x a => h (f x) (Sg.mk x a)

/-- Transpose left to right. -/
def sdInv {A : X → Type} {B : Y → Type} (h : Hom A (Δ f B)) : Hom (Sg f A) B
  | _, Sg.mk x a => h x a

theorem sd_left_inv {A : X → Type} {B : Y → Type} (h : Hom (Sg f A) B) :
    sdInv f (sdTo f h) = h := by
  funext _ p; cases p; rfl

theorem sd_right_inv {A : X → Type} {B : Y → Type} (h : Hom A (Δ f B)) :
    sdTo f (sdInv f h) = h := rfl

/-- Naturality in `A` (contravariantly). -/
theorem sd_nat_left {A A' : X → Type} {B : Y → Type}
    (α : Hom A' A) (h : Hom (Sg f A) B) :
    sdTo f (Hom.comp h (Sgmap f α)) = Hom.comp (sdTo f h) α := rfl

/-- Naturality in `B` (covariantly). -/
theorem sd_nat_right {A : X → Type} {B B' : Y → Type}
    (β : Hom B B') (h : Hom (Sg f A) B) :
    sdTo f (Hom.comp β h) = Hom.comp (Δmap f β) (sdTo f h) := rfl

end SigmaDelta

/-! ## Δ ⊣ Π : bijection `Hom(Δ_f B, A) ≃ Hom(B, Π_f A)` -/

section DeltaPi
variable {X Y : Type} (f : X → Y)

/-- Transpose left to right. Given `B y` and `hxy : f x = y`, we transport
    along `hxy.symm : y = f x` to land in `B (f x) = Δ_f B x`, then apply `h`. -/
def dpTo {A : X → Type} {B : Y → Type} (h : Hom (Δ f B) A) : Hom B (Pr f A) :=
  fun _ b x hxy => h x (show B (f x) from hxy.symm ▸ b)

/-- Transpose right to left. Uses `rfl : f x = f x` to extract the fibre. -/
def dpInv {A : X → Type} {B : Y → Type} (h : Hom B (Pr f A)) : Hom (Δ f B) A :=
  fun x b => h (f x) b x rfl

theorem dp_left_inv {A : X → Type} {B : Y → Type} (h : Hom (Δ f B) A) :
    dpInv f (dpTo f h) = h := rfl

theorem dp_right_inv {A : X → Type} {B : Y → Type} (h : Hom B (Pr f A)) :
    dpTo f (dpInv f h) = h := by
  funext _ _ _ hxy; cases hxy; rfl

/-- Naturality in `B` (contravariantly). -/
theorem dp_nat_left {A : X → Type} {B B' : Y → Type}
    (β : Hom B' B) (h : Hom (Δ f B) A) :
    dpTo f (Hom.comp h (Δmap f β)) = Hom.comp (dpTo f h) β := by
  funext _ _ _ hxy; cases hxy; rfl

/-- Naturality in `A` (covariantly). -/
theorem dp_nat_right {A A' : X → Type} {B : Y → Type}
    (α : Hom A A') (h : Hom (Δ f B) A) :
    dpTo f (Hom.comp α h) = Hom.comp (Prmap f α) (dpTo f h) := rfl

end DeltaPi

/-! ## Unit / counit (triangle data), for illustration -/

section UnitCounit
variable {X Y : Type} (f : X → Y)

/-- Unit of `Σ ⊣ Δ` : `A ⟶ Δ (Σ A)`. -/
def η_sd (A : X → Type) : Hom A (Δ f (Sg f A)) :=
  sdTo f (Hom.id' (Sg f A))

/-- Counit of `Σ ⊣ Δ` : `Σ (Δ B) ⟶ B`. -/
def ε_sd (B : Y → Type) : Hom (Sg f (Δ f B)) B :=
  sdInv f (Hom.id' (Δ f B))

/-- Unit of `Δ ⊣ Π` : `B ⟶ Π (Δ B)`. -/
def η_dp (B : Y → Type) : Hom B (Pr f (Δ f B)) :=
  dpTo f (Hom.id' (Δ f B))

/-- Counit of `Δ ⊣ Π` : `Δ (Π A) ⟶ A`. -/
def ε_dp (A : X → Type) : Hom (Δ f (Pr f A)) A :=
  dpInv f (Hom.id' (Pr f A))

end UnitCounit

end Fong

/-! ## Final summary of the two adjunctions -/

-- Σ_f ⊣ Δ_f: bijection of Hom-sets, with naturality
#check @Fong.sdTo          -- Hom (Σ_f A) B → Hom A (Δ_f B)
#check @Fong.sdInv         -- Hom A (Δ_f B) → Hom (Σ_f A) B
#check @Fong.sd_left_inv   -- sdInv ∘ sdTo = id
#check @Fong.sd_right_inv  -- sdTo ∘ sdInv = id
#check @Fong.sd_nat_left   -- naturality in A
#check @Fong.sd_nat_right  -- naturality in B

-- Δ_f ⊣ Π_f: bijection of Hom-sets, with naturality
#check @Fong.dpTo          -- Hom (Δ_f B) A → Hom B (Π_f A)
#check @Fong.dpInv         -- Hom B (Π_f A) → Hom (Δ_f B) A
#check @Fong.dp_left_inv   -- dpInv ∘ dpTo = id
#check @Fong.dp_right_inv  -- dpTo ∘ dpInv = id
#check @Fong.dp_nat_left   -- naturality in B
#check @Fong.dp_nat_right  -- naturality in A
