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

/-! ## Example: the `Gr : Grph → DDS` functor (discrete shadow)

Fong & Spivak's running database example uses two schemas:

  Grph :  V   E  with arrows  s, t : E → V
  DDS  :  S        with arrow  next : S → S

The functor `Gr : Grph → DDS` collapses both objects to `S` and sends
`s ↦ id_S`, `t ↦ next`. Given a DDS instance `(X, f : X → X)`, the
precomposition `Δ_Gr(X, f)` is the *trajectory graph*: vertex set `X`,
edge set `X`, source `id`, target `f` — one edge from every state `x`
to `f(x)`.

**Scope caveat.** Our formalization treats `X, Y` as **discrete**
categories, so the arrows `s, t, next` and the choice `Gr(s) = id`,
`Gr(t) = next` are not captured — the example below is the
object-level shadow (`GrObj = {V, E}`, `DDSObj = {S}`, `Gr = const S`).
Even at this level the three functors become concrete operations on
indexed type families, and the adjunctions reduce to familiar
"function-of-pair ↔ pair-of-functions" bijections. The full story
requires Kan extensions; see the discussion above.
-/

namespace GrDDS
open Fong

inductive GrObj | V | E deriving DecidableEq
inductive DDSObj | S deriving DecidableEq

/-- `Gr` on objects. Both `V` and `E` go to the single DDS object `S`.
    (The arrow-level `s ↦ id`, `t ↦ next` is not captured here.) -/
def Gr : GrObj → DDSObj := fun _ => .S

/-! ### Δ_Gr : DDS-object-family  ↦  Grph-object-family

`Δ_Gr(B)` sends both `V` and `E` to the same type `B(S)` — i.e. the state
set is copied into the vertex slot and the edge slot. In Fong's full story
this is exactly why the trajectory-graph construction "uses the states
twice" (once for vertices, once for edges). -/

/-- Δ of a DDS family is the "double" family — `V` and `E` both get `B S`. -/
example (B : DDSObj → Type) : Δ Gr B = (fun _ => B .S) := by
  funext x; cases x <;> rfl

/-- Concrete: the 4-cycle DDS has state set `Fin 4`. Δ_Gr yields 4 vertices
    and 4 edges (the trajectory graph at the object level). -/
def cycle4 : DDSObj → Type := fun .S => Fin 4

example : Δ Gr cycle4 .V = Fin 4 := rfl
example : Δ Gr cycle4 .E = Fin 4 := rfl

/-! ### Σ_Gr : Grph-object-family ↦ DDS-object-family

`Σ_Gr(A)(S)` is the disjoint union `A(V) ⊔ A(E)`, since both objects of
`Grph` have `Gr`-image `S`. (In the full Kan-extension version this
disjoint union gets quotiented using `s, t` and `next` to force
functoriality — here we see only the "before quotient" step.) -/

/-- A small concrete Grph-object-family: 3 vertices, 5 edges.
    `abbrev` so `smallGr .V` reduces to `Fin 3` during elaboration. -/
abbrev smallGr : GrObj → Type
  | .V => Fin 3
  | .E => Fin 5

/-- Elements of `Σ_Gr(smallGr)(S)` tag each element with its source object. -/
example : Sg Gr smallGr .S := Sg.mk .V (0 : Fin 3)   -- vertex 0
example : Sg Gr smallGr .S := Sg.mk .V (2 : Fin 3)   -- vertex 2
example : Sg Gr smallGr .S := Sg.mk .E (4 : Fin 5)   -- edge 4

/-! ### Π_Gr : Grph-object-family ↦ DDS-object-family

`Π_Gr(A)(S)` packages a choice of *one vertex and one edge* simultaneously —
the product `A(V) × A(E)`. -/

/-- Build an element of `Π_Gr(A)(S)` from a vertex and an edge. -/
example (A : GrObj → Type) (v : A .V) (e : A .E) : Pr Gr A .S :=
  fun | .V, _ => v | .E, _ => e

/-! ### Σ ⊣ Δ in this example: `pair of fns ↔ fn out of union`

A morphism `Σ_Gr(A) ⟶ (constant Target)` is the same as a morphism
`A ⟶ Δ_Gr(constant Target) = (V ↦ Target, E ↦ Target)`, which in turn is
the same as a pair of functions `(A V → Target, A E → Target)`.

This is the universal property of the disjoint union. -/

/-- Right-to-left: a pair of functions gives a map out of the union. -/
example {A : GrObj → Type} {Target : Type}
    (fV : A .V → Target) (fE : A .E → Target) :
    Hom (Sg Gr A) (fun _ => Target) :=
  sdInv Gr (fun | .V, a => fV a | .E, a => fE a)

/-- Left-to-right: a map out of the union gives a pair of functions. -/
example {A : GrObj → Type} {Target : Type}
    (g : Hom (Sg Gr A) (fun _ => Target)) :
    (A .V → Target) × (A .E → Target) :=
  let p := sdTo Gr g
  (p .V, p .E)

/-! ### Δ ⊣ Π in this example: `fn into product ↔ pair of fns`

Symmetrically, `(constant Source) ⟶ Π_Gr(A)` corresponds to
`Δ_Gr(constant Source) ⟶ A`, i.e. a pair of functions
`(Source → A V, Source → A E)`. -/

example {A : GrObj → Type} {Source : Type}
    (gV : Source → A .V) (gE : Source → A .E) :
    Hom (fun _ : DDSObj => Source) (Pr Gr A) :=
  dpTo Gr (fun | .V, s => gV s | .E, s => gE s)

example {A : GrObj → Type} {Source : Type}
    (h : Hom (fun _ : DDSObj => Source) (Pr Gr A)) :
    (Source → A .V) × (Source → A .E) :=
  let p := dpInv Gr h
  (p .V, p .E)

end GrDDS

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
