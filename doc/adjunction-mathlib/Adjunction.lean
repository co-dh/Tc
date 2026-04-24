import Mathlib.CategoryTheory.Functor.KanExtension.Adjunction
import Mathlib.CategoryTheory.Functor.KanExtension.Pointwise
import Mathlib.CategoryTheory.Whiskering
import Mathlib.CategoryTheory.Limits.Types.Limits
import Mathlib.CategoryTheory.Limits.Types.Colimits

/-!
# Σ_F ⊣ Δ_F ⊣ Π_F via Mathlib — the *real* (Kan-extension) version
# (Fong & Spivak, *Seven Sketches in Compositionality*, Ch. 3)

The companion file `../sigma_delta_pi.lean` only handles the case where
`X, Y` are **discrete** categories (i.e. `f` is a plain set function).
Fong's actual setting has `F : C ⥤ D` between **schema categories** with
non-trivial arrows and equations.

This file delivers the full version using **Mathlib**:

* `Δ_F` is just precomposition: `(whiskeringLeft C D E).obj F`.
* `Σ_F = F.lan` is the **left Kan extension** along `F`.
* `Π_F = F.ran` is the **right Kan extension** along `F`.
* The two adjunctions come from Mathlib's
  `Functor.lanAdjunction` / `Functor.ranAdjunction`.

We specialize to the running example `Gr : Grph ⥤ DDS` with the real
arrows `s, t, next` — the choice `Gr(s) = 𝟙_S`, `Gr(t) = next` is now
genuinely part of the formalization.

## How to compile

This sub-project depends on Mathlib. From `doc/adjunction-mathlib/`:

```
lake update    # first time only — downloads Mathlib (~10 GB, slow)
lake build
```

If `lake build` fails with API drift on `Functor.lanAdjunction` or
`Functor.ranAdjunction`, bump the Mathlib `version` in `lakefile.toml`
and check `Mathlib/CategoryTheory/Functor/KanExtension/` for the
current names.
-/

namespace FongMathlib

open CategoryTheory CategoryTheory.Functor

universe v v' u u'

/-! ## The general adjunction triple

For *any* functor `F : C ⥤ D` between (locally small) categories,
the triple `F.lan ⊣ Δ_F ⊣ F.ran` exists in Mathlib once the relevant
pointwise Kan extensions exist. For `E = Type _` and small `C, D`,
those instances are automatic because `Type _` has all small (co)limits.
-/

section Triple

variable {C : Type u} [Category.{v} C]
variable {D : Type u'} [Category.{v'} D]

/-- `Δ_F`: precomposition with `F`. -/
abbrev DeltaF (F : C ⥤ D) (E : Type*) [Category E] : (D ⥤ E) ⥤ (C ⥤ E) :=
  (whiskeringLeft C D E).obj F

/-- `Σ_F` = left Kan extension along `F`. -/
noncomputable abbrev SigmaF (F : C ⥤ D) (E : Type*) [Category E]
    [∀ G : C ⥤ E, F.HasLeftKanExtension G] : (C ⥤ E) ⥤ (D ⥤ E) :=
  F.lan

/-- `Π_F` = right Kan extension along `F`. -/
noncomputable abbrev PiF (F : C ⥤ D) (E : Type*) [Category E]
    [∀ G : C ⥤ E, F.HasRightKanExtension G] : (C ⥤ E) ⥤ (D ⥤ E) :=
  F.ran

/-- **Σ_F ⊣ Δ_F.** -/
noncomputable def sigmaDeltaAdj (F : C ⥤ D) (E : Type*) [Category E]
    [∀ G : C ⥤ E, F.HasLeftKanExtension G] :
    SigmaF F E ⊣ DeltaF F E :=
  F.lanAdjunction E

/-- **Δ_F ⊣ Π_F.** -/
noncomputable def deltaPiAdj (F : C ⥤ D) (E : Type*) [Category E]
    [∀ G : C ⥤ E, F.HasRightKanExtension G] :
    DeltaF F E ⊣ PiF F E :=
  F.ranAdjunction E

end Triple

/-! ## Example: `Gr : Grph ⥤ DDS` with real arrows

`Grph` is the free category on the quiver with two parallel arrows
`s, t : E → V`. `DDS` is the free monoid `ℕ` viewed as a one-object
category (free category on a single self-loop `next`).

`Gr : Grph ⥤ DDS` collapses both objects to the unique `DDS` object,
sending `s ↦ 𝟙` (i.e. `0 ∈ ℕ`) and `t ↦ next` (i.e. `1 ∈ ℕ`).
-/

/-! ### `Grph` -/

inductive GrphObj | V | E
  deriving DecidableEq

inductive GrphHom : GrphObj → GrphObj → Type
  | id  : (o : GrphObj) → GrphHom o o
  | s   : GrphHom .E .V
  | t   : GrphHom .E .V

namespace GrphHom

/-- Composition. The two arms are exhaustive: by the indices, no two
    non-identity arrows can compose (`s, t` both go `E → V` and there's
    no `V → E`), so at least one factor must be an identity. -/
def comp : ∀ {a b c : GrphObj}, GrphHom a b → GrphHom b c → GrphHom a c
  | _, _, _, .id _, g => g
  | _, _, _, f,     .id _ => f

end GrphHom

instance : Category GrphObj where
  Hom         := GrphHom
  id          := GrphHom.id
  comp        := GrphHom.comp
  id_comp _   := rfl
  comp_id f   := by cases f <;> rfl
  assoc f g h := by cases f <;> cases g <;> cases h <;> rfl

abbrev Grph := GrphObj

/-! ### `DDS`

One-object category whose morphisms are wrapped naturals (we wrap in a
structure so anonymous constructors `⟨n⟩` give the right type — bare
`(n : ℕ)` doesn't unify with `(_ ⟶ _)` because the `Hom` field of the
`Category` instance isn't reducible). Composition is addition. -/

abbrev DDS := PUnit.{1}

@[ext] structure DDSHom (_ _ : DDS) : Type where
  iter : ℕ

instance : Category DDS where
  Hom         := DDSHom
  id _        := ⟨0⟩
  comp f g    := ⟨f.iter + g.iter⟩
  id_comp _   := by ext; simp
  comp_id _   := by ext; rfl
  assoc _ _ _ := by ext; simp [Nat.add_assoc]

/-! ### The functor `Gr` -/

def Gr : Grph ⥤ DDS where
  obj _ := PUnit.unit
  map f := match f with
    | .id _ => ⟨0⟩       -- s ↦ 𝟙_S in DDS = ⟨0⟩
    | .s    => ⟨0⟩       -- s ↦ 𝟙_S
    | .t    => ⟨1⟩       -- t ↦ next
  map_id _ := rfl
  map_comp f g := by cases f <;> cases g <;> rfl

/-! ### Specializing the adjunction triple to `Gr` -/

section GrAdj

abbrev T := Type

/-- `Δ_Gr : (DDS ⥤ Type) ⥤ (Grph ⥤ Type)`. Given a DDS-instance
    `(X, f : X → X)`, produces the trajectory graph: vertex set `X`,
    edge set `X`, `src = id_X` (because `Gr(s) = 𝟙`), `tgt = f`
    (because `Gr(t) = next`). -/
noncomputable abbrev Delta_Gr : (DDS ⥤ T) ⥤ (Grph ⥤ T) := DeltaF Gr T

/-- `Σ_Gr` — the *free* DDS on a graph. -/
noncomputable abbrev Sigma_Gr : (Grph ⥤ T) ⥤ (DDS ⥤ T) := SigmaF Gr T

/-- `Π_Gr` — the *cofree* DDS on a graph. -/
noncomputable abbrev Pi_Gr : (Grph ⥤ T) ⥤ (DDS ⥤ T) := PiF Gr T

/-- **`Σ_Gr ⊣ Δ_Gr`**. -/
noncomputable example : Sigma_Gr ⊣ Delta_Gr := sigmaDeltaAdj Gr T

/-- **`Δ_Gr ⊣ Π_Gr`**. -/
noncomputable example : Delta_Gr ⊣ Pi_Gr := deltaPiAdj Gr T

end GrAdj

/-! ## A note on concrete computation

For an explicit numerical example like the 4-cycle DDS (`X = Fin 4`,
`next = (· + 1)`) you'd want to define a concrete `cycle4 : DDS ⥤ Type`
and then read off `Δ_Gr.obj cycle4` as the trajectory graph
`0 → 1 → 2 → 3 → 0`. Two practical wrinkles in Lean/Mathlib:

* `Fin n` does not have a uniform `Nat.cast` without `NeZero n`, so the
  arithmetic `cycle4.map k = (· + (k : Fin 4))` needs a slightly
  fiddlier definition (e.g. `Function.iterate` of a successor map, or
  `Fin.ofNat n k`).
* `Δ_Gr` is built via `whiskeringLeft` and is `noncomputable`, so
  pointwise equalities like `(Δ_Gr.obj cycle4).obj .V = Fin 4` are true
  but **don't reduce by `rfl`** — they require running the
  whiskering definition through `simp` with the right lemmas (e.g.
  `whiskeringLeft_obj_obj`, `Functor.comp_obj`).

Both are routine but distract from the math; the abstract triple above
is the real content. -/

end FongMathlib
