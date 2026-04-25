import Mathlib.CategoryTheory.Functor.KanExtension.Adjunction
import Mathlib.CategoryTheory.Functor.KanExtension.Pointwise
import Mathlib.CategoryTheory.Whiskering
import Mathlib.CategoryTheory.Limits.Types.Limits
import Mathlib.CategoryTheory.Limits.Types.Colimits

/-!
# Σ_F ⊣ Δ_F ⊣ Π_F via Mathlib (Fong & Spivak, *Seven Sketches*, Ch. 3)

We prove the adjunction triple at the actual textbook level: a functor
`F : C ⥤ D` between arbitrary categories induces three functors between
the *instance categories* `[C, Type]` and `[D, Type]`,

    Σ_F  =  Lan_F  : [C, Type] → [D, Type]   (left  Kan extension along F)
    Δ_F  =  − ∘ F  : [D, Type] → [C, Type]   (precomposition / pullback)
    Π_F  =  Ran_F  : [C, Type] → [D, Type]   (right Kan extension along F)

with adjunctions  Σ_F ⊣ Δ_F ⊣ Π_F.  We then specialize to Fong's running
example `Gr : Grph ⥤ DDS`.

## Lean / Mathlib syntax for category theory, in one breath

A reader who knows the math but not Lean will want this dictionary:

| Lean / Mathlib              | Math meaning                                       |
|-----------------------------|----------------------------------------------------|
| `[Category C]`              | "C is a category" — typeclass attaching `Hom`,    |
|                             | `id`, `∘` to `C` (kept reusable across proofs)     |
| `X ⟶ Y`                     | `Hom_C(X, Y)` (typed `\hom` in editor)             |
| `f ≫ g`                     | `g ∘ f` — diagrammatic order: f first, then g!     |
| `𝟙 X`                       | `id_X : X ⟶ X` (typed `\b1`)                       |
| `C ⥤ D`                     | a *functor* C → D — bundled struct of obj+map+laws |
| `F.obj X`                   | `F(X)` on objects                                  |
| `F.map f`                   | `F(f)` on morphisms                                |
| `F ⋙ G`                     | composition `G ∘ F` (read left-to-right)           |
| `α : F ⟶ G`                 | natural transformation between `F, G : C ⥤ D`     |
| `α.app X`                   | the component `α_X : F(X) ⟶ G(X)`                  |
| `F ⊣ G`                     | "F is left adjoint to G"                           |
| `[HasLeftKanExtension F G]` | the typeclass "Lan_F G exists" (pointwise)         |
| `noncomputable`             | "this isn't an algorithm" — required when the      |
|                             |  construction uses Choice or limits/colimits       |

Universe annotations `Type u`, `Type v`, etc. are size markers; for
us, plain `Type` (= `Type 0`) is the right home for set-valued instance
functors. Mathlib's category typeclass takes two universes: one for
objects, one for morphisms (so `Category.{v} C` with `C : Type u` lets
morphism types live in `Type v`).

## Build

```
lake update    # first time only — fetches Mathlib + olean cache
lake build
```
-/

namespace FongMathlib

-- `open` brings names from a namespace into scope unprefixed. The Kan
-- extension/whiskering API lives under `CategoryTheory.Functor`, so we
-- open both layers to write `whiskeringLeft`, `lan`, `lanAdjunction`
-- without the long prefix.
open CategoryTheory CategoryTheory.Functor

universe v v' u u'

/-! ## Part 1 — the abstract triple

For *any* `F : C ⥤ D`, the triple exists whenever the relevant pointwise
Kan extensions do. For the target `E = Type _` and small `C, D` the
required `HasLeftKanExtension` / `HasRightKanExtension` instances are
supplied by Mathlib for free, because `Type _` has all small (co)limits.

The `variable` declarations introduce *implicit* type and category
parameters that Lean fills in from context — this saves us re-typing
`{C : Type u} [Category.{v} C]` in every signature below.
-/

section Triple

variable {C : Type u} [Category.{v} C]
variable {D : Type u'} [Category.{v'} D]

/-- `Δ_F` — precomposition with `F`.

`whiskeringLeft C D E : (C ⥤ D) ⥤ ((D ⥤ E) ⥤ (C ⥤ E))` is Mathlib's
*bifunctor* "precompose-with". Applying it to `F` (via `.obj F`) gives
the actual restriction-along-`F` functor between instance categories. -/
abbrev DeltaF (F : C ⥤ D) (E : Type*) [Category E] : (D ⥤ E) ⥤ (C ⥤ E) :=
  (whiskeringLeft C D E).obj F

/-- `Σ_F` — left Kan extension along `F`.

`F.lan` is the action on the functor category: `(F.lan).obj G = Lan_F G`.
The instance hypothesis `[∀ G, F.HasLeftKanExtension G]` says the left
Kan extension exists for *every* source diagram `G`. The result is
`noncomputable` because building a Kan extension uses colimits — fine for
proofs, but Lean refuses to extract running code from it. -/
noncomputable abbrev SigmaF (F : C ⥤ D) (E : Type*) [Category E]
    [∀ G : C ⥤ E, F.HasLeftKanExtension G] : (C ⥤ E) ⥤ (D ⥤ E) :=
  F.lan

/-- `Π_F` — right Kan extension along `F`. Dual of `SigmaF`. -/
noncomputable abbrev PiF (F : C ⥤ D) (E : Type*) [Category E]
    [∀ G : C ⥤ E, F.HasRightKanExtension G] : (C ⥤ E) ⥤ (D ⥤ E) :=
  F.ran

/-- **Σ_F ⊣ Δ_F.** Mathlib's `Functor.lanAdjunction` is *exactly* the
statement that left Kan extension is left adjoint to precomposition; we
just rename it to match the schema-functor literature. -/
noncomputable def sigmaDeltaAdj (F : C ⥤ D) (E : Type*) [Category E]
    [∀ G : C ⥤ E, F.HasLeftKanExtension G] :
    SigmaF F E ⊣ DeltaF F E :=
  F.lanAdjunction E

/-- **Δ_F ⊣ Π_F.** Dually: precomposition ⊣ right Kan extension. -/
noncomputable def deltaPiAdj (F : C ⥤ D) (E : Type*) [Category E]
    [∀ G : C ⥤ E, F.HasRightKanExtension G] :
    DeltaF F E ⊣ PiF F E :=
  F.ranAdjunction E

end Triple

/-! ## Part 2 — building the schemas as Lean categories

To make the abstract triple *concrete* on `Gr : Grph ⥤ DDS`, we have to
actually construct `Grph` and `DDS` as instances of Mathlib's `Category`
typeclass. Two routes:

* **Pretty:** use Mathlib's `Quiver` and `Paths` (free category on a
  quiver). Cleaner conceptually, but pulls in extra machinery.
* **Hands-on (used here):** write the morphism inductive by hand and
  supply the `Category` instance directly. For tiny categories like
  `Grph` (4 morphisms total) this is the smaller and more transparent
  presentation.
-/

/-! ### `Grph` — the schema of graphs

Two objects `V, E`, two parallel arrows `s, t : E → V`, no equations.
As a category, `Grph` has exactly four morphisms: `𝟙_V, 𝟙_E, s, t`. -/

inductive GrphObj | V | E
  deriving DecidableEq

/-- The four morphisms of `Grph`, indexed by source and target.

This is a *generalised inductive type* (GADT): each constructor declares
its own indices, so e.g. `GrphHom .V .E` is empty (no `V → E` arrow) and
`GrphHom .E .V` contains exactly `s` and `t`. Lean's pattern-match
exhaustiveness checker reads the indices, so when we `cases f` on a
`f : GrphHom .V .V` it will only produce the `id` case automatically —
which is what makes the proofs below short. -/
inductive GrphHom : GrphObj → GrphObj → Type
  | id  : (o : GrphObj) → GrphHom o o
  | s   : GrphHom .E .V
  | t   : GrphHom .E .V

namespace GrphHom

/-- Composition.

Because `s, t` both have source `E` and target `V`, and there is no
`V → E` arrow, *no two non-identity morphisms can compose*: at least one
factor of any composition must be an identity. The two `match` arms
below therefore cover every legal input — Lean verifies this from the
GADT indices, with no `unreachable` warning. -/
def comp : ∀ {a b c : GrphObj}, GrphHom a b → GrphHom b c → GrphHom a c
  | _, _, _, .id _, g => g
  | _, _, _, f,     .id _ => f

end GrphHom

/-- `Grph` as a Mathlib `Category`.

`where`-syntax fills in the typeclass fields one by one:

* `Hom`, `id`, `comp` give the categorical structure.
* `id_comp`, `comp_id`, `assoc` are the three category laws.

The laws are tactic proofs by case analysis. The combinator `t1 <;> t2`
runs `t2` on *every* goal produced by `t1` (the "all-goals" semicolon).
So `cases f <;> cases g <;> cases h <;> rfl` enumerates the at-most-2³
cases — most of which are eliminated by GADT indices — and discharges
each one by `rfl` once `comp` reduces. -/
instance : Category GrphObj where
  Hom         := GrphHom
  id          := GrphHom.id
  comp        := GrphHom.comp
  id_comp _   := rfl
  comp_id f   := by cases f <;> rfl
  assoc f g h := by cases f <;> cases g <;> cases h <;> rfl

abbrev Grph := GrphObj

/-! ### `DDS` — the schema of discrete dynamical systems

One object `S`, one generator `next : S → S`, no equations. As a category
it's the *free monoid on one generator*: morphisms are non-negative
integers (number of `next`s applied), composition is addition.

A Lean subtlety. The naive

    instance : Category DDS where
      Hom _ _ := ℕ;  id _ := 0;  comp f g := f + g

stores `Hom = ℕ` in the instance, but `(_ ⟶ _)` notation reaches `Hom`
through a non-reducible indirection (`Quiver.Hom`). Lean's elaborator
then refuses to unify a bare `(0 : ℕ)` with `(PUnit.unit ⟶ PUnit.unit)`,
even though they are definitionally equal. The simplest fix is to wrap
`ℕ` in a one-field structure, so anonymous constructor `⟨n⟩` types
*directly* at the morphism type. -/

abbrev DDS := PUnit.{1}

/-- `DDSHom S S` is `ℕ` in disguise. The `@[ext]` attribute auto-generates
the extensionality lemma `(a.iter = b.iter) → a = b`, used in the proofs
below to reduce equality of `DDSHom` to equality of underlying nats. -/
@[ext] structure DDSHom (_ _ : DDS) : Type where
  iter : ℕ

instance : Category DDS where
  Hom         := DDSHom
  id _        := ⟨0⟩                -- 𝟙_S = "do nothing" = next^0
  comp f g    := ⟨f.iter + g.iter⟩  -- next^m  ≫  next^n  =  next^(m+n)
  id_comp _   := by ext; simp                       -- 0 + n = n
  comp_id _   := by ext; rfl                        -- n + 0 = n  (defeq)
  assoc _ _ _ := by ext; simp [Nat.add_assoc]       -- (m+n)+p = m+(n+p)

/-! ### The functor `Gr : Grph ⥤ DDS`

Both `V` and `E` of `Grph` collapse to the unique `DDS` object. On
generators: `s ↦ 𝟙_S` (encoded as `⟨0⟩`, "no `next`s applied"),
`t ↦ next` (encoded as `⟨1⟩`). The functor laws follow by case
analysis just as for `Grph`'s `assoc` above. -/

def Gr : Grph ⥤ DDS where
  obj _ := PUnit.unit
  map f := match f with
    | .id _ => ⟨0⟩
    | .s    => ⟨0⟩      -- s ↦ 𝟙_S
    | .t    => ⟨1⟩      -- t ↦ next
  map_id _ := rfl
  map_comp f g := by cases f <;> cases g <;> rfl

/-! ## Part 3 — specializing the triple to `Gr`

Once `Gr` is a real `Grph ⥤ DDS`, `DeltaF Gr T`, `SigmaF Gr T`, `PiF Gr T`
are bona fide instantiations of Part 1. The pointwise-Kan-extension
hypotheses are filled in automatically by Mathlib because the target
`T = Type` admits all small (co)limits and our schemas are small.
-/

section GrAdj

/-- The target where instance-functors live. `T = Type` matches the
textbook's "`Set`" — the morphisms in this category are plain functions. -/
abbrev T := Type

/-- `Δ_Gr : (DDS ⥤ Type) ⥤ (Grph ⥤ Type)`.

A DDS-instance is a set `X` with a self-map `f : X → X` (encoded as a
functor `DDS ⥤ Type` sending `S ↦ X` and `next ↦ f`). `Δ_Gr` returns the
*trajectory graph*:

* vertex set = `X` (because `Gr.obj V = S`),
* edge set = `X` (because `Gr.obj E = S`),
* `src = id_X` (because `Gr.map s = 𝟙_S`, and `Δ_Gr` precomposes),
* `tgt = f`   (because `Gr.map t = next`, and `Δ_Gr` precomposes).

Concretely: one edge from each state `x` to `f(x)`. -/
noncomputable abbrev Delta_Gr : (DDS ⥤ T) ⥤ (Grph ⥤ T) := DeltaF Gr T

/-- `Σ_Gr` — the *free* DDS on a graph. -/
noncomputable abbrev Sigma_Gr : (Grph ⥤ T) ⥤ (DDS ⥤ T) := SigmaF Gr T

/-- `Π_Gr` — the *cofree* DDS on a graph. -/
noncomputable abbrev Pi_Gr : (Grph ⥤ T) ⥤ (DDS ⥤ T) := PiF Gr T

/-- **`Σ_Gr ⊣ Δ_Gr`** — by specialising the abstract `sigmaDeltaAdj`. -/
noncomputable example : Sigma_Gr ⊣ Delta_Gr := sigmaDeltaAdj Gr T

/-- **`Δ_Gr ⊣ Π_Gr`** — by specialising the abstract `deltaPiAdj`. -/
noncomputable example : Delta_Gr ⊣ Pi_Gr := deltaPiAdj Gr T

end GrAdj

/-! ## A note on concrete computation

It is tempting to define the 4-cycle DDS by

    cycle4 : DDS ⥤ Type
    cycle4.obj _ := Fin 4
    cycle4.map ⟨k⟩ := (· + k : Fin 4 → Fin 4)

and then *evaluate* `Δ_Gr.obj cycle4` to recover the directed cycle
`0 → 1 → 2 → 3 → 0` numerically. Two practical wrinkles:

1. `Fin n` does not have a uniform `Nat.cast` without `[NeZero n]`, so
   `(k : Fin 4)` needs a slightly fiddlier definition (e.g.
   `Function.iterate` of a successor map, or `Fin.ofNat n k`).
2. `Δ_Gr` is built via `whiskeringLeft` and is `noncomputable`, so
   pointwise equalities like `(Δ_Gr.obj cycle4).obj .V = Fin 4` are
   *true* but **don't reduce by `rfl`** — extracting them requires
   `simp` with whiskering lemmas (`whiskeringLeft_obj_obj`,
   `Functor.comp_obj`).

Both are routine but distract from the math; the abstract triple in
Part 3 is the real content of the file.
-/

end FongMathlib
