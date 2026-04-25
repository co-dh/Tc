import kan_extension

/-!
Fong's schema-migration triple as a Lean function.

Given any functor `F : Src ⇒ Tgt` between schema categories, we get
three functors between the instance categories `[Src, X]` and `[Tgt, X]`
(for some target category `X`, e.g. `Type`):

   Σ_F : [Src, X] ⇒ [Tgt, X]   (left Kan extension)
   Δ_F : [Tgt, X] ⇒ [Src, X]   (precomposition with F)
   Π_F : [Src, X] ⇒ [Tgt, X]   (right Kan extension)

`Δ_F` is just `F ⋙ G` and is built fully here.  `Σ_F` and `Π_F` need
Kan extensions, not implemented in our minimal framework — see
`adjunction-mathlib/Adjunction.lean` for the Mathlib version.

We illustrate with Fong's example  `Gr ⇒ DDS`  (graphs to discrete
dynamical systems).
-/

/-! ### `Gr` — graph schema (2 objects V, E; arrows s, t : E → V) -/

inductive GrObj | V | E
inductive GrHom : GrObj → GrObj → Type
  | id : (o : GrObj) → GrHom o o
  | s  : GrHom .E .V
  | t  : GrHom .E .V

def GrHom.comp : ∀ {a b c : GrObj}, GrHom a b → GrHom b c → GrHom a c
  | _, _, _, .id _, m => m
  | _, _, _, m,     .id _ => m

def Gr : Cat where
  Obj := GrObj
  Hom := GrHom
  idH := GrHom.id
  comp := GrHom.comp
  id_comp m := by cases m <;> rfl
  comp_id m := by cases m <;> rfl
  assoc α β γ := by cases α <;> cases β <;> cases γ <;> rfl

/-! ### `DDS` — DDS schema (1 object S; morphisms = ℕ, composition = +) -/

inductive DDSObj | S
abbrev DDSHom (_ _ : DDSObj) : Type := Nat

def DDS : Cat where
  Obj := DDSObj
  Hom := DDSHom
  idH _ := 0
  comp p q := p + q
  id_comp _ := Nat.zero_add _
  comp_id _ := rfl
  assoc _ _ _ := Nat.add_assoc _ _ _

/-! ### Fong's functor `Gr ⇒ DDS`: collapse both objects to S, s ↦ id, t ↦ next -/

def Gr_to_DDS : Gr ⇒ DDS where
  o _ := .S
  f
    | .id _ => (0 : Nat)      -- identities ↦ 𝟙_S = 0
    | .s    => (0 : Nat)      -- s ↦ 𝟙_S
    | .t    => (1 : Nat)      -- t ↦ next
  f_id _ := rfl
  f_comp p q := by cases p <;> cases q <;> rfl

/-! ### The schema-migration triple

Given `F : Src ⇒ Tgt` and a target category `X`, the triple consists of
three functors. Only `Δ` is fully built here. -/

structure Triple {Src Tgt : Cat} (_F : Src ⇒ Tgt) (X : Cat) where
  /-- Σ_F (left Kan extension). Skipped — needs Kan-extension machinery. -/
  lan : Unit
  /-- Δ_F (precomposition with F): pulls Tgt-instances back to Src-instances. -/
  delta : (Tgt ⇒ X) → (Src ⇒ X)
  /-- Π_F (right Kan extension). Skipped. -/
  ran : Unit

/-- The triple as a function of `F`. -/
def schemaMigration {Src Tgt : Cat} (F : Src ⇒ Tgt) (X : Cat) : Triple F X where
  lan := ()
  delta G := F ⋙ G
  ran := ()

/-- Specialise to Fong's `Gr ⇒ DDS` and any target category `X`. -/
def fongMigration (X : Cat) : Triple Gr_to_DDS X := schemaMigration Gr_to_DDS X

-- Sanity: `(fongMigration X).delta` takes a DDS-instance (functor `DDS ⇒ X`)
-- and returns a Gr-instance (functor `Gr ⇒ X`) — the trajectory graph.
example (X : Cat) (D : DDS ⇒ X) : Gr ⇒ X := (fongMigration X).delta D
