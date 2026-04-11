/-
  Minimal lens library: concrete get/set encoding (not van Laarhoven).
  Solves nested record updates like `{ s with a := { s.a with b := v } }`.

  Usage:
    (outerL ∘ₗ innerL).get s       -- read nested field
    (outerL ∘ₗ innerL).set v s     -- write nested field
    (outerL ∘ₗ innerL).modify f s  -- apply f to nested field

  Field lenses: define next to the struct via `fieldL%`
    def NavAxis.curL  [BEq elem] : Lens' (NavAxis n elem) (Fin n)       := fieldL% cur
    def NavAxis.selsL [BEq elem] : Lens' (NavAxis n elem) (Array elem)  := fieldL% sels
-/
import Lean

namespace Tc

/-- Concrete lens: getter + setter pair.
    Simpler and more Lean-friendly than van Laarhoven (no Functor/Applicative needed). -/
structure Lens' (S A : Type u) where
  get : S → A
  set : A → S → S

namespace Lens'
variable {S A B : Type u}

/-- Update a focused field by applying `f`. -/
@[inline] def modify (l : Lens' S A) (f : A → A) (s : S) : S :=
  l.set (f (l.get s)) s

/-- Compose lenses outer → inner: `(outerL ∘ₗ innerL).get s = innerL.get (outerL.get s)`. -/
@[inline] def comp (l : Lens' S A) (m : Lens' A B) : Lens' S B where
  get s   := m.get (l.get s)
  set b s := l.set (m.set b (l.get s)) s

end Lens'

/-- Lens composition infix. `rowL ∘ₗ curL : Lens' NavState (Fin n)`. -/
infixr:90 " ∘ₗ " => Lens'.comp

/-- Build a field lens: `fieldL% cur` expands to a lens reading/writing the `cur` field.
    The target type is taken from the expected type on the LHS (def signature). -/
macro "fieldL% " f:ident : term =>
  `({ get := fun s => s.$f, set := fun a s => { s with $f:ident := a } })

-- | `gen_lenses StructType where f1, f2, f3` generates one field lens per field:
--
--     gen_lenses (View T) where path, prec, widths
--     -- emits:
--     --   def pathL  := ({ get := fun s => s.path, ... } : Lens' (View T) _)
--     --   def precL  := ...
--     --   def widthsL := ...
--
-- Each generated def uses body-side ascription to `Lens' StructType _`, so the codomain
-- is inferred from the field type. Type parameters used in `StructType` (e.g. `T`, `[TblOps T]`)
-- must be in scope via a surrounding `variable` block — they are auto-bound by Lean.
open Lean Elab Command

elab "gen_lenses " src:term " where " fields:ident,+ : command => do
  for field in fields.getElems do
    let lensName := Lean.mkIdent (field.getId.appendAfter "L")
    let cmd ← `(command| def $lensName :=
      (({ get := fun (s : $src) => s.$field,
          set := fun a s => { s with $field:ident := a } }) : Lens' $src _))
    elabCommand cmd

end Tc
