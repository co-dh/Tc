/-
  Validity: which commands are valid for which ViewKind.

  `validFor cmd vk` returns true when `cmd` can be meaningfully handled
  in a view of kind `vk`. Most commands are universal (navigation, stack,
  theme, info, precision, width, filter/search). Only three view-specific
  enter commands require a particular ViewKind.

  This is a specification that can be checked against the dispatch chain;
  theorems below prove key properties.
-/
import Tc.Cmd
import Tc.Types

namespace Tc

/-- ViewKind equality ignoring runtime parameters (cols, total, path, depth) -/
def ViewKind.sameShape : ViewKind → ViewKind → Bool
  | .tbl, .tbl => true
  | .freqV _ _, .freqV _ _ => true
  | .colMeta, .colMeta => true
  | .fld _ _, .fld _ _ => true
  | _, _ => false

/-- Is this command ViewKind-specific? Only three enter commands
    require a particular ViewKind; everything else is universal
    (handled by navigation, stack, theme, filter, or is a harmless no-op). -/
def cmdViewSpecific : Cmd → Bool
  | .freq .ent  => true   -- Enter in freq view: filter by row
  | .metaV .ent => true   -- Enter in meta view: set key columns
  | .fld .ent   => true   -- Enter in folder view: open dir/file
  | _ => false

/-- For view-specific commands, the required ViewKind shape.
    Runtime parameters (cols, total, path, depth) use placeholder values. -/
def cmdRequiredVK : Cmd → Option ViewKind
  | .freq .ent  => some (.freqV #[] 0)
  | .metaV .ent => some .colMeta
  | .fld .ent   => some (.fld "" 0)
  | _ => none

/-- A command is valid for a ViewKind if it is not view-specific,
    or if the ViewKind matches the required shape. -/
def validFor (cmd : Cmd) (vk : ViewKind) : Bool :=
  if cmdViewSpecific cmd then
    match cmdRequiredVK cmd with
    | some req => ViewKind.sameShape req vk
    | none => true   -- unreachable: viewSpecific implies requiredVK is some
  else true

/-! ## Theorems -/

/-- Non-view-specific commands are valid for every ViewKind -/
theorem universal_validFor (cmd : Cmd) (vk : ViewKind)
    (h : cmdViewSpecific cmd = false) : validFor cmd vk = true := by
  simp [validFor, h]

/-- Navigation commands (row) are valid everywhere -/
theorem row_always_valid (v : Verb) (vk : ViewKind) :
    validFor (.row v) vk = true := by
  cases v <;> simp [validFor, cmdViewSpecific]

/-- vPage commands are valid everywhere -/
theorem vPage_always_valid (v : Verb) (vk : ViewKind) :
    validFor (.vPage v) vk = true := by
  cases v <;> simp [validFor, cmdViewSpecific]

/-- hPage commands are valid everywhere -/
theorem hPage_always_valid (v : Verb) (vk : ViewKind) :
    validFor (.hPage v) vk = true := by
  cases v <;> simp [validFor, cmdViewSpecific]

/-- Stack commands are valid everywhere (q to quit, S to swap, etc.) -/
theorem stk_always_valid (v : Verb) (vk : ViewKind) :
    validFor (.stk v) vk = true := by
  cases v <;> simp [validFor, cmdViewSpecific]

/-- Theme commands are valid everywhere -/
theorem thm_always_valid (v : Verb) (vk : ViewKind) :
    validFor (.thm v) vk = true := by
  cases v <;> simp [validFor, cmdViewSpecific]

/-- Info commands are valid everywhere -/
theorem info_always_valid (v : Verb) (vk : ViewKind) :
    validFor (.info v) vk = true := by
  cases v <;> simp [validFor, cmdViewSpecific]

/-- freq.ent (Enter in freq view) is valid for freqV -/
theorem freq_ent_valid_freqV (cols : Array String) (total : Nat) :
    validFor (.freq .ent) (.freqV cols total) = true := by
  simp [validFor, cmdViewSpecific, cmdRequiredVK, ViewKind.sameShape]

theorem freq_ent_not_tbl : validFor (.freq .ent) .tbl = false := by
  simp [validFor, cmdViewSpecific, cmdRequiredVK, ViewKind.sameShape]

theorem freq_ent_not_colMeta : validFor (.freq .ent) .colMeta = false := by
  simp [validFor, cmdViewSpecific, cmdRequiredVK, ViewKind.sameShape]

theorem freq_ent_not_fld (p : String) (d : Nat) :
    validFor (.freq .ent) (.fld p d) = false := by
  simp [validFor, cmdViewSpecific, cmdRequiredVK, ViewKind.sameShape]

/-- metaV.ent (Enter in meta view) is valid only for colMeta -/
theorem meta_ent_valid_colMeta :
    validFor (.metaV .ent) .colMeta = true := by
  simp [validFor, cmdViewSpecific, cmdRequiredVK, ViewKind.sameShape]

theorem meta_ent_not_tbl : validFor (.metaV .ent) .tbl = false := by
  simp [validFor, cmdViewSpecific, cmdRequiredVK, ViewKind.sameShape]

/-- fld.ent (Enter in folder view) is valid for fld -/
theorem fld_ent_valid_fld (p : String) (d : Nat) :
    validFor (.fld .ent) (.fld p d) = true := by
  simp [validFor, cmdViewSpecific, cmdRequiredVK, ViewKind.sameShape]

theorem fld_ent_not_tbl : validFor (.fld .ent) .tbl = false := by
  simp [validFor, cmdViewSpecific, cmdRequiredVK, ViewKind.sameShape]

/-- Every Cmd is valid for at least one ViewKind (no dead commands) -/
theorem no_dead_cmd (cmd : Cmd) : ∃ vk : ViewKind, validFor cmd vk = true := by
  cases cmd with
  | freq v =>
    cases v with
    | ent => exact ⟨.freqV #[] 0, by simp [validFor, cmdViewSpecific, cmdRequiredVK, ViewKind.sameShape]⟩
    | _ => exact ⟨.tbl, by simp [validFor, cmdViewSpecific]⟩
  | metaV v =>
    cases v with
    | ent => exact ⟨.colMeta, by simp [validFor, cmdViewSpecific, cmdRequiredVK, ViewKind.sameShape]⟩
    | _ => exact ⟨.tbl, by simp [validFor, cmdViewSpecific]⟩
  | fld v =>
    cases v with
    | ent => exact ⟨.fld "" 0, by simp [validFor, cmdViewSpecific, cmdRequiredVK, ViewKind.sameShape]⟩
    | _ => exact ⟨.tbl, by simp [validFor, cmdViewSpecific]⟩
  | _ => exact ⟨.tbl, by cases ‹Verb› <;> simp [validFor, cmdViewSpecific]⟩

/-- The three view-specific commands are exactly freq.ent, metaV.ent, fld.ent -/
theorem viewSpecific_iff_ent (cmd : Cmd) :
    cmdViewSpecific cmd = true ↔ (cmd = .freq .ent ∨ cmd = .metaV .ent ∨ cmd = .fld .ent) := by
  constructor
  · intro h; cases cmd with
    | freq v => cases v <;> simp_all [cmdViewSpecific]
    | metaV v => cases v <;> simp_all [cmdViewSpecific]
    | fld v => cases v <;> simp_all [cmdViewSpecific]
    | _ => cases ‹Verb› <;> simp_all [cmdViewSpecific]
  · intro h; rcases h with rfl | rfl | rfl <;> simp [cmdViewSpecific]

/-- sameShape is reflexive -/
theorem ViewKind.sameShape_refl (vk : ViewKind) : vk.sameShape vk = true := by
  cases vk <;> simp [ViewKind.sameShape]

/-- sameShape is symmetric -/
theorem ViewKind.sameShape_symm (a b : ViewKind) :
    a.sameShape b = b.sameShape a := by
  cases a <;> cases b <;> simp [ViewKind.sameShape]

end Tc
