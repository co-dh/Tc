/-
  StrEnum: deriving handler for simple enums where toString = constructor name.
  Usage: `inductive Foo | bar | baz deriving StrEnum`
  Generates: toString, all, ofString?
-/
import Std.Data.HashMap
import Lean.Elab.Deriving

class StrEnum (α : Type) extends ToString α where
  all : Array α
  ofString? : String → Option α

open Lean Elab Command Parser in
initialize registerDerivingHandler ``StrEnum fun typeNames => do
  let nm := typeNames[0]!
  let env ← getEnv
  let some (.inductInfo iv) := env.find? nm | return false
  let ctors := iv.ctors.map fun c => (env.find? c).get!.name.getString!
  -- use short name — deriving runs in the same namespace as the type
  let n := nm.getString!
  let elabStr (s : String) : CommandElabM Unit := do
    match runParserCategory (← getEnv) `command s with
    | .ok stx => elabCommand stx | .error e => throwError e
  let arms := ctors.map (fun c => s!"  | .{c} => \"{c}\"") |> "\n".intercalate
  elabStr s!"def {n}.strEnumToStr : {n} → String\n{arms}"
  let items := ctors.map (fun c => s!".{c}") |> ", ".intercalate
  elabStr s!"def {n}.strEnumAll : Array {n} := #[{items}]"
  elabStr s!"private def {n}.strEnumMap : Std.HashMap String {n} := {n}.strEnumAll.foldl (init := \{}) fun m c => m.insert c.strEnumToStr c"
  elabStr s!"instance : StrEnum {n} where\n  toString := {n}.strEnumToStr\n  all := {n}.strEnumAll\n  ofString? s := {n}.strEnumMap.get? s"
  return true
