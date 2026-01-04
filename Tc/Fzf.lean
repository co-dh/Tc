/-
  Fzf: helpers for running fzf picker
  Suspends terminal, spawns fzf, returns selection
-/
import Tc.Term

namespace Tc.Fzf

-- | Core fzf: spawn fzf with opts, pipe input, return output
def fzfCore (opts : Array String) (input : String) : IO String := do
  Term.shutdown
  let args := #["--height=50%", "--layout=reverse"] ++ opts
  let child ← IO.Process.spawn { cmd := "fzf", args, stdin := .piped, stdout := .piped }
  child.stdin.putStr input
  child.stdin.flush
  let (_, child') ← child.takeStdin
  let out ← child'.stdout.readToEnd
  let _ ← child'.wait
  let _ ← Term.init
  pure out.trim

-- | Single select: returns none if empty/cancelled
def fzf (opts : Array String) (input : String) : IO (Option String) := do
  let out ← fzfCore opts input
  pure (if out.isEmpty then none else some out)

-- | Multi select: returns array of selections
def fzfMulti (opts : Array String) (input : String) : IO (Array String) := do
  let out ← fzfCore (#["-m"] ++ opts) input
  pure (out.splitOn "\n" |>.map String.trim |>.filter (!·.isEmpty) |>.toArray)

-- | Index select: numbered items, returns selected index
def fzfIdx (opts : Array String) (items : Array String) : IO (Option Nat) := do
  let numbered := items.mapIdx fun i s => s!"{i}\t{s}"
  let out ← fzfCore (#["--with-nth=2.."] ++ opts) ("\n".intercalate numbered.toList)
  if out.isEmpty then return none
  match out.splitOn "\t" |>.head? |>.bind String.toNat? with
  | some n => return some n
  | none => return none

-- | Check if string is numeric (for PRQL quoting)
def isNumeric (s : String) : Bool :=
  if s.isEmpty then false
  else
    let s' := if s.startsWith "-" then s.drop 1 else s
    let parts := s'.splitOn "."
    match parts with
    | [int] => int.all Char.isDigit && !int.isEmpty
    | [int, dec] => int.all Char.isDigit && dec.all Char.isDigit && !int.isEmpty
    | _ => false

-- | Quote value for PRQL: numeric unquoted, strings single-quoted
def quoteVal (v : String) : String :=
  if isNumeric v then v else s!"'{v}'"

-- | Build filter expression from fzf result
-- With --print-query: line 0 = query, lines 1+ = selections
def buildFilterExpr (col : String) (vals : Array String) (result : String) : String :=
  let lines := result.splitOn "\n" |>.filter (!·.isEmpty) |>.toArray
  let input := lines.getD 0 ""
  let fromHints := (lines.extract 1 lines.size).filter vals.contains
  let selected := if vals.contains input && !fromHints.contains input
                  then #[input] ++ fromHints else fromHints
  if selected.size == 1 then s!"{col} == {quoteVal (selected.getD 0 "")}"
  else if selected.size > 1 then "(" ++ " || ".intercalate (selected.map fun v => s!"{col} == {quoteVal v}").toList ++ ")"
  else if !input.isEmpty then
    -- custom expr: > 5, < 10, == 'x', ~= 'pat'
    if input.startsWith ">" || input.startsWith "<" || input.startsWith "=" || input.startsWith "~"
    then s!"{col} {input}" else input
  else ""

end Tc.Fzf
