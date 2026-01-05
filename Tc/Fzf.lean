/-
  Fzf: helpers for running fzf picker
  Suspends terminal, spawns fzf, returns selection
  In testMode, returns first value without spawning fzf
-/
import Tc.Key
import Tc.Term

open Tc

namespace Tc.Fzf

-- | Global testMode flag (set by App.main)
initialize testMode : IO.Ref Bool ← IO.mkRef false

-- | Set testMode
def setTestMode (b : Bool) : IO Unit := testMode.set b

-- | Get testMode
def getTestMode : IO Bool := testMode.get

-- | Core fzf: testMode returns first line, else spawn fzf
-- Uses --tmux popup if in tmux (keeps table visible), otherwise fullscreen
def fzfCore (opts : Array String) (input : String) : IO String := do
  if ← getTestMode then
    pure (input.splitOn "\n" |>.filter (!·.isEmpty) |>.headD "")
  else
    let inTmux := (← IO.getEnv "TMUX").isSome
    let baseArgs := if inTmux
      then #["--tmux=center,50%,50%", "--layout=reverse"]  -- popup over table
      else #["--height=100%", "--layout=reverse"]           -- fullscreen fallback
    if !inTmux then Term.shutdown
    let child ← IO.Process.spawn { cmd := "fzf", args := baseArgs ++ opts, stdin := .piped, stdout := .piped }
    child.stdin.putStr input
    child.stdin.flush
    let (_, child') ← child.takeStdin
    let out ← child'.stdout.readToEnd
    let _ ← child'.wait
    if !inTmux then let _ ← Term.init; pure ()
    pure out.trim

-- | Single select: returns none if empty/cancelled
def fzf (opts : Array String) (input : String) : IO (Option String) := do
  let out ← fzfCore opts input
  pure (if out.isEmpty then none else some out)

-- | Index select: testMode returns 0
def fzfIdx (opts : Array String) (items : Array String) : IO (Option Nat) := do
  if ← getTestMode then pure (if items.isEmpty then none else some 0)
  else
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

-- | Show fzf menu for +/- prefix commands, returns selected Cmd
def prefixCmd (verb : Verb) : IO (Option Cmd) := do
  let prompt := if verb == .inc then "+" else "-"
  let items := prefixMenu.map fun (c, desc, _) => s!"{c}\t{desc}"
  let input := "\n".intercalate items.toList
  match ← fzf #["--with-nth=2..", s!"--prompt={prompt}"] input with
  | some sel =>
    let key := sel.toList.getD 0 ' '
    pure (prefixMenu.findSome? fun (c, _, mk) => if c == key then some (mk verb) else none)
  | none => pure none

end Tc.Fzf
