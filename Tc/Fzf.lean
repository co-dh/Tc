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
    pure out.trimAscii.toString

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

-- | Quote value for PRQL based on column type
def quoteVal (v : String) (numeric : Bool) : String :=
  if numeric then v else s!"'{v}'"

-- | Build filter expression from fzf result
-- With --print-query: line 0 = query, lines 1+ = selections
def buildFilterExpr (col : String) (vals : Array String) (result : String) (numeric : Bool) : String :=
  let lines := result.splitOn "\n" |>.filter (!·.isEmpty) |>.toArray
  let input := lines.getD 0 ""
  let fromHints := (lines.extract 1 lines.size).filter vals.contains
  let selected := if vals.contains input && !fromHints.contains input
                  then #[input] ++ fromHints else fromHints
  if selected.size == 1 then s!"{col} == {quoteVal (selected.getD 0 "") numeric}"
  else if selected.size > 1 then "(" ++ " || ".intercalate (selected.map fun v => s!"{col} == {quoteVal v numeric}").toList ++ ")"
  else if !input.isEmpty then input
  else ""

-- | Build fzf args for 1-char selection using jump mode
-- jump-labels shows keys as labels, load:jump enters jump mode when list ready, jump:accept selects
private def jumpArgs (keys : Array Char) : Array String :=
  let labels := String.ofList keys.toList
  #[s!"--jump-labels={labels}", "--bind=load:jump,jump:accept"]

-- | Extract first char from fzf selection (same as toList.getD 0)
def selKey (s : String) : Char := s.toList.getD 0 ' '

-- | Pure cmdMode: given fzf object/verb selections and view kind, compute resulting Cmd
-- Uses >>= (bind) for Kleisli composition: find obj → find verb → construct Cmd
def cmdModePure (objSel verbSel : String) (vk : ViewKind) : Option Cmd :=
  objMenu.find? (·.2.1 == objSel) >>= fun (objKey, _, mk) =>
    (verbsFor objKey vk).find? (·.2.1 == verbSel) |>.map fun (_, _, verb) => mk verb

-- | Command mode: space → select object → select verb → return Cmd
-- vk = current view kind, for context-sensitive verb filtering
def cmdMode (vk : ViewKind) : IO (Option Cmd) := do
  -- step 1: select object (1-char select via jump mode)
  let objKeys := objMenu.map (·.1)
  let objItems := objMenu.map fun (_, desc, _) => desc  -- no prefix, jump label shows key
  let objInput := "\n".intercalate objItems.toList
  let some objSel ← fzf (#["--prompt=obj "] ++ jumpArgs objKeys) objInput | return none
  -- find by matching description (jump label not in output)
  let some (objKey, _, mk) := objMenu.find? (·.2.1 == objSel) | return none
  -- step 2: select verb (1-char select via jump mode, context-sensitive per view)
  let verbs := verbsFor objKey vk
  if verbs.isEmpty then return none
  let verbKeys := verbs.map (·.1)
  let verbItems := verbs.map fun (_, desc, _) => desc  -- no prefix, jump label shows key
  let verbInput := "\n".intercalate verbItems.toList
  let some verbSel ← fzf (#["--prompt=verb "] ++ jumpArgs verbKeys) verbInput | return none
  let some (_, _, verb) := verbs.find? (·.2.1 == verbSel) | return none
  pure (some (mk verb))

end Tc.Fzf
