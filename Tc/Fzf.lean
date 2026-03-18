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
-- Uses --tmux popup if in tmux (keeps table visible), otherwise compact at bottom
def fzfCore (opts : Array String) (input : String) : IO String := do
  if ← getTestMode then
    pure (input.splitOn "\n" |>.filter (!·.isEmpty) |>.headD "")
  else
    let inTmux := (← IO.getEnv "TMUX").isSome
    let baseArgs := if inTmux
      then #["--tmux=bottom,80%,40%", "--layout=reverse"]  -- compact popup at bottom
      else #["--height=~15", "--layout=reverse"]            -- compact inline at bottom
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

-- | Build flat menu items: "{!?}{objChar}{verbChar}\t{objName} > {verbLabel}"
-- ! prefix marks previewable commands (theme, heat, width, prec)
private def flatItems (vk : ViewKind) : Array String :=
  objMenu.foldl (fun acc (objKey, objLabel, mk) =>
    let objName := ((objLabel.splitOn ":").headD objLabel).trimAscii.toString
    (verbsFor objKey vk).foldl (fun acc (_, verbLabel, verb) =>
      let cmd := mk verb
      let pfx := if cmd.isPreviewable then "!" else ""
      acc.push s!"{pfx}{cmd}\t{objName} > {verbLabel}"
    ) acc
  ) #[]

-- | Parse flat selection: strip ! prefix, extract 2-char Cmd code before \t
def parseFlatSel (sel : String) : Option Cmd :=
  let cmdStr := (sel.splitOn "\t").headD ""
  let cmdStr := if cmdStr.startsWith "!" then (cmdStr.drop 1).toString else cmdStr
  (Parse.parse? cmdStr : Option Cmd)

-- | Command mode: space → flat fzf menu → return Cmd
-- vk = current view kind, for context-sensitive verb filtering
def cmdMode (vk : ViewKind) : IO (Option Cmd) := do
  let items := flatItems vk
  if items.isEmpty then return none
  let input := "\n".intercalate items.toList
  -- Build fzf args: hide cmd prefix, show only label
  let sockPath := (← IO.getEnv "TV_SOCK").getD ""
  let previewBind := if sockPath.isEmpty then #[]
    else
      -- fzf focus bind: extract 2-char cmd prefix, send previewable (!) commands via socket
      let bind := "--bind=focus:execute-silent(line={};cmd=${line%%\\t*};[ \"${cmd:0:1}\" = \"!\" ]&&printf '%s\\n' \"${cmd:1:2}\"|socat - UNIX-CONNECT:" ++ sockPath ++ " 2>/dev/null)"
      #[bind]
  let some sel ← fzf (#["--with-nth=2..", "--prompt=cmd "] ++ previewBind) input | return none
  pure (parseFlatSel sel)

end Tc.Fzf
