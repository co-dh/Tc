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
-- poll: optional callback invoked in loop while fzf runs (tmux only, for live socket dispatch)
def fzfCore (opts : Array String) (input : String) (poll : IO Unit := pure ()) : IO String := do
  if ← getTestMode then
    pure (input.splitOn "\n" |>.filter (!·.isEmpty) |>.headD "")
  else
    let inTmux := (← IO.getEnv "TMUX").isSome
    let baseArgs := if inTmux
      then #["--tmux=bottom,80%,40%", "--layout=reverse", "--exact", "+i"]  -- compact popup at bottom
      else #["--height=~15", "--layout=reverse", "--exact", "+i"]            -- compact inline at bottom
    if !inTmux then Term.shutdown
    let child ← IO.Process.spawn { cmd := "fzf", args := baseArgs ++ opts, stdin := .piped, stdout := .piped }
    child.stdin.putStr input
    child.stdin.flush
    let (_, child') ← child.takeStdin
    -- Read stdout in background; poll socket while fzf popup is open
    let done ← IO.mkRef false
    let outRef ← IO.mkRef ""
    let _ ← IO.asTask (prio := .dedicated) do
      let out ← child'.stdout.readToEnd
      outRef.set out
      done.set true
    while !(← done.get) do
      if inTmux then poll
      IO.sleep 30
    let out ← outRef.get
    let _ ← child'.wait
    let _ ← Term.init
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

-- | Build flat menu items: "{!?}{objChar}{verbChar}\t{label}"
-- </> verbs collapsed into one entry per obj (cycled via fzf <> key binds).
-- Non-</> verbs are separate entries. ! prefix marks previewable commands.
private def flatItems (vk : ViewKind) : Array String :=
  objMenu.foldl (fun acc (objKey, objLabel, mk) =>
    let verbs := verbsFor objKey vk
    let hasLtGt := verbs.any (fun (k, _, _) => k == '<' || k == '>')
    let acc := if hasLtGt then
      let cmd := mk .inc
      let pfx := if cmd.isPreviewable then "!" else ""
      acc.push s!"{pfx}{cmd}\t{objLabel}"
    else acc
    verbs.foldl (fun acc (verbKey, verbLabel, verb) =>
      if verbKey == '<' || verbKey == '>' then acc
      else
        let cmd := mk verb
        let pfx := if cmd.isPreviewable then "!" else ""
        acc.push s!"{pfx}{cmd}\t{objLabel} {verbLabel}"
    ) acc
  ) #[]

-- | Parse flat selection: strip ! prefix, extract 2-char Cmd code before \t
def parseFlatSel (sel : String) : Option Cmd :=
  let cmdStr := (sel.splitOn "\t").headD ""
  let cmdStr := if cmdStr.startsWith "!" then (cmdStr.drop 1).toString else cmdStr
  (Parse.parse? cmdStr : Option Cmd)

-- | Command mode: space → flat fzf menu → return Cmd
-- poll: callback invoked while fzf popup is open (for live socket dispatch + re-render)
def cmdMode (vk : ViewKind) (poll : IO Unit := pure ()) : IO (Option Cmd) := do
  let items := flatItems vk
  if items.isEmpty then return none
  let input := "\n".intercalate items.toList
  let sockPath := (← IO.getEnv "TV_SOCK").getD ""
  let sockBinds := if sockPath.isEmpty then #[]
    else
      let sock := sockPath
      -- Shell template: extract cmd prefix, send to socket if previewable (! prefix)
      let mkBind (key expr : String) := "--bind=" ++ key ++ ":execute-silent(line={};cmd=${line%%\\t*};[ \"${cmd:0:1}\" = \"!\" ]&&printf '%s\\n' \"" ++ expr ++ "\"|socat - UNIX-CONNECT:" ++ sock ++ " 2>/dev/null)"
      #[mkBind "focus" "${cmd:1:2}", mkBind "<" "${cmd:1:1}-", mkBind ">" "${cmd:1:1}+", "--header=< dec  > inc"]
  let opts := #["--with-nth=2..", "--prompt=cmd "] ++ sockBinds
  let out ← fzfCore opts input poll
  if out.isEmpty then return none
  pure (parseFlatSel out)

end Tc.Fzf
