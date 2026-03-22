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
    -- Clear after re-init: fzf inline renders below termbox area, leaving residue
    if !inTmux then let _ ← Term.init; Term.clear; Term.present
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
-- Previewable </> verbs collapsed into one entry (cycled live via fzf <> socket binds).
-- Non-previewable </> keep separate entries. ! prefix marks previewable commands.
private def flatItems (vk : ViewKind) : Array String :=
  objMenu.foldl (fun acc (objKey, objLabel, mk) =>
    let verbs := verbsFor objKey vk
    let previewable := (mk .inc).isPreviewable
    -- Previewable <> objects: single collapsed entry, cycled live via socket
    let acc := if previewable && verbs.any (fun (k, _, _) => k == '<' || k == '>') then
      acc.push s!"!{mk .inc}\t{objLabel}"
    else acc
    verbs.foldl (fun acc (verbKey, verbLabel, verb) =>
      -- Skip <> for previewable objects (already collapsed above)
      if (verbKey == '<' || verbKey == '>') && previewable then acc
      else
        let cmd := mk verb
        acc.push s!"{cmd}\t{objLabel} {verbLabel}"
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
  -- Helper script: extract cmd prefix from fzf item, send via socket if previewable.
  -- Written to file because fzf --tmux corrupts special shell chars in execute-silent.
  let tmp := (← IO.getEnv "TMPDIR").getD "/tmp"
  let script := tmp ++ "/tv-fzf-send.sh"
  let sockBinds ← if sockPath.isEmpty then pure #[]
    else do
      let lines := [
        "#!/bin/sh",
        "cmd=\"${1%%\t*}\"",  -- real tab: strip label after \t
        "case \"$cmd\" in '!'*) ;; *) exit 0;; esac",
        "code=\"${cmd#!}\"",
        "if [ \"$2\" = f ]; then printf '%s\\n' \"$code\"",
        "else printf '%s\\n' \"${code%?}$2\"",
        "fi | socat - UNIX-CONNECT:" ++ sockPath ++ " 2>/dev/null"
      ]
      IO.FS.writeFile script (String.intercalate "\n" lines ++ "\n")
      let _ ← IO.Process.output { cmd := "chmod", args := #["+x", script] }
      let bind (key dir : String) := "--bind=" ++ key ++ ":execute-silent(" ++ script ++ " {} " ++ dir ++ ")"
      pure #[bind "focus" "f", bind "left" "-", bind "right" "+", "--header=← dec  → inc"]
  let opts := #["--with-nth=2..", "--prompt=cmd "] ++ sockBinds
  let out ← fzfCore opts input poll
  if out.isEmpty then return none
  pure (parseFlatSel out)

end Tc.Fzf
