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
      else #["--height=50%", "--layout=reverse", "--exact", "+i"]             -- bottom half inline
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
      poll
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
    let out ← fzfCore (#["--with-nth=2.."] ++ opts) (numbered.joinWith "\n")
    if out.isEmpty then return none
    match out.splitOn "\t" |>.head? |>.bind String.toNat? with
    | some n => return some n
    | none => return none

-- | Build aligned menu items: "handler | ctx | key | label" with padding
private def flatItems (vk : ViewKind) : IO (Array String) := do
  let items ← CmdConfig.menuItems vk.ctxStr
  let (maxH, maxX, maxK) := items.foldl
    (fun (mh, mx, mk) (h, x, k, _) => (max mh h.length, max mx x.length, max mk k.length)) (0, 0, 0)
  return items.map fun (handler, ctx, key, label) =>
    let hp := handler ++ "".pushn ' ' (maxH - handler.length)
    let xp := ctx ++ "".pushn ' ' (maxX - ctx.length)
    let kp := key ++ "".pushn ' ' (maxK - key.length)
    s!"{hp} | {xp} | {kp} | {label}"

-- | Parse flat selection: extract handler name before first |
def parseFlatSel (sel : String) : Option String :=
  let h := ((sel.splitOn " | ").headD "").trimRight
  if h.isEmpty then none else some h

-- | Command mode: space → flat fzf menu → return handler name
-- poll: callback invoked while fzf popup is open (for external socket dispatch + re-render)
def cmdMode (vk : ViewKind) (poll : IO Unit := pure ()) : IO (Option String) := do
  let items ← flatItems vk
  if items.isEmpty then return none
  let input := items.joinWith "\n"
  let opts := #["--prompt=cmd "]
  let out ← fzfCore opts input poll
  if out.isEmpty then return none
  pure (parseFlatSel out)

end Tc.Fzf
