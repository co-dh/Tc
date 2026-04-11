/-
  Fzf: helpers for running fzf picker.
  Suspends terminal, spawns fzf, returns selection.
  In test mode (passed explicitly), returns first value without spawning fzf.
-/
import Tc.CmdConfig
import Tc.Key
import Tc.Term


open Tc

namespace Tc.Fzf

-- | Core fzf: test=true returns first line; otherwise spawn fzf.
-- Uses --tmux popup if in tmux (keeps table visible), otherwise compact at bottom.
-- poll: optional callback invoked in loop while fzf runs (tmux only, for live socket dispatch).
def fzfCore (test : Bool) (opts : Array String) (input : String) (poll : IO Unit := pure ()) : IO String := do
  if test then
    pure (input.splitOn "\n" |>.filter (!·.isEmpty) |>.headD "")
  else
    let inTmux := (← IO.getEnv "TMUX").isSome
    let lines := input.splitOn "\n" |>.filter (!·.isEmpty)
    let popupH := min (lines.length + 2) 15  -- fit content, cap at 15
    -- measure visible width: strip hidden prefix when --with-nth hides leading fields
    let visLines := if opts.any (·.startsWith "--with-nth=2")
      then lines.map fun l => match l.splitOn "\t" with | _ :: rest => "\t".intercalate rest | _ => l
      else lines
    let maxW := visLines.foldl (fun m l => max m l.length) 0
    let popupW := min (max (maxW + 4) 50) 80  -- fit content, floor 50 for typing
    let baseArgs := if inTmux
      then #[s!"--tmux=bottom,{popupW},{popupH}", "--layout=reverse", "--exact", "+i"]
      else #[s!"--height={popupH + 1}", "--layout=reverse", "--exact", "+i"]
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
def fzf (test : Bool) (opts : Array String) (input : String) : IO (Option String) := do
  let out ← fzfCore test opts input
  pure (if out.isEmpty then none else some out)

-- | Index select: test=true returns 0
def fzfIdx (test : Bool) (opts : Array String) (items : Array String) : IO (Option Nat) := do
  if test then pure (if items.isEmpty then none else some 0)
  else
    let numbered := items.mapIdx fun i s => s!"{i}\t{s}"
    let out ← fzfCore test (#["--with-nth=2.."] ++ opts) (numbered.joinWith "\n")
    if out.isEmpty then return none
    match out.splitOn "\t" |>.head? |>.bind String.toNat? with
    | some n => return some n
    | none => return none

-- | Build aligned menu items: "handler | ctx | key | label" with padding
private def flatItems (cache : CmdConfig.Cache) (vk : ViewKind) : Array String :=
  let items := cache.menuItems vk.ctxStr
  let (maxH, maxX, maxK) := items.foldl
    (fun (mh, mx, mk) (h, x, k, _) => (max mh h.length, max mx x.length, max mk k.length)) (0, 0, 0)
  items.map fun (handler, ctx, key, label) =>
    let hp := handler ++ "".pushn ' ' (maxH - handler.length)
    let xp := ctx ++ "".pushn ' ' (maxX - ctx.length)
    let kp := key ++ "".pushn ' ' (maxK - key.length)
    s!"{hp} | {xp} | {kp} | {label}"

-- | Parse flat selection: extract handler name before first |
def parseFlatSel (sel : String) : Option String :=
  let h := ((sel.splitOn " | ").headD "").trimAsciiEnd.toString
  if h.isEmpty then none else some h

-- | Command mode: space → flat fzf menu → return handler name.
-- poll: callback invoked while fzf popup is open (for external socket dispatch + re-render).
def cmdMode (test : Bool) (cache : CmdConfig.Cache) (vk : ViewKind) (poll : IO Unit := pure ())
    : IO (Option String) := do
  let items := flatItems cache vk
  if items.isEmpty then return none
  let input := items.joinWith "\n"
  let opts := #["--prompt=cmd "]
  let out ← fzfCore test opts input poll
  if out.isEmpty then return none
  pure (parseFlatSel out)

end Tc.Fzf
