/-
  Clip: clipboard copy support (deprecated — not suitable for database use case).
  Kept for copyToClip utility used by statusMsg.
-/
import Tc.Cmd
import Tc.View

namespace Tc.Clip

-- | Cached clipboard command (detected once)
initialize clipCmd : IO.Ref (Option (String × Array String)) ← IO.mkRef none

-- | Clipboard tools in priority order: macOS → Wayland → X11
private def clipTools : Array (String × Array String) := #[
  ("pbcopy", #[]), ("wl-copy", #[]),
  ("xclip", #["-selection", "clipboard"]), ("xsel", #["--clipboard", "--input"])
]

-- | Detect first available clipboard tool
private def detectClip : IO (Option (String × Array String)) :=
  clipTools.findSomeM? fun tool => do
    let r ← IO.Process.output { cmd := "which", args := #[tool.1] }
    pure (if r.exitCode == 0 then some tool else none)

-- | Get clipboard command (cached)
private def getClip : IO (Option (String × Array String)) := do
  match ← clipCmd.get with
  | some cmd => return some cmd
  | none =>
    let cmd ← detectClip
    if let some c := cmd then clipCmd.set (some c)
    return cmd

-- | Copy string to system clipboard
def copyToClip (s : String) : IO Bool := do
  match ← getClip with
  | none => return false
  | some (cmd, args) =>
    let child ← IO.Process.spawn
      { cmd := cmd, args := args
        stdin := .piped, stdout := .null, stderr := .null }
    child.stdin.putStr s
    child.stdin.flush
    let (_, child) ← child.takeStdin
    let code ← child.wait
    return code == 0

end Tc.Clip
