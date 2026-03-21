/-
  Clip: clipboard copy support.
  Detects system clipboard tool, pipes string to it.
-/
import Tc.Cmd
import Tc.View

namespace Tc.Clip

variable {T : Type} [TblOps T]

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

-- | Run clipboard effect: fetch data and copy to system clipboard
def run (s : ViewStack T) (ce : ClipEffect) : IO (ViewStack T) := do
  let n := s.cur.nav
  let names := TblOps.colNames n.tbl
  let curCol := n.curColIdx
  let text ← match ce with
    | .cell => TblOps.cellStr n.tbl n.row.cur.val curCol
    | .row => do
      let cols ← TblOps.getCols n.tbl (Array.range names.size) n.row.cur.val (n.row.cur.val + 1)
      pure ("\t".intercalate (cols.map fun c => (c.get 0).toRaw).toList)
    | .col => do
      let nr := TblOps.nRows n.tbl
      let cols ← TblOps.getCols n.tbl #[curCol] 0 nr
      let col := cols.getD 0 default
      pure ("\n".intercalate (List.range nr |>.map fun i => (col.get i).toRaw))
  let ok ← Clip.copyToClip text
  if ok then statusMsg s!"yanked {text.length} chars"
  else statusMsg "no clipboard tool found"
  pure s

-- | Pure update: map yank Cmd to clip Effect
def update (s : ViewStack T) (cmd : Cmd) : Option (ViewStack T × Effect) :=
  match cmd with
  | .yank .ent => some (s, .clip .cell)
  | .yank .inc => some (s, .clip .row)
  | .yank .dec => some (s, .clip .col)
  | _ => none

end Tc.Clip
