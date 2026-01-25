/-
  Shared test utilities for Tc tests
-/
namespace Test

-- | Log to file
def log (msg : String) : IO Unit := do
  let h ← IO.FS.Handle.mk "test.log" .append
  h.putStrLn msg; h.flush

-- | Convert key notation to tmux args: "<C-d>" → ["C-d"], "abc" → ["-l", "abc"]
def keysToTmux (keys : String) : Array (Array String) := Id.run do
  let mut result : Array (Array String) := #[]
  let mut buf := ""
  let chars := keys.toList.toArray
  let mut i := 0
  while i < chars.size do
    let c := chars.getD i ' '
    if c == '<' then
      if !buf.isEmpty then result := result.push #["-l", buf]; buf := ""
      -- find closing >
      let mut j := i + 1
      while j < chars.size && chars.getD j ' ' != '>' do j := j + 1
      let tag := String.ofList (chars.toList.drop (i + 1) |>.take (j - i - 1))
      let tmuxKey := if tag.startsWith "C-" then tag
                     else if tag == "ret" then "Enter"
                     else tag
      result := result.push #[tmuxKey]
      i := j + 1
    else
      buf := buf.push c; i := i + 1
  if !buf.isEmpty then result := result.push #["-l", buf]
  result

-- | Run tc variant with keys, optional file (uses tmux send-keys + capture-pane)
-- finalWait: extra ms to wait after all keys (for slow queries like freq on large files)
def runWith (bin keys file : String) (slow : Bool := false) (finalWait : UInt32 := 0) : IO String := do
  let f := if file.isEmpty then "" else s!"\"{file}\" "
  log s!"  spawn: {file} keys={keys}"
  let sess := "tctest"
  -- kill stale session
  let _ ← IO.Process.output { cmd := "tmux", args := #["kill-session", "-t", sess] }
  -- start tc in tmux with TC_TEST_MODE for fzf auto-select
  let _ ← IO.Process.output { cmd := "tmux", args := #["new-session", "-d", "-s", sess, "-x", "80", "-y", "24", "-e", "TC_TEST_MODE=1", s!"{bin} {f}"] }
  let (t1, t2, t3) := if slow then (500, 150, 300) else (150, 50, 100)
  IO.sleep t1
  -- send keys
  for ka in keysToTmux keys do
    let _ ← IO.Process.output { cmd := "tmux", args := #["send-keys", "-t", sess] ++ ka }
    IO.sleep t2
  IO.sleep (t3 + finalWait)
  -- capture screen
  let out ← IO.Process.output { cmd := "tmux", args := #["capture-pane", "-t", sess, "-p"] }
  let _ ← IO.Process.output { cmd := "tmux", args := #["kill-session", "-t", sess] }
  log "  done"
  pure out.stdout

-- | Run with specified binary
def runBin (bin keys : String) (file : String := "") (finalWait : UInt32 := 0) : IO String :=
  let slow := (file.splitOn "1.parquet").length > 1
  runWith bin keys file slow finalWait

-- | Run tc (full) with keys
def run (keys : String) (file : String := "") (finalWait : UInt32 := 0) : IO String :=
  runBin ".lake/build/bin/tc" keys file finalWait

-- | Aliases for backward compat
def runKeys (keys file : String) (finalWait : UInt32 := 0) : IO String := run keys file finalWait
def runFolder (keys : String) : IO String := run keys

-- | Check if line has content
def isContent (l : String) : Bool := l.any fun c => c.isAlpha || c.isDigit

-- | Check string contains substring
def contains (s sub : String) : Bool := (s.splitOn sub).length > 1

-- | Extract footer: (tab line, status line) - last two content lines
def footer (output : String) : String × String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  (lines.getD (n - 2) "", lines.getD (n - 1) "")

-- | Extract header row: last 80 chars of first content line
def header (output : String) : String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let hdr := lines.headD ""
  if hdr.length > 80 then (hdr.drop (hdr.length - 80)).toString else hdr

-- | Get data lines (skip header, skip footer 2 lines)
def dataLines (output : String) : List String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  lines.drop 1 |>.take (n - 3)

-- | Assert with message (silent on success)
def assert (cond : Bool) (msg : String) : IO Unit :=
  unless cond do throw (IO.userError msg)

-- | Check string ends with substring
def endsWith (s sub : String) : Bool := s.endsWith sub

end Test
