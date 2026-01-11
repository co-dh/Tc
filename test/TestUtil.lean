/-
  Test utilities shared between core and ADBC tests
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

-- | Run tc with keys, optional file (uses tmux send-keys + capture-pane)
def run (bin : String) (keys : String) (file : String := "") : IO String := do
  let f := if file.isEmpty then "" else s!"\"{file}\" "
  log s!"  spawn: {file} keys={keys}"
  let sess := "tctest"
  let _ ← IO.Process.output { cmd := "tmux", args := #["kill-session", "-t", sess] }
  let _ ← IO.Process.output { cmd := "tmux", args := #["new-session", "-d", "-s", sess, "-x", "80", "-y", "24", "-e", "TC_TEST_MODE=1", s!"{bin} {f}"] }
  let (t1, t2, t3) := if (file.splitOn "1.parquet").length > 1 then (500, 150, 300) else (150, 50, 100)
  IO.sleep t1
  for ka in keysToTmux keys do
    let _ ← IO.Process.output { cmd := "tmux", args := #["send-keys", "-t", sess] ++ ka }
    IO.sleep t2
  IO.sleep t3
  let out ← IO.Process.output { cmd := "tmux", args := #["capture-pane", "-t", sess, "-p"] }
  let _ ← IO.Process.output { cmd := "tmux", args := #["kill-session", "-t", sess] }
  log "  done"
  pure out.stdout

-- | Check if line has content
def isContent (l : String) : Bool := l.any fun c => c.isAlpha || c.isDigit

-- | Check string contains substring
def contains (s sub : String) : Bool := (s.splitOn sub).length > 1

-- | Extract footer: (tab line, status line)
def footer (output : String) : String × String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  (lines.getD (n - 2) "", lines.getD (n - 1) "")

-- | Extract header row
def header (output : String) : String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let hdr := lines.headD ""
  if hdr.length > 80 then hdr.drop (hdr.length - 80) else hdr

-- | Get data lines (skip header, skip footer 2 lines)
def dataLines (output : String) : List String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  lines.drop 1 |>.take (n - 3)

-- | Assert with message
def assert (cond : Bool) (msg : String) : IO Unit :=
  unless cond do throw (IO.userError msg)

end Test
