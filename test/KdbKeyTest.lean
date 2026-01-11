/-
  Kdb key tests - UI tests for kdb backend
  Requires: kdb server at localhost:8888 with nbbo table
  Run: lake build kdb-key-test && .lake/build/bin/kdb-key-test
-/

namespace KdbKeyTest

-- | Log to file
def log (msg : String) : IO Unit := do
  let h ← IO.FS.Handle.mk "kdb-key-test.log" .append
  h.putStrLn msg
  h.flush

-- | Run tc with -c keys against kdb
def run (keys : String) : IO String := do
  log s!"  keys={keys}"
  let child ← IO.Process.spawn {
    cmd := "bash"
    args := #["-c", s!"script -q -c 'stty rows 24 cols 80; .lake/build/bin/tc \"kdb://localhost:8888/nbbo\" -c \"{keys}\"' /dev/null | ansi2txt | tail -24"]
    stdin := .null
    stdout := .piped
    stderr := .piped
    env := #[("TERM", "xterm")]
  }
  let stdout ← child.stdout.readToEnd
  let _ ← child.wait
  log "  done"
  pure stdout

-- | Check line has content
def isContent (l : String) : Bool := l.any fun c => c.isAlpha || c.isDigit

-- | Check contains substring
def contains (s sub : String) : Bool := (s.splitOn sub).length > 1

-- | Extract (tab, status) from output
def footer (output : String) : String × String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  (lines.getD (n - 2) "", lines.getD (n - 1) "")

-- | Extract header row
def header (output : String) : String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let hdr := lines.headD ""
  if hdr.length > 80 then hdr.drop (hdr.length - 80) else hdr

-- | Assert with message
def assert (cond : Bool) (msg : String) : IO Unit := do
  if cond then IO.println s!"✓ {msg}"
  else throw (IO.userError s!"✗ {msg}")

-- | Check kdb server available
def kdbAvail : IO Bool := do
  try
    let out ← IO.Process.output { cmd := "./qcon", args := #["localhost:8888"], stdin := .piped }
    pure (out.exitCode == 0)
  catch _ => pure false

-- === Tests ===

def test_load : IO Unit := do
  log "load"
  let o ← run ""
  let (_, st) := footer o
  assert (contains st "r0/") "load shows rows"
  assert (!contains st "Error") "no error"

def test_nav_down : IO Unit := do
  log "nav_down"
  let o ← run "j"
  let (_, st) := footer o
  assert (contains st "r1/") "j moves down"

def test_nav_right : IO Unit := do
  log "nav_right"
  let o ← run "l"
  let (_, st) := footer o
  assert (contains st "c1/") "l moves right"

def test_page_down : IO Unit := do
  log "page_down"
  let o ← run "<C-d>"
  let (_, st) := footer o
  assert (!contains st "r0/") "^D pages down"

def test_sort_asc : IO Unit := do
  log "sort_asc"
  let o ← run "["
  let (_, st) := footer o
  assert (contains st "r0/") "[ sorts asc"

def test_sort_desc : IO Unit := do
  log "sort_desc"
  let o ← run "]"
  let (_, st) := footer o
  assert (contains st "r0/") "] sorts desc"

def test_key_col : IO Unit := do
  log "key_col"
  let o ← run "!"
  let h := header o
  assert (contains h "║") "! adds key separator"

def test_key_remove : IO Unit := do
  log "key_remove"
  let o ← run "!!"
  let h := header o
  assert (!contains h "║") "!! removes key"

def test_del_col : IO Unit := do
  log "del_col"
  let b ← run ""
  let a ← run "d"
  assert (header b != header a) "d deletes column"

def test_meta : IO Unit := do
  log "meta"
  let o ← run "M"
  let (tab, _) := footer o
  assert (contains tab "meta") "M shows meta"

def test_meta_quit : IO Unit := do
  log "meta_quit"
  let o ← run "Mq"
  let (tab, _) := footer o
  assert (!contains tab "meta") "Mq returns"

def test_freq : IO Unit := do
  log "freq"
  let o ← run "F"
  let (tab, _) := footer o
  assert (contains tab "freq") "F shows freq"

def test_freq_quit : IO Unit := do
  log "freq_quit"
  let o ← run "Fq"
  let (tab, _) := footer o
  assert (!contains tab "freq") "Fq returns"

def test_stack_swap : IO Unit := do
  log "stack_swap"
  let o ← run "S"
  let (tab, _) := footer o
  assert (contains tab "nbbo") "S swaps view"

-- | Run all tests
def main : IO Unit := do
  IO.FS.writeFile "kdb-key-test.log" ""
  let avail ← kdbAvail
  if !avail then
    IO.println "✗ kdb server unavailable at localhost:8888"
    IO.Process.exit 1
  IO.println "Running kdb key tests...\n"
  test_load
  test_nav_down
  test_nav_right
  test_page_down
  test_sort_asc
  test_sort_desc
  test_key_col
  test_key_remove
  test_del_col
  test_meta
  test_meta_quit
  test_freq
  test_freq_quit
  test_stack_swap
  IO.println "\nAll kdb key tests passed!"

end KdbKeyTest

def main : IO Unit := KdbKeyTest.main
