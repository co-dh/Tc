/-
  Shared test utilities used by Test.lean, TestLargeData.lean, and TestScreen.lean
-/

namespace TestUtil

def bin := ".lake/build/bin/tc"

def log (msg : String) : IO Unit := do
  let h ← IO.FS.Handle.mk "test.log" .append
  h.putStrLn msg; h.flush

def run (keys : String) (file : String := "") : IO String := do
  log s!"  run: {file} keys={keys}"
  -- I toggles info overlay off so it doesn't pollute header/dataLines helpers
  let keys' := "I" ++ keys
  let args := if file.isEmpty then #["-c", keys'] else #[file, "-c", keys']
  let out ← IO.Process.output { cmd := bin, args }
  if !out.stderr.isEmpty then log s!"  stderr: {out.stderr.trimAscii.toString}"
  if out.exitCode != 0 then log s!"  exit: {out.exitCode}"
  log "  done"
  pure out.stdout

def isContent (l : String) : Bool := l.any fun c => c.isAlpha || c.isDigit
def contains (s sub : String) : Bool := (s.splitOn sub).length > 1

def footer (output : String) : String × String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  (lines.getD (n - 2) "", lines.getD (n - 1) "")

def header (output : String) : String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let hdr := lines.headD ""
  if hdr.length > 80 then (hdr.drop (hdr.length - 80)).toString else hdr

def dataLines (output : String) : List String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  lines.drop 1 |>.take (n - 3)

def assert (cond : Bool) (msg : String) : IO Unit :=
  unless cond do throw (IO.userError msg)

def hasFile (path : String) : IO Bool :=
  try let _ ← IO.FS.Handle.mk path .read; pure true
  catch _ => pure false

end TestUtil
