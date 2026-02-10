/-
  Per-process temporary directory.
  Each tc process gets /tmp/tc-XXXXXX/ via mktemp -d, avoiding collisions
  when multiple instances run concurrently.
-/

namespace Tc

initialize tmpDir : IO.Ref String ← do
  let r ← IO.Process.output { cmd := "mktemp", args := #["-d", "/tmp/tc-XXXXXX"] }
  IO.mkRef r.stdout.trimAscii.toString

def tmpPath (name : String) : IO String :=
  return s!"{← tmpDir.get}/{name}"

def cleanupTmp : IO Unit := do
  let dir ← tmpDir.get
  let _ ← IO.Process.output { cmd := "rm", args := #["-rf", dir] }

end Tc
