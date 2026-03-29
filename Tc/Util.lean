/-
  Util: small utility modules consolidated into one file.
  Contents: Log (error/debug logging), TmpDir (per-process temp dir),
  Socket (unix socket IPC), Remote (URI path ops).
-/

-- ============================================================================
-- Log: centralized error/debug logging
-- ============================================================================

namespace Log

-- | Log dir — ~/.cache/tv/, works from any cwd (including CI)
initialize logDir : IO.Ref String ← do
  let home := (← IO.getEnv "HOME").getD "/tmp"
  let dir := s!"{home}/.cache/tv"
  let _ ← IO.Process.output { cmd := "mkdir", args := #["-p", dir] }
  IO.mkRef dir

def dir : IO String := logDir.get
def path : IO String := return s!"{← dir}/tv.log"

@[extern "lean_set_log_path"]
opaque setLogPath : @& String → IO Unit

@[extern "lean_local_timestamp"]
opaque localTimestamp : IO String

-- | Format timestamp HH:MM:SS.mmm (local time)
def timestamp : IO String := localTimestamp

-- | Write log entry
def write (tag msg : String) : IO Unit := do
  let h ← IO.FS.Handle.mk (← path) .append
  h.putStrLn s!"[{←timestamp}] [{tag}] {msg}"

-- | Log error message
def error (msg : String) : IO Unit := write "error" msg

-- | Run command and log on failure; returns output
def run (tag cmd : String) (args : Array String) : IO IO.Process.Output := do
  let r ← IO.Process.output { cmd, args }
  if r.exitCode != 0 then
    write tag s!"{cmd} {" ".intercalate args.toList} → exit {r.exitCode}: {r.stderr.trimAscii.toString}"
  pure r

end Log

-- ============================================================================
-- TmpDir: per-process temporary directory
-- ============================================================================

namespace Tc

initialize tmpDir : IO.Ref String ← do
  let r ← IO.Process.output { cmd := "mktemp", args := #["-d", "/tmp/tv-XXXXXX"] }
  IO.mkRef r.stdout.trimAscii.toString

def tmpPath (name : String) : IO String :=
  return s!"{← tmpDir.get}/{name}"

def cleanupTmp : IO Unit := do
  let dir ← tmpDir.get
  let _ ← IO.Process.output { cmd := "rm", args := #["-rf", dir] }

end Tc

-- ============================================================================
-- Socket: unix socket command channel
-- ============================================================================

namespace Socket

@[extern "lean_sock_start"]
private opaque sockStart : @& String → IO Bool

@[extern "lean_sock_poll_cmd"]
private opaque sockPollCmd : IO String

@[extern "lean_sock_close"]
private opaque sockClose : IO Unit

@[extern "lean_setenv"]
private opaque setEnv : @& String → @& String → IO Unit

@[extern "lean_getpid"]
private opaque getPid : IO Nat

-- | Global socket path
initialize sockPath : IO.Ref String ← IO.mkRef ""

-- | Start socket listener, set TV_SOCK env var
def init : IO Unit := do
  let tmp := (← IO.getEnv "TMPDIR").getD "/tmp"
  let pid ← getPid
  let path := s!"{tmp}/tv-{pid}.sock"
  let ok ← sockStart path
  if ok then
    sockPath.set path
    setEnv "TV_SOCK" path
  else
    Log.write "socket" s!"failed to start: {path}"

-- | Poll for pending command (empty string = nothing)
def pollCmd : IO (Option String) := do
  let s ← sockPollCmd
  pure (if s.isEmpty then none else some s)

-- | Get socket path (empty if not started)
def getPath : IO String := sockPath.get

-- | Shutdown socket listener, cleanup
def shutdown : IO Unit := do
  sockClose
  sockPath.set ""

-- | Run action with socket active (init before, shutdown after)
def bracket (_test : Bool) (f : IO α) : IO α := do
  init
  try f
  finally shutdown

end Socket

-- ============================================================================
-- Remote: shared path operations for URI-based remote backends
-- ============================================================================

namespace Tc.Remote

-- | Join URI prefix with child name
def join (pfx name : String) : String :=
  if pfx.endsWith "/" then s!"{pfx}{name}" else s!"{pfx}/{name}"

-- | Get parent URI: drop last path component. Returns none at root (≤ minParts components).
def parent (path : String) (minParts : Nat) : Option String :=
  let p := if path.endsWith "/" then (path.take (path.length - 1)).toString else path
  let parts := p.splitOn "/"
  if parts.length ≤ minParts then none
  else some ("/".intercalate (parts.dropLast) ++ "/")

-- | Display name: last non-empty path component (preserves protocol-only paths)
def dispName (path : String) : String :=
  let p := if path.endsWith "/" then (path.take (path.length - 1)).toString else path
  let parts := p.splitOn "/" |>.filter (·.length > 0)
  if parts.length ≤ 1 then path else parts.getLast?.getD path

end Tc.Remote
