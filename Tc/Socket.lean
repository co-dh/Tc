/-
  Unix socket command channel: external tools send handler name strings
  Socket path: $TMPDIR/tv-$PID.sock (set as TV_SOCK env var for children)
-/
import Tc.Error

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
