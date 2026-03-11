/-
  Centralized error/debug logging: log to file, store for status bar
-/

namespace Log

-- | Log file path (single source of truth for Lean + C)
def path : String := "tmp/tc.log"

@[extern "lean_set_log_path"]
opaque setLogPath : @& String → IO Unit

@[extern "lean_local_timestamp"]
opaque localTimestamp : IO String

-- | Format timestamp HH:MM:SS.mmm (local time)
def timestamp : IO String := localTimestamp

-- | Write log entry
def write (tag msg : String) : IO Unit := do
  let h ← IO.FS.Handle.mk path .append
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
