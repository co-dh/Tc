/-
  Centralized error/debug logging: log to file, store for status bar
-/

namespace Log

-- | Log file path
def path : String := "/tmp/tc.log"

-- | Format timestamp HH:MM:SS.mmm
def timestamp : IO String := do
  let ms ← IO.monoMsNow
  let s := ms / 1000 % 86400
  let d2 := fun n : Nat => s!"{Char.ofNat (48 + n / 10)}{Char.ofNat (48 + n % 10)}"
  pure s!"{d2 (s / 3600)}:{d2 ((s % 3600) / 60)}:{d2 (s % 60)}.{ms % 1000}"

-- | Write log entry
def write (tag msg : String) : IO Unit := do
  let h ← IO.FS.Handle.mk path .append
  h.putStrLn s!"[{←timestamp}] [{tag}] {msg}"

-- | Log error message
def error (msg : String) : IO Unit := write "error" msg

end Log

namespace Error

-- | Set error (logs to file)
def set (msg : String) : IO Unit := Log.error msg

end Error
