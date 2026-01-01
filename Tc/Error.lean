/-
  Centralized error handling: log to file, store for status bar, return Option
-/

namespace Error

-- | Last error (for status bar display)
initialize lastErr : IO.Ref String ← IO.mkRef ""

-- | Get and clear last error
def pop : IO String := lastErr.modifyGet fun e => ("", e)

-- | Set last error (logs + stores for status bar)
def set (msg : String) : IO Unit := do
  logError msg
  lastErr.set msg
where
  logError (msg : String) : IO Unit := do
    let ts ← timestamp
    let h ← IO.FS.Handle.mk "/tmp/tv.log" .append
    h.putStrLn s!"[{ts}] [error] {msg}"
  timestamp : IO String := do
    let ms ← IO.monoMsNow
    let s := ms / 1000 % 86400
    let d2 := fun n : Nat => s!"{Char.ofNat (48 + n / 10)}{Char.ofNat (48 + n % 10)}"
    pure s!"{s / 3600}:{d2 ((s % 3600) / 60)}:{d2 (s % 60)}.{ms % 1000}"

end Error
