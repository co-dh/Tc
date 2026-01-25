/-
  App/Core: Core build entry point (MemTable only, no ADBC/Kdb)
  Supports CSV files and stdin only.
-/
import Tc.Table.Mem  -- core build: mem only
import Tc.App.Common

open Tc

-- | Output table as plain text
def outputTable (a : AppState Table) : IO Unit := do
  IO.println (← Table.toText a.stk.cur.nav.tbl)

-- | Entry point (core build: CSV only)
def main (args : List String) : IO Unit := do
  let (path?, keys, testMode) := parseArgs args
  let envTest := (← IO.getEnv "TC_TEST_MODE").isSome
  Fzf.setTestMode (testMode || envTest)
  let pipeMode ← if testMode then pure false else (! ·) <$> Term.isattyStdin
  let theme ← Theme.State.init
  let ok ← Backend.init  -- no-op for core
  if !ok then IO.eprintln "Backend init failed"; return
  if pipeMode && path?.isNone then
    if let some a ← runMem (T := Table) (← MemTable.fromStdin) "stdin" true testMode theme keys then outputTable a
    return
  let path := path?.getD ""
  try
    if path.isEmpty then  -- no file: show current directory as folder view
      match ← Folder.mkView (T := Table) "." 1 with
      | some v => let _ ← runApp (T := Table) v pipeMode testMode theme keys
      | none => IO.eprintln "Cannot list directory"
    else if path.endsWith ".txt" then  -- txt: parse as space-separated text
      let _ ← runMem (T := Table) (MemTable.fromText (← IO.FS.readFile path)) path pipeMode testMode theme keys
    else  -- use Table.fromFile (CSV only in core)
      match ← Table.fromFile path with
      | some tbl => match View.fromTbl tbl path with
        | some v => let _ ← runApp (T := Table) v pipeMode testMode theme keys
        | none => IO.eprintln "Cannot open file (empty table)"
      | none => pure ()  -- error already printed by Table.fromFile
  finally
    Backend.shutdown
