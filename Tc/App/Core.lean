/-
  App/Core: Core build entry point (MemTable only, no ADBC/Kdb)
-/
import Tc.Table.Mem
import Tc.App.Common

open Tc

def main (args : List String) : IO Unit := do
  try appMain (T := Table) Table.toText Backend.init Backend.shutdown args
  catch e =>
    let msg := toString e
    IO.eprintln s!"Error: {msg}"
