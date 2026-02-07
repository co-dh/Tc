/-
  App: Full build entry point (ADBC + Kdb)
-/
import Tc.Table
import Tc.App.Common

open Tc

def main (args : List String) : IO Unit := do
  try appMain Table.toText Backend.init Backend.shutdown args
  catch e =>
    let msg := toString e
    IO.eprintln s!"Error: {msg}"
