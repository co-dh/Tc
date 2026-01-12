/-
  Entry point for core tests
-/
import test.CoreTest

def main : IO Unit := do
  IO.FS.writeFile "test.log" ""
  IO.println "Running Tc core tests (CSV only)...\n"
  CoreTest.runTests ".lake/build/bin/tc-core"
  IO.println "\nAll core tests passed!"
