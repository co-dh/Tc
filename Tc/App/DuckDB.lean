/-
  App/DuckDB: DuckDB build entry point (MemTable + ADBC, no Kdb)
-/
import Tc.Table.DuckDB
import Tc.App.Common

open Tc

def main (args : List String) : IO Unit :=
  appMain (T := Table) Table.toText Backend.init Backend.shutdown args
