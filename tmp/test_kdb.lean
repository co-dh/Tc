/-
  Test kdb backend against running q server at :8888
-/
import Tc.Data.Kdb.FFI
import Tc.Data.Kdb.Q
import Tc.Data.Kdb.Table
open Tc

def main : IO Unit := do
  IO.println "Connecting to kdb server at localhost:8888..."
  let ok ← Kdb.connect "localhost" 8888
  if !ok then
    IO.eprintln "Failed to connect"
    return
  IO.println "Connected!"

  -- query tables
  IO.println "\nQuerying tables..."
  let qr ← Kdb.query "tables[]"
  let nr ← Kdb.nrows qr
  IO.println s!"Found {nr} tables"
  for i in [:nr.toNat] do
    let name ← Kdb.cellStr qr i.toUInt64 0
    IO.println s!"  - {name}"

  -- if nbbo exists, query it
  IO.println "\nQuerying 10#nbbo..."
  try
    let qr2 ← Kdb.query "10#nbbo"
    let nc ← Kdb.ncols qr2
    let nr2 ← Kdb.nrows qr2
    IO.println s!"nbbo: {nr2} rows x {nc} cols"
    -- print column names
    IO.print "Columns: "
    for c in [:nc.toNat] do
      let name ← Kdb.colName qr2 c.toUInt64
      let typ ← Kdb.colType qr2 c.toUInt64
      IO.print s!"{name}({typ}) "
    IO.println ""
    -- print first 3 rows
    for r in [:min nr2.toNat 3] do
      IO.print s!"Row {r}: "
      for c in [:nc.toNat] do
        let val ← Kdb.cellStr qr2 r.toUInt64 c.toUInt64
        IO.print s!"{val} | "
      IO.println ""
  catch e =>
    IO.eprintln s!"nbbo query failed: {e}"

  Kdb.disconnect
  IO.println "\nDisconnected."
