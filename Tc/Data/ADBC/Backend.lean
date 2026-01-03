/-
  Backend: PRQL compilation + chunked query execution
  Views store PRQL strings, compile to SQL on demand
-/
import Tc.Data.ADBC.FFI
import Tc.Data.ADBC.Table
import Tc.Data.ADBC.Prql
import Tc.Error

open Tc

namespace Backend

-- | Init backend (ADBC + shellfs extension)
def init : IO Bool := do
  let ok ← Adbc.init
  if ok then let _ ← Adbc.query "LOAD shellfs"
  pure ok

-- | Shutdown backend
def shutdown : IO Unit := Adbc.shutdown

-- | Execute raw SQL, return AdbcTable (zero-copy)
def execSql (sql : String) : IO AdbcTable := do
  let qr ← Adbc.query sql
  AdbcTable.ofQueryResult qr

-- | Log to /tmp/tv.log
def logPrql (prql : String) : IO Unit := do
  let ms ← IO.monoMsNow
  let s := ms / 1000 % 86400
  let d2 := fun n : Nat => s!"{Char.ofNat (48 + n / 10)}{Char.ofNat (48 + n % 10)}"
  let ts := s!"{s / 3600}:{d2 ((s % 3600) / 60)}:{d2 (s % 60)}.{ms % 1000}"
  let h ← IO.FS.Handle.mk "/tmp/tv.log" .append
  h.putStrLn s!"[{ts}] [prql] {prql}"

-- | Check if string contains "take "
def hasLimit (s : String) : Bool := (s.splitOn "take ").length > 1

-- | PRQL query with proof it has a limit
structure LimitedQuery where
  prql : String
  proof : hasLimit prql = true

-- | Execute PRQL query (logs error, returns Option)
def query (q : LimitedQuery) : IO (Option AdbcTable) := do
  logPrql q.prql
  let some sql ← Prql.compile q.prql | return none
  try return some (← execSql sql)
  catch e => Error.set s!"SQL error: {e}"; return none

-- | Create LimitedQuery by appending take
def mkLimited (prql : String) (n : Nat) : LimitedQuery :=
  let q := s!"{prql} | take {n}"
  if h : hasLimit q = true then ⟨q, h⟩
  else ⟨"select 1 | take 1", by native_decide⟩  -- fallback, never reached

-- | Theorem: example shows mkLimited has limit
theorem mkLimited_example : hasLimit "from df | take 1000" = true := by native_decide

-- | Get total row count for PRQL query (logs error, returns Option)
def queryCount (prql : String) : IO (Option Nat) := do
  let countPrql := prql ++ " | aggregate {n = std.count this}"
  pure <| (← query (mkLimited countPrql 1)).map fun st =>
    if st.nRows > 0 then
      match st.getIdx 0 0 with
      | .int cnt => cnt.toInt.toNat
      | _ => 0
    else 0

-- | Get cell values for a specific row (logs error, returns Option)
def queryRow (prql : String) (row : Nat) (ncols : Nat) : IO (Option (Array Cell)) := do
  let rowPrql := prql ++ s!" | take {row + 1}"
  pure <| (← query (mkLimited rowPrql (row + 1))).map fun st =>
    if st.nRows > row then Array.range ncols |>.map fun c => st.getIdx row c
    else #[]

-- | Query distinct values for a column (logs error, returns Option)
-- Uses Cell.toRaw for raw values (no comma formatting in numbers)
def queryDistinct (prql : String) (col : String) : IO (Option (Array String)) := do
  let distinctPrql := prql ++ " | select {" ++ col ++ "} | group {" ++ col ++ "} (take 1)"
  logPrql distinctPrql
  let some sql ← Prql.compile distinctPrql | return none
  try
    let st ← execSql sql
    return some ((Array.range st.nRows).map fun r => (st.getIdx r 0).toRaw)
  catch e => Error.set s!"SQL error: {e}"; return none

end Backend
