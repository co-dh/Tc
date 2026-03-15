/-
  Join: combine top 2 stack views via PRQL join/append/remove.
  Key columns (nav.grp) determine join condition; fzf picks operation.
-/
import Tc.View
import Tc.Fzf
import Tc.Data.ADBC.Ops

namespace Tc.Join

inductive JoinOp | inner | left | right | union | diff

-- PRQL join condition: ==col1 && ==col2
private def joinCond (cols : Array String) : String :=
  " && ".intercalate (cols.map (s!"=={Prql.quote ·}")).toList

private def opLabel : JoinOp → String
  | .inner => "join inner" | .left => "join left" | .right => "join right"
  | .union => "union" | .diff => "set diff"

-- PRQL side modifier for join ops
private def joinSide : JoinOp → String
  | .inner => "" | .left => "side:left " | .right => "side:right "
  | _ => ""  -- union/diff don't use join syntax

private def prqlStr (lName rName : String) (cols : Array String) : JoinOp → String
  | .union => s!"from {lName} | append {rName}"
  | .diff  => s!"from {lName} | remove {rName}"
  | op     => s!"from {lName} | join {joinSide op}{rName} ({joinCond cols})"

private def allOps : Array JoinOp := #[.inner, .left, .right, .union, .diff]

-- Generate unique view name and compile PRQL to SQL (deferred DDL)
private def prepareView (tbl : AdbcTable) (suffix : String) : IO (String × String) := do
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let name := s!"tc_j{suffix}_{n}"
  let prql := tbl.query.render
  Log.write "prql" prql
  let some sql ← Prql.compile prql | throw (IO.userError "PRQL compile failed")
  pure (name, stripSemi sql)

-- Full workflow: validate stack, show fzf menu, execute, push result
def run (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  let some parent := s.tl.head? | return none  -- need ≥2 views
  let leftGrp := parent.nav.grp
  let rightGrp := s.cur.nav.grp
  -- Joins require matching key columns by name; union/diff always available
  let joinOk := leftGrp.size > 0 && leftGrp == rightGrp
  let ops := if joinOk then allOps else #[.union, .diff]
  -- Prepare view names + SQL (deferred DDL — no DB work until after fzf)
  let (lName, lSql) ← prepareView parent.nav.tbl "l"
  let (rName, rSql) ← prepareView s.tbl "r"
  let items := ops.map fun op => s!"{opLabel op}  |  {prqlStr lName rName leftGrp op}"
  let some idx ← Fzf.fzfIdx #["--prompt=join> "] items | return none
  let op := ops.getD idx .inner
  -- Create temp views + materialize join result (only after user confirms)
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP VIEW {lName} AS {lSql}"
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP VIEW {rName} AS {rSql}"
  let prql := prqlStr lName rName leftGrp op
  Log.write "prql" prql
  let n ← memTblCounter.modifyGet fun n => (n, n + 1)
  let tblName := s!"tc_join_{n}"
  let some sql ← Prql.compile prql | throw (IO.userError s!"join PRQL compile failed: {prql}")
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP TABLE {tblName} AS {sql |> stripSemi}"
  let q : Prql.Query := { base := s!"from {tblName}" }
  let total ← AdbcTable.queryCount q
  let some adbc ← AdbcTable.requery q total | return none
  -- Pop top view, replace second with result
  let some s' := s.pop | return none
  let disp := match op with
    | .union => "union" | .diff => "diff"
    | _ => s!"⋈ ({", ".intercalate leftGrp.toList})"
  match View.fromTbl adbc s'.cur.path with
  | some v => return some (s'.setCur { v with disp })
  | none => return none

end Tc.Join
