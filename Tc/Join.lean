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
  cols.map (s!"=={Prql.quote ·}") |>.joinWith " && "

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
  let name ← nextTmpName s!"j{suffix}"
  let prql := tbl.query.render
  Log.write "prql" prql
  let some sql ← Prql.compile prql | throw (IO.userError "PRQL compile failed")
  pure (name, stripSemi sql)

-- | Execute join with resolved op. Shared by run (fzf) and runWith (socket).
private def execJoin (s : ViewStack AdbcTable) (op : JoinOp) (leftGrp : Array String) : IO (Option (ViewStack AdbcTable)) := do
  let some parent := s.tl.head? | return none
  let (lName, lSql) ← prepareView parent.nav.tbl "l"
  let (rName, rSql) ← prepareView s.tbl "r"
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP VIEW {lName} AS {lSql}"
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP VIEW {rName} AS {rSql}"
  let prql := prqlStr lName rName leftGrp op
  Log.write "prql" prql
  let tblName ← nextTmpName "join"
  let some sql ← Prql.compile prql | throw (IO.userError s!"join PRQL compile failed: {prql}")
  let _ ← Adbc.query s!"CREATE OR REPLACE TEMP TABLE {tblName} AS {sql |> stripSemi}"
  let some adbc ← AdbcTable.fromTmpTbl tblName | return none
  let some s' := s.pop | return none
  let disp := match op with
    | .union => "union" | .diff => "diff"
    | _ => s!"⋈ ({leftGrp.joinWith ", "})"
  return (View.fromTbl adbc s'.cur.path |>.map fun v => s'.setCur { v with disp })

-- | Resolve available ops from stack state
private def resolveOps (s : ViewStack AdbcTable) : Option (Array JoinOp × Array String) := do
  let parent ← s.tl.head?
  let leftGrp := parent.nav.grp
  let joinOk := leftGrp.size > 0 && leftGrp == s.cur.nav.grp
  pure (if joinOk then allOps else #[.union, .diff], leftGrp)

-- Full workflow: validate stack, show fzf menu, execute, push result
def run (test : Bool) (s : ViewStack AdbcTable) : IO (Option (ViewStack AdbcTable)) := do
  let some (ops, leftGrp) := resolveOps s | return none
  let some parent := s.tl.head? | return none
  let (lName, _) ← prepareView parent.nav.tbl "l"
  let (rName, _) ← prepareView s.tbl "r"
  let items := ops.map fun op => s!"{opLabel op}  |  {prqlStr lName rName leftGrp op}"
  let some idx ← Fzf.fzfIdx test #["--prompt=join> "] items | return none
  ops.getD idx .inner |> (execJoin s · leftGrp)

-- | Join by operation index directly (no fzf). Called by socket/dispatch.
def runWith (s : ViewStack AdbcTable) (idxStr : String) : IO (Option (ViewStack AdbcTable)) := do
  let some (ops, leftGrp) := resolveOps s | return none
  let idx := idxStr.toNat?.getD 0
  if idx >= ops.size then return none
  ops.getD idx .inner |> (execJoin s · leftGrp)

end Tc.Join
