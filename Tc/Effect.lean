/-
  Effect: describes IO operations without executing them.
  Pure state machine returns Effect; Runner interprets it.
-/
import Tc.Cmd

namespace Tc

-- | Effect: describes an IO operation to perform
-- Runner interprets effects and may produce follow-up state changes
inductive Effect where
  | none                                              -- no effect
  | quit                                              -- exit app
  -- fzf effects (user selection via fzf)
  | fzfCol                                            -- column picker: s
  | fzfRow (colIdx : Nat) (colName : String)          -- row search: /
  | fzfFilter (colIdx : Nat) (colName : String)       -- row filter: \
  -- query effects (database/table operations)
  | queryMeta                                         -- push meta view: M
  | queryFreq (cols : Array Nat) (colNames : Array String)  -- push freq view: F
  | freqFilter (cols : Array String) (row : Nat)      -- filter from freq row
  | queryFilter (expr : String)                       -- apply filter expr
  | querySort (colIdx : Nat) (grp : Array Nat) (asc : Bool)  -- sort: [/]
  | queryDel (colIdx : Nat) (sels : Array Nat) (grp : Array String)  -- delete: d
  -- folder effects (filesystem)
  | folderPush                                        -- push folder view: Dc
  | folderEnter                                       -- enter dir/file: D~
  | folderDel                                         -- delete file: Dd
  | folderDepth (delta : Int)                         -- change find depth: +d/-d
  -- search effects
  | findNext                                          -- search next: n
  | findPrev                                          -- search prev: N
  -- theme effects
  | themeLoad (delta : Int)                           -- load theme with delta cycle
  deriving Repr, BEq

namespace Effect

-- | Is this a no-op effect?
def isNone : Effect → Bool
  | .none => true | _ => false

end Effect

-- | Update typeclass: pure state transition returning Effect
-- Unlike Exec (IO), this is pure and testable
class Update (α : Type) where
  update : α → Cmd → Option (α × Effect)

end Tc
