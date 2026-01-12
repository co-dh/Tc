/-
  Meta view: column statistics (name, type, count, distinct, null%, min, max)
  Returns MemTable with typed columns for proper sorting.
  Pure update returns Effect; Runner executes IO.
-/
import Tc.View
import Tc.Data.Mem.Meta

namespace Tc.Meta

-- Uses shared helpers from Tc/Data/Mem/Meta.lean: headers, toMemTable, selNull, selSingle, selNames

-- | Push column metadata view onto stack
def push (s : ViewStack) : IO (Option ViewStack) := do
  let tbl ← QueryTable.queryMeta s.cur.nav.tbl <&> toMemTable
  let some v := View.fromTbl (.mem tbl) s.cur.path | return none
  return some (s.push { v with vkind := .colMeta, disp := "meta" })

-- | Select rows in meta view by predicate f (e.g. selNull, selSingle)
-- In meta view, rows = columns from parent table, so selecting rows = selecting columns
def sel (s : ViewStack) (f : MemTable → Array Nat) : ViewStack :=
  if s.cur.vkind != .colMeta then s else
  match s.cur.nav.tbl.asMem? with
  | some tbl =>
    let rows := f tbl
    let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
    s.setCur { s.cur with nav := nav' }
  | none => s

-- | Set key cols from meta view selections, pop to parent, select cols
def setKey (s : ViewStack) : Option ViewStack :=
  if s.cur.vkind != .colMeta then some s else
  if !s.hasParent then some s else
  match s.cur.nav.tbl.asMem? with
  | some tbl =>
    let colNames := selNames tbl s.cur.nav.row.sels
    match s.pop with
    | some s' =>
      let nav' := { s'.cur.nav with grp := colNames, col := { s'.cur.nav.col with sels := colNames } }
      some (s'.setCur { s'.cur with nav := nav' })
    | none => some s
  | none => some s

-- | Pure update: returns Effect for IO operations, pure for selections
def update (s : ViewStack) (cmd : Cmd) : Option (ViewStack × Effect) :=
  match cmd with
  | .metaV .dup => some (s, .queryMeta)              -- push meta view (IO)
  | .metaV .dec => some (sel s selNull, .none)       -- select null cols (pure)
  | .metaV .inc => some (sel s selSingle, .none)     -- select single-val cols (pure)
  | .metaV .ent => if s.cur.vkind == .colMeta then setKey s |>.map (·, .none) else none
  | _ => none

-- | Execute meta command (IO version for backward compat)
def exec (s : ViewStack) (cmd : Cmd) : IO (Option ViewStack) := do
  match cmd with
  | .metaV .dup => (← push s).orElse (fun _ => some s) |> pure
  | .metaV .dec => pure (some (sel s selNull))
  | .metaV .inc => pure (some (sel s selSingle))
  | .metaV .ent => if s.cur.vkind == .colMeta then pure (setKey s) else pure none
  | _ => pure none

end Tc.Meta
