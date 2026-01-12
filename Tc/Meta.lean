/-
  Meta view: column statistics (name, type, count, distinct, null%, min, max)
  Generic over table type T - no ADBC dependency.
-/
import Tc.View.Generic
import Tc.Data.Mem.Meta

namespace Tc.Meta

-- Uses shared helpers from Tc/Data/Mem/Meta.lean: headers, toMemTable, selNull, selSingle, selNames

variable {T : Type} [ReadTable T] [ModifyTable T] [RenderTable T] [AsMem T]

-- | Select rows in meta view by predicate f (e.g. selNull, selSingle)
-- In meta view, rows = columns from parent table, so selecting rows = selecting columns
def sel (s : GViewStack T) (f : MemTable → Array Nat) : GViewStack T :=
  if s.cur.vkind != .colMeta then s else
  match AsMem.asMem? s.cur.nav.tbl with
  | some tbl =>
    let rows := f tbl
    let nav' := { s.cur.nav with row := { s.cur.nav.row with sels := rows } }
    s.setCur { s.cur with nav := nav' }
  | none => s

-- | Set key cols from meta view selections, pop to parent, select cols
def setKey (s : GViewStack T) : Option (GViewStack T) :=
  if s.cur.vkind != .colMeta then some s else
  if !s.hasParent then some s else
  match AsMem.asMem? s.cur.nav.tbl with
  | some tbl =>
    let colNames := selNames tbl s.cur.nav.row.sels
    s.pop.map fun s' =>
      let nav' := { s'.cur.nav with grp := colNames, col := { s'.cur.nav.col with sels := colNames } }
      s'.setCur { s'.cur with nav := nav' }
  | none => some s

-- | Pure update: returns Effect for IO operations, pure for selections
def update (s : GViewStack T) (cmd : Cmd) : Option (GViewStack T × Effect) :=
  match cmd with
  | .metaV .dup => some (s, .queryMeta)              -- push meta view (IO)
  | .metaV .dec => some (sel s selNull, .none)       -- select null cols
  | .metaV .inc => some (sel s selSingle, .none)     -- select single-val cols
  | .metaV .ent => if s.cur.vkind == .colMeta then setKey s |>.map (·, .none) else none
  | _ => none

-- | Execute meta command (IO version)
def exec (s : GViewStack T) (pushMeta : GViewStack T → IO (Option (GViewStack T))) (cmd : Cmd)
    : IO (Option (GViewStack T)) := do
  match cmd with
  | .metaV .dup => (← pushMeta s).orElse (fun _ => some s) |> pure
  | .metaV .dec => pure (some (sel s selNull))
  | .metaV .inc => pure (some (sel s selSingle))
  | .metaV .ent => if s.cur.vkind == .colMeta then pure (setKey s) else pure none
  | _ => pure none

end Tc.Meta
