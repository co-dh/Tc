/-
  Command config: loads cfg/commands.sql into DuckDB, caches entries for runtime lookup.
  Single source of truth for the command matrix.
-/
import Std.Data.HashMap
import Tc.Data.ADBC.Table
import Tc.Error

namespace Tc.CmdConfig

structure Entry where
  obj : Char
  verb : Char
  label : String
  handler : String
  key : String
  resetsVS : Bool
  argHandler : String   -- verb→arg shortcut target handler (empty = none)
  argDefault : String   -- verb→arg shortcut default arg (empty = none)
  viewCtx : String

-- | Lookup result for dispatch
structure CmdInfo where
  handler : Handler
  resetsVS : Bool

private def commandsSql : String := include_str "../cfg/commands.sql"

-- | Cached: handler+resetsVS by "objverb" key, entries array for menus
initialize infoMap : IO.Ref (Std.HashMap String CmdInfo) ← IO.mkRef {}
initialize entryList : IO.Ref (Array Entry) ← IO.mkRef #[]
-- | Cached: key shortcut char → Cmd
initialize keyMap : IO.Ref (Std.HashMap Char (Char × Char)) ← IO.mkRef {}

-- | Create tv_commands table and build caches. Call after Adbc.init.
def init : IO Unit := do
  let stmts := commandsSql.splitOn ";\n" |>.map (·.trimAscii.toString) |>.filter (·.length > 0)
  for stmt in stmts do
    let _ ← Adbc.query stmt
  let qr ← Adbc.query "SELECT obj, verb, label, handler, key, resets_vs, arg_handler, arg_default, view_ctx FROM tv_commands"
  let nr ← Adbc.nrows qr
  let mut entries : Array Entry := #[]
  let mut info : Std.HashMap String CmdInfo := {}
  let mut keys : Std.HashMap Char (Char × Char) := {}
  for i in [:nr.toNat] do
    let row := i.toUInt64
    let objStr ← Adbc.cellStr qr row 0;    let obj := if objStr.isEmpty then ' ' else objStr.front
    let verbStr ← Adbc.cellStr qr row 1;   let verb := if verbStr.isEmpty then ' ' else verbStr.front
    let label ← Adbc.cellStr qr row 2;     let handler ← Adbc.cellStr qr row 3
    let key ← Adbc.cellStr qr row 4;       let resetsVS := (← Adbc.cellStr qr row 5) == "true"
    let argHandler ← Adbc.cellStr qr row 6; let argDefault ← Adbc.cellStr qr row 7
    let viewCtx ← Adbc.cellStr qr row 8
    entries := entries.push { obj, verb, label, handler, key, resetsVS, argHandler, argDefault, viewCtx }
    let h := Handler.fromString? handler |>.getD .rowInc
    info := info.insert s!"{obj}{verb}" { handler := h, resetsVS }
    if !key.isEmpty then keys := keys.insert key.front (obj, verb)
  infoMap.set info
  entryList.set entries
  keyMap.set keys
  Log.write "init" s!"commands: {entries.size} entries"

-- | O(1) handler + resetsVS lookup. Defaults to .rowInc/false for unknown.
def lookup (obj verb : Char) : IO CmdInfo := do
  let m ← infoMap.get
  pure (m.getD s!"{obj}{verb}" { handler := .rowInc, resetsVS := false })

-- | Verb→arg shortcut: returns (targetHandler, defaultArg) if this command has one
def argShortcut (obj verb : Char) : IO (Option (Handler × String)) := do
  let es ← entryList.get
  return es.findSome? fun e =>
    if e.obj == obj && e.verb == verb && !e.argHandler.isEmpty then
      (Handler.fromString? e.argHandler).map (·, e.argDefault)
    else none

-- | Key shortcut lookup: physical key char → (obj, verb). From SQL 'key' column.
def keyLookup (c : Char) : IO (Option (Char × Char)) := do
  let m ← keyMap.get
  pure (m.get? c)

-- | Menu items for fzf, filtered by view context.
def menuItems (viewCtx : String) : IO (Array (String × String)) := do
  let es ← entryList.get
  return es.filterMap fun e =>
    if e.label.isEmpty then none
    else if !e.viewCtx.isEmpty && e.viewCtx != viewCtx then none
    else some (s!"{e.obj}{e.verb}", e.label)

end Tc.CmdConfig
