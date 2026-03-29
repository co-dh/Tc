/-
  Command config: loads cfg/commands.sql into DuckDB, caches entries for runtime lookup.
  Single source of truth for the command matrix.
  Lookup by key char or handler name — no obj+verb intermediate encoding.
-/
import Std.Data.HashMap
import Tc.Data.ADBC.Table

namespace Tc.CmdConfig

-- | Lookup result for dispatch
structure CmdInfo where
  handler : String
  resetsVS : Bool

-- | Entry for menus and arg shortcuts
structure Entry where
  label : String
  handler : String
  key : String
  resetsVS : Bool
  argHandler : String   -- arg shortcut target handler (empty = none)
  argDefault : String   -- arg shortcut default arg (empty = none)
  viewCtx : String

private def commandsSql : String := include_str "../cfg/commands.sql"

-- | Cached: key char → CmdInfo (physical key → handler)
initialize keyInfoMap : IO.Ref (Std.HashMap Char CmdInfo) ← IO.mkRef {}
-- | Cached: handler name → CmdInfo (socket/programmatic dispatch)
initialize handlerInfoMap : IO.Ref (Std.HashMap String CmdInfo) ← IO.mkRef {}
-- | Cached: entries for menus and arg shortcuts
initialize entryList : IO.Ref (Array Entry) ← IO.mkRef #[]

-- | Create tv_commands table and build caches. Call after Adbc.init.
def init : IO Unit := do
  let stmts := commandsSql.splitOn ";\n" |>.map (·.trimAscii.toString) |>.filter (·.length > 0)
  for stmt in stmts do
    let _ ← Adbc.query stmt
  let qr ← Adbc.query "SELECT obj, verb, label, handler, key, resets_vs, arg_handler, arg_default, view_ctx FROM tv_commands"
  let nr ← Adbc.nrows qr
  let mut entries : Array Entry := #[]
  let mut keyInfo : Std.HashMap Char CmdInfo := {}
  let mut handlerInfo : Std.HashMap String CmdInfo := {}
  for i in [:nr.toNat] do
    let row := i.toUInt64
    let _ ← Adbc.cellStr qr row 0  -- obj (unused, kept for SQL compatibility)
    let _ ← Adbc.cellStr qr row 1  -- verb (unused)
    let label ← Adbc.cellStr qr row 2;     let handler ← Adbc.cellStr qr row 3
    let key ← Adbc.cellStr qr row 4;       let resetsVS := (← Adbc.cellStr qr row 5) == "true"
    let argHandler ← Adbc.cellStr qr row 6; let argDefault ← Adbc.cellStr qr row 7
    let viewCtx ← Adbc.cellStr qr row 8
    let ci : CmdInfo := { handler, resetsVS }
    entries := entries.push { label, handler, key, resetsVS, argHandler, argDefault, viewCtx }
    if !key.isEmpty then keyInfo := keyInfo.insert key.front ci
    handlerInfo := handlerInfo.insert handler ci
  keyInfoMap.set keyInfo
  handlerInfoMap.set handlerInfo
  entryList.set entries
  Log.write "init" s!"commands: {entries.size} entries"

-- | O(1) lookup by physical key char → handler + resetsVS.
def keyLookup (c : Char) : IO (Option CmdInfo) := do
  pure ((← keyInfoMap.get).get? c)

-- | O(1) lookup by handler name → CmdInfo. For socket/programmatic dispatch.
def handlerLookup (h : String) : IO CmdInfo := do
  pure ((← handlerInfoMap.get).getD h { handler := h, resetsVS := false })

-- | Arg shortcut by handler name: returns (argHandler, defaultArg) if this handler has one
def argShortcut (handler : String) : IO (Option (String × String)) := do
  let es ← entryList.get
  return es.findSome? fun e =>
    if e.handler == handler && !e.argHandler.isEmpty then some (e.argHandler, e.argDefault) else none

-- | Menu items for fzf, filtered by view context. Returns (handler, label).
def menuItems (viewCtx : String) : IO (Array (String × String)) := do
  let es ← entryList.get
  return es.filterMap fun e =>
    if e.label.isEmpty then none
    else if !e.viewCtx.isEmpty && e.viewCtx != viewCtx then none
    else some (e.handler, e.label)

end Tc.CmdConfig
