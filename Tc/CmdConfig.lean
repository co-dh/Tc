/-
  Command config: Entry type + cached lookups.
  Commands array lives in App/Common (single table with both metadata and handler).
-/
import Std.Data.HashMap
import Std.Data.HashSet
import Tc.Util

namespace Tc.CmdConfig

-- | Lookup result for dispatch
structure CmdInfo where
  cmd : Cmd
  resetsVS : Bool

-- | Command entry: metadata for key binding, menu, and dispatch
structure Entry where
  cmd : Cmd
  ctx : String := ""        -- input context: r=current row, c=current column,
                             -- g=group columns, s=selected rows, a=user arg, S=stack(2+ views)
  key : String := ""        -- key name: "j", "<ret>", "<C-d>", "<S-left>", etc.
  label : String := ""      -- fzf menu label (empty = hidden from menu)
  resetsVS : Bool := false
  viewCtx : String := ""    -- context filter: "freqV", "colMeta", "fld", "tbl", or "" (global)

-- | Cached: (key, viewCtx) → CmdInfo — context-aware key lookup
initialize keyInfoMap : IO.Ref (Std.HashMap (String × String) CmdInfo) ← IO.mkRef {}
-- | Cached: cmd → CmdInfo (socket/programmatic dispatch)
initialize cmdInfoMap : IO.Ref (Std.HashMap Cmd CmdInfo) ← IO.mkRef {}
-- | Cached: handlers that take user input (ctx contains 'a')
initialize argCmdSet : IO.Ref (Std.HashSet Cmd) ← IO.mkRef {}
-- | Cached: menu items for fzf
initialize menuCache : IO.Ref (Array Entry) ← IO.mkRef #[]

-- | Build caches from command entries (called by App/Common.initHandlers).
def init (cmds : Array Entry) : IO Unit := do
  let mut keyInfo : Std.HashMap (String × String) CmdInfo := {}
  let mut cmdInfo : Std.HashMap Cmd CmdInfo := {}
  let mut argSet : Std.HashSet Cmd := {}
  for e in cmds do
    let ci : CmdInfo := { cmd := e.cmd, resetsVS := e.resetsVS }
    if !e.key.isEmpty then keyInfo := keyInfo.insert (e.key, e.viewCtx) ci
    cmdInfo := cmdInfo.insert e.cmd ci
    if e.ctx.contains 'a' then argSet := argSet.insert e.cmd
  keyInfoMap.set keyInfo
  cmdInfoMap.set cmdInfo
  argCmdSet.set argSet
  menuCache.set cmds
  Log.write "init" s!"commands: {cmds.size} entries"

-- | O(1) context-aware lookup: try (key, viewCtx) first, fall back to (key, "")
def keyLookup (key : String) (viewCtx : String := "") : IO (Option CmdInfo) := do
  let m ← keyInfoMap.get
  match m.get? (key, viewCtx) with
  | some ci => pure (some ci)
  | none => pure (m.get? (key, ""))

-- | O(1) lookup by Cmd → CmdInfo.
def cmdLookup (c : Cmd) : IO CmdInfo := do
  pure ((← cmdInfoMap.get).getD c { cmd := c, resetsVS := false })

-- | Lookup by handler name string (socket/external boundary only)
def handlerLookup (h : String) : IO (Option CmdInfo) := do
  match Cmd.ofString? h with
  | some c => some <$> cmdLookup c
  | none => Log.write "cmd" s!"unknown command: {h}"; pure none

-- | O(1) check if command takes user input (ctx contains 'a').
def isArgCmd (c : Cmd) : IO Bool := do
  pure ((← argCmdSet.get).contains c)

-- | Menu items for fzf, filtered by view context. Returns (handler, ctx, key, label).
def menuItems (viewCtx : String) : IO (Array (String × String × String × String)) := do
  let cmds ← menuCache.get
  pure (cmds.filterMap fun e =>
    if e.label.isEmpty then none
    else if !e.viewCtx.isEmpty && e.viewCtx != viewCtx then none
    else some (toString e.cmd, e.ctx, e.key, e.label))

end Tc.CmdConfig
