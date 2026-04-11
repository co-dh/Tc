/-
  Command config: Entry type + immutable cached lookups (Cache).
  Commands array lives in App/Common (single table with both metadata and handler).
  Cache is built once at boot and carried via Env (see Tc/Env.lean).
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

-- | Immutable cache built once from the command entries array.
-- Replaces the previous 4 IO.Refs (keyInfoMap/cmdInfoMap/argCmdSet/menuCache).
structure Cache where
  keyInfo : Std.HashMap (String × String) CmdInfo := {}
  cmdInfo : Std.HashMap Cmd CmdInfo                := {}
  argCmds : Std.HashSet Cmd                        := {}
  entries : Array Entry                            := #[]
  deriving Inhabited

-- | Build the cache from the command entries array. Pure: no IO, no globals.
def build (cmds : Array Entry) : Cache := Id.run do
  let mut keyInfo : Std.HashMap (String × String) CmdInfo := {}
  let mut cmdInfo : Std.HashMap Cmd CmdInfo := {}
  let mut argCmds : Std.HashSet Cmd := {}
  for e in cmds do
    let ci : CmdInfo := { cmd := e.cmd, resetsVS := e.resetsVS }
    if !e.key.isEmpty then keyInfo := keyInfo.insert (e.key, e.viewCtx) ci
    cmdInfo := cmdInfo.insert e.cmd ci
    if e.ctx.contains 'a' then argCmds := argCmds.insert e.cmd
  pure { keyInfo, cmdInfo, argCmds, entries := cmds }

-- | O(1) context-aware lookup: try (key, viewCtx) first, fall back to (key, "")
def Cache.keyLookup (c : Cache) (key : String) (viewCtx : String := "") : Option CmdInfo :=
  match c.keyInfo.get? (key, viewCtx) with
  | some ci => some ci
  | none => c.keyInfo.get? (key, "")

-- | O(1) lookup by Cmd → CmdInfo.
def Cache.cmdLookup (c : Cache) (cmd : Cmd) : CmdInfo :=
  c.cmdInfo.getD cmd { cmd, resetsVS := false }

-- | Lookup by handler name string (socket/external boundary only)
def Cache.handlerLookup (c : Cache) (h : String) : IO (Option CmdInfo) := do
  match Cmd.ofString? h with
  | some cmd => pure (some (c.cmdLookup cmd))
  | none => Log.write "cmd" s!"unknown command: {h}"; pure none

-- | O(1) check if command takes user input (ctx contains 'a').
def Cache.isArgCmd (c : Cache) (cmd : Cmd) : Bool :=
  c.argCmds.contains cmd

-- | Menu items for fzf, filtered by view context. Returns (handler, ctx, key, label).
def Cache.menuItems (c : Cache) (viewCtx : String) : Array (String × String × String × String) :=
  c.entries.filterMap fun e =>
    if e.label.isEmpty then none
    else if !e.viewCtx.isEmpty && e.viewCtx != viewCtx then none
    else some (toString e.cmd, e.ctx, e.key, e.label)

end Tc.CmdConfig
