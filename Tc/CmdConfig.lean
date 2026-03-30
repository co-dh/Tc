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
  handler : String
  resetsVS : Bool

-- | Command entry: metadata for key binding, menu, and dispatch
structure Entry where
  handler : String
  key : String := ""       -- key name: "j", "<ret>", "<C-d>", "<S-left>", etc.
  label : String := ""      -- fzf menu label (empty = hidden from menu)
  resetsVS : Bool := false
  viewCtx : String := ""    -- context filter: "freqV", "colMeta", "fld", "tbl", or "" (global)
  isArg : Bool := false     -- handler takes user input (fzf or typed arg)

-- | Cached: (key, viewCtx) → CmdInfo — context-aware key lookup
initialize keyInfoMap : IO.Ref (Std.HashMap (String × String) CmdInfo) ← IO.mkRef {}
-- | Cached: handler name → CmdInfo (socket/programmatic dispatch)
initialize handlerInfoMap : IO.Ref (Std.HashMap String CmdInfo) ← IO.mkRef {}
-- | Cached: handlers that take user input (isArg = true)
initialize argHandlerSet : IO.Ref (Std.HashSet String) ← IO.mkRef {}
-- | Cached: menu items (handler, label) for fzf
initialize menuCache : IO.Ref (Array Entry) ← IO.mkRef #[]

-- | Build caches from command entries (called by App/Common.initHandlers).
def init (cmds : Array Entry) : IO Unit := do
  let mut keyInfo : Std.HashMap (String × String) CmdInfo := {}
  let mut handlerInfo : Std.HashMap String CmdInfo := {}
  let mut argSet : Std.HashSet String := {}
  for e in cmds do
    let ci : CmdInfo := { handler := e.handler, resetsVS := e.resetsVS }
    if !e.key.isEmpty then keyInfo := keyInfo.insert (e.key, e.viewCtx) ci
    handlerInfo := handlerInfo.insert e.handler ci
    if e.isArg then argSet := argSet.insert e.handler
  keyInfoMap.set keyInfo
  handlerInfoMap.set handlerInfo
  argHandlerSet.set argSet
  menuCache.set cmds
  Log.write "init" s!"commands: {cmds.size} entries"

-- | O(1) context-aware lookup: try (key, viewCtx) first, fall back to (key, "")
def keyLookup (key : String) (viewCtx : String := "") : IO (Option CmdInfo) := do
  let m ← keyInfoMap.get
  match m.get? (key, viewCtx) with
  | some ci => pure (some ci)
  | none => pure (m.get? (key, ""))

-- | O(1) lookup by handler name → CmdInfo. For socket/programmatic dispatch.
def handlerLookup (h : String) : IO CmdInfo := do
  pure ((← handlerInfoMap.get).getD h { handler := h, resetsVS := false })

-- | O(1) check if handler takes user input (derived from isArg field in config).
def isArgHandler (h : String) : IO Bool := do
  pure ((← argHandlerSet.get).contains h)

-- | Menu items for fzf, filtered by view context. Returns (handler, key, label).
def menuItems (viewCtx : String) : IO (Array (String × String × String)) := do
  let cmds ← menuCache.get
  pure (cmds.filterMap fun e =>
    if e.label.isEmpty then none
    else if !e.viewCtx.isEmpty && e.viewCtx != viewCtx then none
    else some (e.handler, e.key, e.label))

end Tc.CmdConfig
