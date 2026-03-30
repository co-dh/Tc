/-
  Command config: inline Lean array, no SQL/DuckDB dependency.
  Single source of truth for the command matrix.
  Lookup by key string or handler name.
-/
import Std.Data.HashMap
import Std.Data.HashSet
import Tc.Util

namespace Tc.CmdConfig

-- | Lookup result for dispatch
structure CmdInfo where
  handler : String
  resetsVS : Bool

-- | Entry for menus and arg shortcuts
structure Entry where
  handler : String
  key : String := ""       -- key name: "j", "<ret>", "<C-d>", "<S-left>", etc.
  label : String := ""      -- fzf menu label (empty = hidden from menu)
  resetsVS : Bool := false
  viewCtx : String := ""    -- context filter: "freqV", "colMeta", "fld", "tbl", or "" (global)
  isArg : Bool := false     -- handler takes user input (fzf or typed arg)

-- | Command matrix — single source of truth
def commands : Array Entry := #[
  -- row navigation (hjkl, arrows, pgup/pgdn, home/end, ctrl-d/u)
  { handler := "nav.rowInc",      key := "j" },
  { handler := "nav.rowDec",      key := "k" },
  { handler := "nav.rowPgDn",     key := "<pgdn>" },
  { handler := "nav.rowPgUp",     key := "<pgup>" },
  { handler := "nav.rowPgDn",     key := "<C-d>" },
  { handler := "nav.rowPgUp",     key := "<C-u>" },
  { handler := "nav.rowTop",      key := "<home>" },
  { handler := "nav.rowBot",      key := "<end>" },
  { handler := "nav.rowSel",      key := "T", label := "Select/deselect current row" },
  -- row search/filter
  { handler := "filter.rowSearch", key := "/", label := "Search for value in current column", resetsVS := true, isArg := true },
  { handler := "filter.rowFilter", key := "\\", label := "Filter rows by PRQL expression", resetsVS := true, isArg := true },
  { handler := "filter.searchNext",key := "n", label := "Jump to next search match" },
  { handler := "filter.searchPrev",key := "N", label := "Jump to previous search match" },
  -- col navigation
  { handler := "nav.colInc",      key := "l" },
  { handler := "nav.colDec",      key := "h" },
  { handler := "nav.colFirst" },
  { handler := "nav.colLast" },
  { handler := "nav.colGrp",      key := "!", label := "Toggle group on current column" },
  { handler := "nav.colHide",     key := "H", label := "Hide/unhide current column" },
  { handler := "nav.colExclude",  key := "x", label := "Delete column(s) from query", resetsVS := true },
  { handler := "nav.colShiftL",   key := "<S-left>", label := "Shift key column left" },
  { handler := "nav.colShiftR",   key := "<S-right>", label := "Shift key column right" },
  -- col sort
  { handler := "sort.asc",        key := "[", label := "Sort ascending", resetsVS := true },
  { handler := "sort.desc",       key := "]", label := "Sort descending", resetsVS := true },
  -- col arg commands
  { handler := "split",           key := ":", label := "Split column by delimiter", isArg := true },
  { handler := "derive",          key := "=", label := "Derive new column (name = expr)", isArg := true },
  { handler := "filter.colSearch", key := "g", label := "Jump to column by name", resetsVS := true, isArg := true },
  -- col plot
  { handler := "plot.area",       label := "Plot: area chart" },
  { handler := "plot.line",       label := "Plot: line chart" },
  { handler := "plot.scatter",    label := "Plot: scatter plot" },
  { handler := "plot.bar",        label := "Plot: bar chart" },
  { handler := "plot.box",        label := "Plot: boxplot" },
  { handler := "plot.step",       label := "Plot: step chart" },
  { handler := "plot.hist",       label := "Plot: histogram" },
  { handler := "plot.density",    label := "Plot: density plot" },
  { handler := "plot.violin",     label := "Plot: violin plot" },
  -- stk: view stack operations
  { handler := "menu",            key := " ", label := "Open command menu" },
  { handler := "stk.swap",        key := "S", label := "Swap top two views" },
  { handler := "stk.pop",         key := "q", label := "Close current view", resetsVS := true },
  { handler := "stk.dup",         label := "Duplicate current view" },
  { handler := "quit" },
  { handler := "xpose",           key := "X", label := "Transpose table (rows <-> columns)" },
  { handler := "diff",            key := "d", label := "Diff top two views" },
  -- info: precision, heatmap, scroll
  { handler := "infoTog",         key := "I", label := "Toggle info overlay" },
  { handler := "precDec",         label := "Decrease decimal precision" },
  { handler := "precInc",         label := "Increase decimal precision" },
  { handler := "prec0",           label := "Set precision to 0 decimals" },
  { handler := "precMax",         label := "Set precision to max (17)" },
  { handler := "scrollUp",        key := "{", label := "Scroll cell preview up" },
  { handler := "scrollDn",        key := "}", label := "Scroll cell preview down" },
  { handler := "heat.0",          label := "Heatmap: off" },
  { handler := "heat.1",          label := "Heatmap: numeric columns" },
  { handler := "heat.2",          label := "Heatmap: categorical columns" },
  { handler := "heat.3",          label := "Heatmap: all columns" },
  -- metaV: column metadata view
  { handler := "meta.push",       key := "M", label := "Open column metadata view", resetsVS := true },
  { handler := "meta.setKey",     key := "<ret>", label := "Set selected rows as key columns", resetsVS := true, viewCtx := "colMeta" },
  { handler := "meta.selNull",    key := "0", label := "Select columns with null values", resetsVS := true },
  { handler := "meta.selSingle",  key := "1", label := "Select columns with single value", resetsVS := true },
  -- freq: frequency table
  { handler := "freq.open",       key := "F", label := "Open frequency view", resetsVS := true },
  { handler := "freq.filter",     key := "<ret>", label := "Filter parent table by current row", resetsVS := true, viewCtx := "freqV" },
  -- fld: folder/file browser
  { handler := "folder.push",     key := "D", label := "Browse folder", resetsVS := true },
  { handler := "folder.enter",    key := "<ret>", label := "Open file or enter directory", resetsVS := true, viewCtx := "fld" },
  { handler := "folder.parent",   key := "<bs>", label := "Go to parent directory", resetsVS := true, viewCtx := "fld" },
  { handler := "folder.del",      label := "Move to trash", resetsVS := true },
  { handler := "folder.depthDec", label := "Decrease folder depth", resetsVS := true },
  { handler := "folder.depthInc", label := "Increase folder depth", resetsVS := true },
  -- arg-only commands
  { handler := "export",          key := "e", label := "Export table (csv/parquet/json/ndjson)", isArg := true },
  { handler := "sessSave",        key := "W", label := "Save session", isArg := true },
  { handler := "sessLoad",        label := "Load session", isArg := true },
  { handler := "join",            key := "J", label := "Join tables", isArg := true }
]

-- | Cached: (key, viewCtx) → CmdInfo — context-aware key lookup
initialize keyInfoMap : IO.Ref (Std.HashMap (String × String) CmdInfo) ← IO.mkRef {}
-- | Cached: handler name → CmdInfo (socket/programmatic dispatch)
initialize handlerInfoMap : IO.Ref (Std.HashMap String CmdInfo) ← IO.mkRef {}
-- | Cached: handlers that take user input (isArg = true)
initialize argHandlerSet : IO.Ref (Std.HashSet String) ← IO.mkRef {}

-- | Build caches from inline config. No DuckDB needed.
def init : IO Unit := do
  let mut keyInfo : Std.HashMap (String × String) CmdInfo := {}
  let mut handlerInfo : Std.HashMap String CmdInfo := {}
  let mut argSet : Std.HashSet String := {}
  for e in commands do
    let ci : CmdInfo := { handler := e.handler, resetsVS := e.resetsVS }
    if !e.key.isEmpty then keyInfo := keyInfo.insert (e.key, e.viewCtx) ci
    handlerInfo := handlerInfo.insert e.handler ci
    if e.isArg then argSet := argSet.insert e.handler
  keyInfoMap.set keyInfo
  handlerInfoMap.set handlerInfo
  argHandlerSet.set argSet
  Log.write "init" s!"commands: {commands.size} entries"

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

-- | Menu items for fzf, filtered by view context. Returns (handler, label).
def menuItems (viewCtx : String) : IO (Array (String × String)) :=
  pure (commands.filterMap fun e =>
    if e.label.isEmpty then none
    else if !e.viewCtx.isEmpty && e.viewCtx != viewCtx then none
    else some (e.handler, e.label))

end Tc.CmdConfig
