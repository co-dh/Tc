/-
  Command config: inline Lean array, no SQL/DuckDB dependency.
  Single source of truth for the command matrix.
  Lookup by key char or handler name.
-/
import Std.Data.HashMap
import Tc.Util

namespace Tc.CmdConfig

-- | Lookup result for dispatch
structure CmdInfo where
  handler : String
  resetsVS : Bool

-- | Entry for menus and arg shortcuts
structure Entry where
  handler : String
  key : Option Char := none
  label : String := ""      -- fzf menu label (empty = hidden from menu)
  resetsVS : Bool := false
  viewCtx : String := ""    -- menu filter context (empty = all)

-- | Command matrix — single source of truth
def commands : Array Entry := #[
  -- row navigation (hjkl and arrows handled in Key.lean)
  { handler := "nav.rowInc" },
  { handler := "nav.rowDec" },
  { handler := "nav.rowPgUp" },
  { handler := "nav.rowPgDn" },
  { handler := "nav.rowTop" },
  { handler := "nav.rowBot" },
  { handler := "nav.rowSel",      key := 'T', label := "Select/deselect current row" },
  -- row search/filter
  { handler := "filter.rowSearch", key := '/', label := "Search for value in current column", resetsVS := true },
  { handler := "filter.rowFilter", key := '\\', label := "Filter rows by PRQL expression", resetsVS := true },
  { handler := "filter.searchNext",key := 'n', label := "Jump to next search match" },
  { handler := "filter.searchPrev",key := 'N', label := "Jump to previous search match" },
  -- col navigation
  { handler := "nav.colInc" },
  { handler := "nav.colDec" },
  { handler := "nav.colFirst" },
  { handler := "nav.colLast" },
  { handler := "nav.colGrp",      key := '!', label := "Toggle group on current column" },
  { handler := "nav.colHide",     key := 'H', label := "Hide/unhide current column" },
  { handler := "nav.colExclude",  key := 'x', label := "Delete column(s) from query", resetsVS := true },
  { handler := "nav.colShiftL",   label := "Shift key column left" },
  { handler := "nav.colShiftR",   label := "Shift key column right" },
  -- col sort
  { handler := "sort.asc",        key := '[', label := "Sort ascending", resetsVS := true },
  { handler := "sort.desc",       key := ']', label := "Sort descending", resetsVS := true },
  -- col arg commands
  { handler := "split",           key := ':', label := "Split column by delimiter" },
  { handler := "derive",          key := '=', label := "Derive new column (name = expr)" },
  { handler := "filter.colSearch", key := 'g', label := "Jump to column by name", resetsVS := true },
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
  { handler := "menu",            key := ' ', label := "Open command menu" },
  { handler := "stk.swap",        key := 'S', label := "Swap top two views" },
  { handler := "stk.pop",         key := 'q', label := "Close current view", resetsVS := true },
  { handler := "stk.dup",         label := "Duplicate current view" },
  { handler := "quit" },
  { handler := "xpose",           key := 'X', label := "Transpose table (rows <-> columns)" },
  { handler := "diff",            key := 'd', label := "Diff top two views" },
  -- info: precision, heatmap, scroll
  { handler := "infoTog",         key := 'I', label := "Toggle info overlay" },
  { handler := "precDec",         label := "Decrease decimal precision" },
  { handler := "precInc",         label := "Increase decimal precision" },
  { handler := "prec0",           label := "Set precision to 0 decimals" },
  { handler := "precMax",         label := "Set precision to max (17)" },
  { handler := "scrollUp",        key := '{', label := "Scroll cell preview up" },
  { handler := "scrollDn",        key := '}', label := "Scroll cell preview down" },
  { handler := "heat.0",          label := "Heatmap: off" },
  { handler := "heat.1",          label := "Heatmap: numeric columns" },
  { handler := "heat.2",          label := "Heatmap: categorical columns" },
  { handler := "heat.3",          label := "Heatmap: all columns" },
  -- metaV: column metadata view
  { handler := "meta.push",       key := 'M', label := "Open column metadata view", resetsVS := true },
  { handler := "meta.setKey",     label := "Set selected rows as key columns", resetsVS := true, viewCtx := "colMeta" },
  { handler := "meta.selNull",    key := '0', label := "Select columns with null values", resetsVS := true },
  { handler := "meta.selSingle",  key := '1', label := "Select columns with single value", resetsVS := true },
  -- freq: frequency table
  { handler := "freq.open",       key := 'F', label := "Open frequency view", resetsVS := true },
  { handler := "freq.filter",     label := "Filter parent table by current row", resetsVS := true, viewCtx := "freqV" },
  -- fld: folder/file browser
  { handler := "folder.push",     key := 'D', label := "Browse folder", resetsVS := true },
  { handler := "folder.enter",    label := "Open file or enter directory", resetsVS := true },
  { handler := "folder.parent",   label := "Go to parent directory", resetsVS := true },
  { handler := "folder.del",      label := "Move to trash", resetsVS := true },
  { handler := "folder.depthDec", label := "Decrease folder depth", resetsVS := true },
  { handler := "folder.depthInc", label := "Increase folder depth", resetsVS := true },
  -- arg-only commands
  { handler := "export",          key := 'e', label := "Export table (csv/parquet/json/ndjson)" },
  { handler := "sessSave",        key := 'W', label := "Save session" },
  { handler := "sessLoad",        label := "Load session" },
  { handler := "join",            key := 'J', label := "Join tables" }
]

-- | Cached: key char → CmdInfo (physical key → handler)
initialize keyInfoMap : IO.Ref (Std.HashMap Char CmdInfo) ← IO.mkRef {}
-- | Cached: handler name → CmdInfo (socket/programmatic dispatch)
initialize handlerInfoMap : IO.Ref (Std.HashMap String CmdInfo) ← IO.mkRef {}

-- | Build caches from inline config. No DuckDB needed.
def init : IO Unit := do
  let mut keyInfo : Std.HashMap Char CmdInfo := {}
  let mut handlerInfo : Std.HashMap String CmdInfo := {}
  for e in commands do
    let ci : CmdInfo := { handler := e.handler, resetsVS := e.resetsVS }
    if let some k := e.key then keyInfo := keyInfo.insert k ci
    handlerInfo := handlerInfo.insert e.handler ci
  keyInfoMap.set keyInfo
  handlerInfoMap.set handlerInfo
  Log.write "init" s!"commands: {commands.size} entries"

-- | O(1) lookup by physical key char → handler + resetsVS.
def keyLookup (c : Char) : IO (Option CmdInfo) := do
  pure ((← keyInfoMap.get).get? c)

-- | O(1) lookup by handler name → CmdInfo. For socket/programmatic dispatch.
def handlerLookup (h : String) : IO CmdInfo := do
  pure ((← handlerInfoMap.get).getD h { handler := h, resetsVS := false })

-- | Menu items for fzf, filtered by view context. Returns (handler, label).
def menuItems (viewCtx : String) : IO (Array (String × String)) :=
  pure (commands.filterMap fun e =>
    if e.label.isEmpty then none
    else if !e.viewCtx.isEmpty && e.viewCtx != viewCtx then none
    else some (e.handler, e.label))

end Tc.CmdConfig
