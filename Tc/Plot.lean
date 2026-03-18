/-
  Plot: export table data, render via ggplot2 (R), display via kitty graphics or viu.
  X-axis = first group column, category = second group column (optional),
  Y-axis = current column under cursor (must be numeric).
  Facet = third group column (optional, small multiples).
  Interactive: in-place re-rendering with interval and plot type cycling.
-/
import Tc.View
import Tc.Term
import Tc.Error
import Tc.Render
import Tc.TmpDir

namespace Tc.Plot

variable {T : Type} [TblOps T]

-- | Max data points for plot (more is slow and unreadable)
private def maxPoints : Nat := 2000

private def isTimeType (typ : String) : Bool :=
  typ == "time" || typ == "timestamp" || typ == "date"

-- | Downsampling interval for interactive plot control
structure Interval where
  label    : String  -- display label (e.g. "1s", "1m", "2x")
  truncLen : Nat     -- SUBSTRING length for time; step for non-time
  deriving Inhabited

private def timeIntervals : Array Interval := #[
  ⟨"1s", 8⟩, ⟨"1m", 5⟩, ⟨"1h", 2⟩
]

private def tsIntervals : Array Interval := #[
  ⟨"1s", 19⟩, ⟨"1m", 16⟩, ⟨"1h", 13⟩, ⟨"1d", 10⟩
]

private def dateIntervals : Array Interval := #[
  ⟨"1d", 10⟩, ⟨"1M", 7⟩, ⟨"1Y", 4⟩
]

private def stepIntervals (baseStep : Nat) : Array Interval :=
  let s0 := if baseStep == 0 then 1 else baseStep
  #[s0, s0 * 2, s0 * 4, s0 * 8, s0 * 16].map fun s => ⟨s!"{s}x", s⟩

private def getIntervals (xType : String) (baseStep : Nat) : Array Interval :=
  if xType == "time" then timeIntervals
  else if xType == "timestamp" then tsIntervals
  else if xType == "date" then dateIntervals
  else stepIntervals baseStep

-- | Try running cmd with args, return true if it ran successfully
private def tryDisplay (cmd : String) (args : Array String) : IO Bool := do
  let which ← IO.Process.output { cmd := "which", args := #[cmd] }
  if which.exitCode != 0 then return false
  let child ← IO.Process.spawn
    { cmd, args, stdin := .inherit, stdout := .inherit, stderr := .inherit }
  let rc ← child.wait
  pure (rc == 0)

-- | Display PNG: try kitten icat (kitty graphics), then viu, then xdg-open
private def showPng (png : String) : IO Unit := do
  if ← tryDisplay "kitten" #["icat", png] then return
  if ← tryDisplay "viu" #[png] then return
  let _ ← tryDisplay "xdg-open" #[png]

-- | ANSI: clear screen and move cursor to top-left
private def clearScreen : IO Unit := IO.print "\x1b[2J\x1b[H"

-- | Set terminal to raw mode (single keypress without Enter)
private def setRaw : IO Unit := do
  let _ ← Log.run "stty" "stty" #["-F", "/dev/tty", "raw", "-echo"]

-- | Restore terminal to normal mode
private def setSane : IO Unit := do
  let _ ← Log.run "stty" "stty" #["-F", "/dev/tty", "sane"]

-- | Read one byte from /dev/tty (caller must be in raw mode)
private def readKeyRaw : IO Char := do
  let tty ← IO.FS.Handle.mk "/dev/tty" .read
  let buf ← tty.read 1
  pure (if buf.size > 0 then Char.ofNat buf[0]!.toNat else 'q')

private def err (s : ViewStack T) (msg : String) : IO (Option (ViewStack T)) := do
  Log.write "plot" msg; errorPopup msg; pure (some s)

private def isNumericType (typ : String) : Bool :=
  typ == "int" || typ == "float" || typ == "decimal"

-- | Plot types that share the same x/y/cat data (cycleable with h/l)
private def cyclableKinds : Array PlotKind := #[.line, .scatter, .bar, .box]

private def cycleKind (k : PlotKind) (delta : Nat) : PlotKind :=
  let n := cyclableKinds.size
  match cyclableKinds.idxOf? k with
  | some i => cyclableKinds.getD ((i + delta) % n) .line
  | none => .line

-- | Generate R script for ggplot2 rendering
private def rScript (dataPath pngPath : String) (kind : PlotKind)
    (xName yName : String) (hasCat : Bool) (catName : String)
    (hasFacet : Bool) (facetName : String) (xIsTime : Bool) : String :=
  let rq (s : String) := s!"`{s}`"
  let xR := rq xName; let yR := rq yName; let catR := rq catName; let facetR := rq facetName
  let readData := s!"d <- read.delim('{dataPath}', header=TRUE, sep='\\t', colClasses='character', check.names=FALSE)\n"
  let convY := s!"d[['{yName}']] <- as.numeric(d[['{yName}']])\n"
  let convX := if xIsTime then s!"d[['{xName}']] <- as.POSIXct(d[['{xName}']])\n"
    else if kind != .hist then
      "tryCatch(d[['" ++ xName ++ "']] <- as.numeric(d[['" ++ xName ++ "']]), warning=function(w) NULL)\n"
    else ""
  let aes := match kind with
    | .hist => s!"aes(x = {yR})"
    | .box => if hasCat then s!"aes(x = {catR}, y = {yR})" else s!"aes(x = factor(''), y = {yR})"
    | _ => s!"aes(x = {xR}, y = {yR})"
  let colorAes := if hasCat && kind != .box then s!", color = {catR}" else ""
  let fillAes := if hasCat && kind == .box then s!", fill = {catR}" else ""
  let geom := match kind with
    | .line => "geom_line(linewidth = 0.5)"
    | .bar => "geom_col()"
    | .scatter => "geom_point(size = 1.5, alpha = 0.7)"
    | .hist => "geom_histogram(bins = 30)"
    | .box => "geom_boxplot()"
  let facet := if hasFacet then s!" + facet_wrap(vars({facetR}), scales = 'free_y')" else ""
  "library(ggplot2)\n" ++ readData ++ convY ++ convX ++
    s!"p <- ggplot(d, {aes}{colorAes}{fillAes}) + {geom}{facet} + " ++
    s!"labs(x = '{xName}', y = '{yName}') + theme_gray() + scale_color_viridis_d() + scale_fill_viridis_d()\n" ++
    s!"ggsave('{pngPath}', p, width = 12, height = 7, dpi = 100)\n"

-- | Run Rscript to render plot; returns error message on failure
private def renderR (script : String) : IO (Option String) := do
  let rPath ← Tc.tmpPath "plot.R"
  IO.FS.writeFile rPath script
  let r ← IO.Process.output { cmd := "Rscript", args := #[rPath] }
  if r.exitCode != 0 then
    let msg := s!"Rscript failed: {r.stderr.trimAscii.toString}"
    Log.write "plot" msg
    return some msg
  return none

-- | Export plot data with headers for R (prepends column names to plotExport output)
private def exportWithHeaders (t : T) (xName yName : String) (catName? : Option String)
    (xIsTime : Bool) (step truncLen : Nat) : IO (Option (Array String)) := do
  let cats ← TblOps.plotExport t xName yName catName? xIsTime step truncLen
  let some cats := cats | return none
  let datPath ← Tc.tmpPath "plot.dat"
  let content ← IO.FS.readFile datPath
  let header := match catName? with
    | some cn => s!"{xName}\t{yName}\t{cn}"
    | none => s!"{xName}\t{yName}"
  IO.FS.writeFile datPath (header ++ "\n" ++ content)
  return some cats

-- | Render plot image and status bar in-place (clear → image → status)
private def renderFrame (pngPath : String) (kind : PlotKind)
    (xName yName : String) (intervals : Array Interval) (idx : Nat)
    (err? : Option String) : IO Unit := do
  clearScreen
  if err?.isNone then showPng pngPath
  else IO.println (err?.getD "plot error")
  -- status bar: show all plot types with current highlighted
  let typeBar := String.intercalate " " (cyclableKinds.toList.map fun k =>
    if k == kind then s!"\x1b[1;7m {k} \x1b[0m" else s!" {k} ")
  IO.println s!"\x1b[1m─── x={xName}  y={yName} ───\x1b[0m"
  let ivLine := if intervals.size > 1 then
    let ivBar := String.intercalate " " (intervals.toList.mapIdx fun i iv =>
      if i == idx then s!"\x1b[1;7m {iv.label} \x1b[0m" else s!" {iv.label} ")
    s!"  ,/.:downsample {ivBar}"
  else ""
  IO.println s!"h/l:{typeBar}{ivLine}"
  IO.print "q:exit "

-- | Run plot with interactive controls (in-place re-rendering)
def run (s : ViewStack T) (kind : PlotKind) : IO (Option (ViewStack T)) := do
  Log.write "plot" s!"run entered, kind={repr kind}"
  let n := s.cur.nav
  let names := TblOps.colNames n.tbl
  -- histogram: no group col needed, just cursor col
  if kind == .hist then
    let yIdx := colIdxAt n.grp names n.col.cur.val
    let yName := names.getD yIdx ""
    let yType := TblOps.colType n.tbl yIdx
    if !isNumericType yType then return ← err s "histogram needs a numeric column"
    Term.shutdown
    let datPath ← Tc.tmpPath "plot.dat"
    let pngPath ← Tc.tmpPath "plot.png"
    let nr := min (TblOps.nRows n.tbl) maxPoints
    let cols ← TblOps.getCols n.tbl #[yIdx] 0 nr
    let vals := match cols.getD 0 default with
      | .strs vs => vs | .ints vs => vs.map toString | .floats vs => vs.map toString
    IO.FS.writeFile datPath (yName ++ "\n" ++ "\n".intercalate (vals.filter (!·.isEmpty)).toList ++ "\n")
    let script := rScript datPath pngPath kind "" yName false "" false "" false
    let err? ← renderR script
    clearScreen
    if err?.isNone then showPng pngPath
    else IO.println (err?.getD "plot error")
    IO.println s!"\x1b[1m─── histogram: {yName} ───\x1b[0m"
    IO.print "q: exit "
    setRaw
    let _ ← readKeyRaw
    setSane
    let _ ← Term.init
    return some s
  -- all other plots need at least 1 group column
  if n.grp.isEmpty then return ← err s "group a column first (!)"
  let xName := n.grp.getD 0 ""
  let some xIdx := names.idxOf? xName | return ← err s s!"x-axis column '{xName}' not found"
  let hasFacet := n.grp.size > 2
  let facetName := if hasFacet then n.grp.getD 1 "" else ""
  let catName := if hasFacet then n.grp.getD 2 "" else n.grp.getD 1 ""
  let exportCatName? := if n.grp.size > 2 then some facetName
    else if n.grp.size > 1 then some catName else none
  let yIdx := colIdxAt n.grp names n.col.cur.val
  let yName := names.getD yIdx ""
  if n.grp.contains yName then return ← err s "move cursor to a non-group column"
  let yType := TblOps.colType n.tbl yIdx
  if !isNumericType yType then return ← err s s!"y-axis '{yName}' must be numeric (got {yType})"
  let nr := TblOps.nRows n.tbl
  let xType0 := TblOps.colType n.tbl xIdx
  let xType ← do
    if xType0 != "str" then pure xType0
    else
      let cols ← TblOps.getCols n.tbl #[xIdx] 0 1
      let v := match cols.getD 0 default with
        | .strs vals => (vals.getD 0 "").trimAscii.toString
        | _ => ""
      let cs := v.toList
      let at_ (i : Nat) := cs.getD i ' '
      if cs.length >= 19 && at_ 4 == '-' && at_ 10 == ' ' then pure "timestamp"
      else if cs.length >= 10 && at_ 4 == '-' && at_ 7 == '-' then pure "date"
      else if cs.length >= 8 && at_ 2 == ':' && at_ 5 == ':' then pure "time"
      else pure xType0
  Log.write "plot" s!"xType={xType} (raw={xType0}) xIdx={xIdx} xName={xName}"
  let xIsTime := isTimeType xType
  let baseStep := if nr > maxPoints then nr / maxPoints else 1
  let hasCat := n.grp.size > 1 && !hasFacet
  let intervals := getIntervals xType baseStep
  let maxIdx := intervals.size - 1
  -- enter plot mode: shutdown TUI, set raw mode once
  Term.shutdown
  setRaw
  let datPath ← Tc.tmpPath "plot.dat"
  let pngPath ← Tc.tmpPath "plot.png"
  let mut idx : Nat := 0
  let mut curKind := kind
  let mut needExport := true  -- skip re-export when only plot type changes
  let mut continue_ := true
  while continue_ do
    let iv := intervals.getD idx default
    Log.write "plot" s!"kind={curKind} interval={iv.label} truncLen={iv.truncLen} idx={idx}"
    let exportResult ← do
      if needExport then
        try
          if let some _cats := ← exportWithHeaders n.tbl xName yName exportCatName? xIsTime baseStep iv.truncLen then
            pure (none : Option String)
          else pure (some "export returned no data")
        catch e => pure (some e.toString)
      else pure none
    let err? ← match exportResult with
      | some msg => pure (some msg)
      | none => renderR (rScript datPath pngPath curKind xName yName hasCat catName hasFacet facetName xIsTime)
    renderFrame pngPath curKind xName yName intervals idx err?
    let key ← readKeyRaw
    if key == 'q' then continue_ := false
    else if key == '.' || key == '>' then idx := min (idx + 1) maxIdx; needExport := true
    else if key == ',' || key == '<' then idx := if idx > 0 then idx - 1 else 0; needExport := true
    else if key == 'l' then curKind := cycleKind curKind 1; needExport := false
    else if key == 'h' then curKind := cycleKind curKind (cyclableKinds.size - 1); needExport := false
    else needExport := false  -- ignore unknown keys
  -- exit plot mode: restore terminal, re-init TUI
  setSane
  let _ ← Term.init
  pure (some s)

-- | Pure update: map Cmd to Effect
def update (s : ViewStack T) (cmd : Cmd) : Option (ViewStack T × Effect) :=
  match cmd with
  | .plot .inc => some (s, .plot .line)
  | .plot .dec => some (s, .plot .bar)
  | .plot .ent => some (s, .plot .scatter)
  | .plot .del => some (s, .plot .hist)
  | .plot .dup => some (s, .plot .box)
  | _ => none

end Tc.Plot
