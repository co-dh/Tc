/-
  Plot: export table data to gnuplot, display via viu.
  X-axis = first group column, category = second group column (optional),
  Y-axis = current column under cursor (must be numeric).
  Interactive: after display, +/− keys change downsampling interval.
-/
import Std.Data.HashMap
import Std.Data.HashSet
import Tc.View
import Tc.Term
import Tc.Error

namespace Tc.Plot

variable {T : Type} [TblOps T]

-- | Check if a column is numeric
private def isNumericCol (col : Column) : Bool :=
  match col with
  | .ints _   => true
  | .floats _ => true
  | .strs _   => false

-- | Max data points for gnuplot (more is slow and unreadable)
private def maxPoints : Nat := 2000

-- | Check if column type is time-like
private def isTimeType (typ : String) : Bool :=
  typ == "time" || typ == "timestamp"

-- | Check if y-value is zero (filter out no-bid/no-ask conditions)
private def isZeroY (yv : String) : Bool :=
  yv == "0" || yv == "0.0" || yv == "0.000000"

-- | Downsampling interval for interactive plot control
structure Interval where
  label    : String  -- display label (e.g. "1s", "1m", "2x")
  truncLen : Nat     -- SUBSTRING length for time; step for non-time
  timefmt  : String  -- gnuplot timefmt string
  xfmt     : String  -- gnuplot format x string
  deriving Inhabited

-- | Time intervals (colType = "time", format HH:MM:SS)
private def timeIntervals : Array Interval := #[
  ⟨"1s", 8, "%H:%M:%S", "%H:%M:%S"⟩,
  ⟨"1m", 5, "%H:%M", "%H:%M"⟩,
  ⟨"1h", 2, "%H", "%H"⟩
]

-- | Timestamp intervals (colType = "timestamp", format YYYY-MM-DD HH:MM:SS)
private def tsIntervals : Array Interval := #[
  ⟨"1s", 19, "%Y-%m-%d %H:%M:%S", "%H:%M"⟩,
  ⟨"1m", 16, "%Y-%m-%d %H:%M", "%H:%M"⟩,
  ⟨"1h", 13, "%Y-%m-%d %H", "%d %Hh"⟩,
  ⟨"1d", 10, "%Y-%m-%d", "%m-%d"⟩
]

-- | Build non-time interval array from base step
private def stepIntervals (baseStep : Nat) : Array Interval :=
  let s0 := if baseStep == 0 then 1 else baseStep
  #[s0, s0 * 2, s0 * 4, s0 * 8, s0 * 16].map fun s =>
    ⟨s!"{s}x", s, "", ""⟩

-- | Get interval array based on column type
private def getIntervals (xType : String) (baseStep : Nat) : Array Interval :=
  if xType == "time" then timeIntervals
  else if xType == "timestamp" then tsIntervals
  else stepIntervals baseStep

-- | Truncate time string to given length
private def truncTime (len : Nat) (s : String) : String :=
  if s.length > len then (s.take len).toString else s

-- | Export data to tab-separated file for gnuplot (Lean-side fallback)
-- cols order: [xCol, yCol] or [xCol, yCol, catCol]
-- truncLen: SUBSTRING length for time x-axis; step for non-time
private def exportData (cols : Array Column) (hasCat : Bool) (xIsTime : Bool) (truncLen : Nat)
    : IO (String × Option (Array String)) := do
  let xCol := cols.getD 0 default
  let yCol := cols.getD 1 default
  let nr := xCol.size
  let path := "/tmp/tc-plot.dat"
  if xIsTime then
    -- time x-axis: keep last value per truncated period
    let mut last : Std.HashMap String String := {}
    let mut lastCat : Std.HashMap String String := {}
    let mut cats : Std.HashSet String := {}
    let mut order : Array String := #[]
    for r in [:nr] do
      let xvRaw := (xCol.get r).toRaw
      let yv := (yCol.get r).toRaw
      if xvRaw.isEmpty || yv.isEmpty || isZeroY yv then continue
      let xv := truncTime truncLen xvRaw
      if hasCat then
        let cv := ((cols.getD 2 default).get r).toRaw
        if cv.isEmpty then continue
        let key := s!"{xv}\t{cv}"
        if !last.contains key then order := order.push key
        cats := cats.insert cv
        last := last.insert key yv
        lastCat := lastCat.insert key cv
      else
        if !last.contains xv then order := order.push xv
        last := last.insert xv yv
    let mut lines : Array String := #[]
    for key in order do
      if hasCat then
        lines := lines.push s!"{key.takeWhile (· != '\t')}\t{last.getD key ""}\t{lastCat.getD key ""}"
      else
        lines := lines.push s!"{key}\t{last.getD key ""}"
    IO.FS.writeFile path ("\n".intercalate lines.toList)
    let catArr := if cats.isEmpty then none else some cats.toArray
    pure (path, catArr)
  else
    -- non-time: even sampling with given step
    let step := if truncLen == 0 then 1 else truncLen
    let mut lines : Array String := #[]
    let mut cats : Std.HashSet String := {}
    let idxs := Array.range ((nr + step - 1) / step) |>.map (· * step)
    for r in idxs do
      let xv := (xCol.get r).toRaw
      let yv := (yCol.get r).toRaw
      if xv.isEmpty || yv.isEmpty || isZeroY yv then continue
      if hasCat then
        let cv := ((cols.getD 2 default).get r).toRaw
        if cv.isEmpty then continue
        cats := cats.insert cv
        lines := lines.push s!"{xv}\t{yv}\t{cv}"
      else
        lines := lines.push s!"{xv}\t{yv}"
    IO.FS.writeFile path ("\n".intercalate lines.toList)
    let catArr := if cats.isEmpty then none else some cats.toArray
    pure (path, catArr)

-- | Max number of x-axis labels before thinning
private def maxLabels : Nat := 40

-- | Generate gnuplot xtic expression that thins labels for string x-axis
-- Shows label every N-th point; empty string for others
private def thinXtic (nPoints : Nat) : String :=
  let stride := if nPoints > maxLabels then nPoints / maxLabels else 1
  if stride <= 1 then "xtic(1)"
  else s!"(int($0) % {stride} == 0 ? stringcolumn(1) : \"\")"

-- | Generate gnuplot script (ggplot-minimal style)
-- timefmt/xfmt: gnuplot time format strings (from Interval)
-- nPoints: data point count (for thinning string x-axis labels)
private def gnuplotScript (dataPath : String) (xName yName : String)
    (catVals : Option (Array String)) (bar : Bool) (xIsStr xIsTime : Bool)
    (timefmt xfmt : String) (nPoints : Nat) : String :=
  let pngPath := "/tmp/tc-plot.png"
  -- terminal + data
  let header := "set terminal pngcairo size 1200,700 enhanced font 'Sans,11'\n" ++
    s!"set output '{pngPath}'\nset datafile separator '\\t'\n"
  -- ggplot-minimal style: light grey grid, no top/right border
  let style := "set border 3 lc rgb '#333333'\n" ++
    "set grid ytics lc rgb '#e0e0e0' lt 1\nset grid xtics lc rgb '#e0e0e0' lt 1\n" ++
    "set tics nomirror\nset key outside right top\n"
  let labels := s!"set xlabel '{xName}' offset 0,0.5\nset ylabel '{yName}' offset 1.5,0\n"
  -- x-axis: time parsing or categorical
  let xsetup := if xIsTime then
      s!"set xdata time\nset timefmt \"{timefmt}\"\nset format x \"{xfmt}\"\n"
    else if xIsStr then "set xtics rotate by -45\n" else ""
  -- xtic expression for string axes (thins labels when too many points)
  let xt := thinXtic nPoints
  -- plot commands
  let lineColor := "rgb '#4682B4'"  -- steelblue
  let plotCmd := match catVals, bar with
    | none, false =>
      if xIsTime then
        s!"plot '{dataPath}' using 1:2 with lines title '{yName}' lw 1.5 lc {lineColor}\n"
      else if xIsStr then
        s!"plot '{dataPath}' using 2:{xt} with lines title '{yName}' lw 1.5 lc {lineColor}\n"
      else
        s!"plot '{dataPath}' using 1:2 with lines title '{yName}' lw 1.5 lc {lineColor}\n"
    | none, true =>
      "set style data histogram\nset style histogram clustered\n" ++
      s!"set style fill solid 0.8 border -1\nset boxwidth 0.8\n" ++
      s!"plot '{dataPath}' using 2:{xt} title '{yName}' lc {lineColor}\n"
    | some cats, false =>
      let catList := " ".intercalate cats.toList
      if xIsTime then
        s!"plot for [cat in \"{catList}\"] '{dataPath}' " ++
        s!"using 1:(stringcolumn(3) eq cat ? $2 : 1/0) with lines title cat lw 1.5\n"
      else
        s!"plot for [cat in \"{catList}\"] '{dataPath}' " ++
        s!"using (stringcolumn(3) eq cat ? $2 : 1/0):{xt} with lines title cat lw 1.5\n"
    | some cats, true =>
      let catList := " ".intercalate cats.toList
      "set style data histogram\nset style histogram clustered\n" ++
      "set style fill solid 0.8 border -1\nset boxwidth 0.8\n" ++
      s!"plot for [cat in \"{catList}\"] '{dataPath}' " ++
      s!"using (stringcolumn(3) eq cat ? $2 : 1/0):{xt} title cat\n"
  header ++ style ++ labels ++ xsetup ++ plotCmd

-- | Display PNG via viu (no wait — caller handles key reading)
private def showPng (png : String) : IO Unit := do
  let viu ← IO.Process.output { cmd := "which", args := #["viu"] }
  if viu.exitCode == 0 then
    let child ← IO.Process.spawn
      { cmd := "viu", args := #[png]
        stdin := .inherit, stdout := .inherit, stderr := .inherit }
    let _ ← child.wait
  else
    let child ← IO.Process.spawn
      { cmd := "xdg-open", args := #[png]
        stdin := .inherit, stdout := .inherit, stderr := .inherit }
    let _ ← child.wait

-- | Read one key from /dev/tty
private def readKey : IO Char := do
  let tty ← IO.FS.Handle.mk "/dev/tty" .read
  let buf ← tty.read 1
  pure (if buf.size > 0 then Char.ofNat buf[0]!.toNat else 'q')

-- | Log error and return current stack unchanged
private def err (s : ViewStack T) (msg : String) : IO (Option (ViewStack T)) := do
  Log.write "plot" msg; pure (some s)

-- | Check if column type name is numeric
private def isNumericType (typ : String) : Bool :=
  typ == "int" || typ == "float" || typ == "decimal"

-- | Render gnuplot script and run it, return false on error
private def renderGnuplot (script : String) : IO Bool := do
  IO.FS.writeFile "/tmp/tc-plot.gp" script
  let gp ← IO.Process.output { cmd := "gnuplot", args := #["/tmp/tc-plot.gp"] }
  if gp.exitCode != 0 then
    Log.write "plot" s!"gnuplot failed: {gp.stderr.trimAscii.toString}"
    return false
  return true

-- | Run plot with interactive interval control
-- After displaying, press + or - to change interval and re-render in place.
def run (s : ViewStack T) (bar : Bool) : IO (Option (ViewStack T)) := do
  Log.write "plot" "run entered"
  let n := s.cur.nav
  let names := TblOps.colNames n.tbl
  -- need at least 1 group column
  if n.grp.isEmpty then return ← err s "group a column first (!)"
  -- x-axis = first group column
  let xName := n.grp.getD 0 ""
  let some xIdx := names.idxOf? xName | return ← err s s!"x-axis column '{xName}' not found"
  -- category = second group column (optional)
  let catName := n.grp.getD 1 ""
  let catIdx := if n.grp.size > 1 then names.idxOf? catName else none
  -- y-axis = current column
  let yIdx := colIdxAt n.grp names n.col.cur.val
  let yName := names.getD yIdx ""
  -- skip if y is a group column (nothing to plot)
  if n.grp.contains yName then return ← err s "move cursor to a non-group column"
  let nr := TblOps.nRows n.tbl
  let xType := TblOps.colType n.tbl xIdx
  let xIsTime := isTimeType xType
  let xIsStr := !isNumericType xType
  let baseStep := if nr > maxPoints then nr / maxPoints else 1
  let catName? := if catIdx.isSome then some catName else none
  -- build interval array
  let intervals := getIntervals xType baseStep
  -- for Lean-side fallback: preload columns once
  let colIdxs := match catIdx with
    | some ci => #[xIdx, yIdx, ci]
    | none    => #[xIdx, yIdx]
  -- check y is numeric (for fallback path)
  let hasFallbackCols := do
    let cols ← TblOps.getCols n.tbl colIdxs 0 nr
    if cols.size < 2 then return none
    if !isNumericCol (cols.getD 1 default) then return none
    return some cols
  -- shutdown TUI once for the interactive loop
  Term.shutdown
  let mut idx : Nat := 0  -- start at finest interval
  let mut continue_ := true
  while continue_ do
    let iv := intervals.getD idx default
    Log.write "plot" s!"interval={iv.label} truncLen={iv.truncLen} idx={idx}"
    -- try DB-side export
    let dbOk ← do
      if let some cats := ← TblOps.plotExport n.tbl xName yName catName? xIsTime baseStep iv.truncLen then
        let catVals := if cats.isEmpty then none else some cats
        let script := gnuplotScript "/tmp/tc-plot.dat" xName yName catVals bar xIsStr xIsTime iv.timefmt iv.xfmt nr
        if ← renderGnuplot script then
          showPng "/tmp/tc-plot.png"
          pure true
        else pure false
      else pure false
    -- fallback: Lean-side export
    if !dbOk then
      if let some cols := ← hasFallbackCols then
        let (dataPath, catVals) ← exportData cols catIdx.isSome xIsTime iv.truncLen
        let script := gnuplotScript dataPath xName yName catVals bar xIsStr xIsTime iv.timefmt iv.xfmt nr
        if ← renderGnuplot script then
          showPng "/tmp/tc-plot.png"
        else
          IO.println s!"plot error (see /tmp/tc-plot.log)"
      else
        IO.println "failed to extract columns or y-axis not numeric"
    -- show info line: x=col y=col [1s 1m 1h] +/-:interval q:exit
    let maxIdx := intervals.size - 1
    let bar_ := if bar then "bar" else "line"
    let ivLabels := intervals.map (·.label)
    let ivBar := String.intercalate " " (ivLabels.toList.mapIdx fun i l =>
      if i == idx then s!"\x1b[1;7m {l} \x1b[0m" else s!" {l} ")
    IO.println s!"─── x={xName}  y={yName}  {bar_} ───"
    IO.print s!"{ivBar}  +\x2f-: interval  any key: exit "
    let key ← readKey
    IO.println ""
    if key == '+' || key == '=' then
      idx := min (idx + 1) maxIdx
    else if key == '-' || key == '_' then
      idx := if idx > 0 then idx - 1 else 0
    else
      continue_ := false
  let _ ← Term.init
  pure (some s)

-- | Pure update: map Cmd to Effect
def update (s : ViewStack T) (cmd : Cmd) : Option (ViewStack T × Effect) :=
  match cmd with
  | .plot .inc => some (s, .plotLine)
  | .plot .dec => some (s, .plotBar)
  | _ => none

-- | Execute (IO version for backward compat)
def exec (s : ViewStack T) (cmd : Cmd) : IO (Option (ViewStack T)) := do
  match cmd with
  | .plot .inc => run s false
  | .plot .dec => run s true
  | _ => pure none

end Tc.Plot
