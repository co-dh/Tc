/-
  Plot: export table data to gnuplot, display via viu.
  X-axis = first group column, category = second group column (optional),
  Y-axis = current column under cursor (must be numeric).
  Interactive: after display, +/− keys change downsampling interval.
-/
import Tc.View
import Tc.Term
import Tc.Error
import Tc.TmpDir

namespace Tc.Plot

variable {T : Type} [TblOps T]

-- | Max data points for gnuplot (more is slow and unreadable)
private def maxPoints : Nat := 2000

-- | Check if column type is time-like
private def isTimeType (typ : String) : Bool :=
  typ == "time" || typ == "timestamp" || typ == "date"

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

-- | Date intervals (colType = "date", format YYYY-MM-DD)
private def dateIntervals : Array Interval := #[
  ⟨"1d", 10, "%Y-%m-%d", "%Y-%m-%d"⟩,
  ⟨"1M", 7, "%Y-%m", "%Y-%m"⟩,
  ⟨"1Y", 4, "%Y", "%Y"⟩
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
  else if xType == "date" then dateIntervals
  else stepIntervals baseStep

-- | Max number of x-axis labels before thinning
private def maxLabels : Nat := 40

-- | Generate gnuplot xtic expression that thins labels for string x-axis
-- Shows label every N-th point; empty string for others
private def thinXtic (nPoints : Nat) : String :=
  let stride := if nPoints > maxLabels then nPoints / maxLabels else 1
  if stride <= 1 then "xtic(1)"
  else s!"xtic(int($0) % {stride} == 0 ? stringcolumn(1) : \"\")"

-- | Generate gnuplot script (ggplot-minimal style)
-- timefmt/xfmt: gnuplot time format strings (from Interval)
-- nPoints: data point count (for thinning string x-axis labels)
private def gnuplotScript (dataPath pngPath : String) (xName yName : String)
    (catVals : Option (Array String)) (bar : Bool) (xIsStr xIsTime : Bool)
    (timefmt xfmt : String) (nPoints : Nat) : String :=
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
  -- set raw mode so we get single keypress without Enter
  let _ ← Log.run "stty" "stty" #["-F", "/dev/tty", "raw", "-echo"]
  let tty ← IO.FS.Handle.mk "/dev/tty" .read
  let buf ← tty.read 1
  -- restore cooked mode
  let _ ← Log.run "stty" "stty" #["-F", "/dev/tty", "sane"]
  pure (if buf.size > 0 then Char.ofNat buf[0]!.toNat else 'q')

-- | Log error and return current stack unchanged
private def err (s : ViewStack T) (msg : String) : IO (Option (ViewStack T)) := do
  Log.write "plot" msg; pure (some s)

-- | Check if column type name is numeric
private def isNumericType (typ : String) : Bool :=
  typ == "int" || typ == "float" || typ == "decimal"

-- | Render gnuplot script and run it, return false on error
private def renderGnuplot (script : String) : IO Bool := do
  let gpPath ← Tc.tmpPath "plot.gp"
  IO.FS.writeFile gpPath script
  let gp ← IO.Process.output { cmd := "gnuplot", args := #[gpPath] }
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
  let xType0 := TblOps.colType n.tbl xIdx
  -- infer date/time from string values (e.g. "2024-01-15" → "date")
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
  let xIsStr := !isNumericType xType
  let baseStep := if nr > maxPoints then nr / maxPoints else 1
  let catName? := if catIdx.isSome then some catName else none
  -- build interval array
  let intervals := getIntervals xType baseStep
  -- shutdown TUI once for the interactive loop
  Term.shutdown
  let mut idx : Nat := 0  -- start at finest interval
  let mut continue_ := true
  while continue_ do
    let iv := intervals.getD idx default
    Log.write "plot" s!"interval={iv.label} truncLen={iv.truncLen} idx={idx}"
    -- DB-side export via PRQL
    let ok ← do
      if let some cats := ← TblOps.plotExport n.tbl xName yName catName? xIsTime baseStep iv.truncLen then
        let catVals := if cats.isEmpty then none else some cats
        let datPath ← Tc.tmpPath "plot.dat"
        let pngPath ← Tc.tmpPath "plot.png"
        let script := gnuplotScript datPath pngPath xName yName catVals bar xIsStr xIsTime iv.timefmt iv.xfmt nr
        if ← renderGnuplot script then showPng pngPath; pure true
        else pure false
      else pure false
    if !ok then IO.println "plot error (see /tmp/tc.log)"
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
  | .plot .inc => some (s, .plot .line)
  | .plot .dec => some (s, .plot .bar)
  | _ => none

end Tc.Plot
