/-
  Plot: export table data to gnuplot, display via viu.
  X-axis = first group column, category = second group column (optional),
  Y-axis = current column under cursor (must be numeric).
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

-- | Truncate time string to HH:MM:SS (drop sub-second fraction)
private def truncSec (s : String) : String :=
  if s.length > 8 then (s.take 8).toString else s

-- | Check if y-value is zero (filter out no-bid/no-ask conditions)
private def isZeroY (yv : String) : Bool :=
  yv == "0" || yv == "0.0" || yv == "0.000000"

-- | Export data to tab-separated file for gnuplot
-- cols order: [xCol, yCol] or [xCol, yCol, catCol]
-- Filters zero y-values. For time x-axis: last value per second.
-- For non-time: even sampling when too many points.
private def exportData (cols : Array Column) (hasCat : Bool) (xIsTime : Bool)
    : IO (String × Option (Array String)) := do
  let xCol := cols.getD 0 default
  let yCol := cols.getD 1 default
  let nr := xCol.size
  let path := "/tmp/tc-plot.dat"
  if xIsTime then
    -- time x-axis: keep last value per second (keyed by x string)
    let mut last : Std.HashMap String String := {}
    let mut lastCat : Std.HashMap String String := {}
    let mut cats : Std.HashSet String := {}
    let mut order : Array String := #[]
    for r in [:nr] do
      let xvRaw := (xCol.get r).toRaw
      let yv := (yCol.get r).toRaw
      if xvRaw.isEmpty || yv.isEmpty || isZeroY yv then continue
      let xv := truncSec xvRaw
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
    -- non-time: even sampling
    let step := if nr > maxPoints then nr / maxPoints else 1
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

-- | Generate gnuplot script (ggplot-minimal style)
private def gnuplotScript (dataPath : String) (xName yName : String)
    (catVals : Option (Array String)) (bar : Bool) (xIsStr xIsTime : Bool) : String :=
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
      "set xdata time\nset timefmt \"%H:%M:%S\"\nset format x \"%H:%M\"\n"
    else if xIsStr then "set xtics rotate by -45\n" else ""
  -- plot commands
  let lineColor := "rgb '#4682B4'"  -- steelblue
  let plotCmd := match catVals, bar with
    | none, false =>
      if xIsTime then
        s!"plot '{dataPath}' using 1:2 with lines title '{yName}' lw 1.5 lc {lineColor}\n"
      else if xIsStr then
        s!"plot '{dataPath}' using 2:xtic(1) with lines title '{yName}' lw 1.5 lc {lineColor}\n"
      else
        s!"plot '{dataPath}' using 1:2 with lines title '{yName}' lw 1.5 lc {lineColor}\n"
    | none, true =>
      "set style data histogram\nset style histogram clustered\n" ++
      s!"set style fill solid 0.8 border -1\nset boxwidth 0.8\n" ++
      (if xIsTime then
        s!"plot '{dataPath}' using 2:xtic(1) title '{yName}' lc {lineColor}\n"
      else
        s!"plot '{dataPath}' using 2:xtic(1) title '{yName}' lc {lineColor}\n")
    | some cats, false =>
      let catList := " ".intercalate cats.toList
      if xIsTime then
        s!"plot for [cat in \"{catList}\"] '{dataPath}' " ++
        s!"using 1:(stringcolumn(3) eq cat ? $2 : 1/0) with lines title cat lw 1.5\n"
      else
        s!"plot for [cat in \"{catList}\"] '{dataPath}' " ++
        s!"using (stringcolumn(3) eq cat ? $2 : 1/0):xtic(1) with lines title cat lw 1.5\n"
    | some cats, true =>
      let catList := " ".intercalate cats.toList
      "set style data histogram\nset style histogram clustered\n" ++
      "set style fill solid 0.8 border -1\nset boxwidth 0.8\n" ++
      s!"plot for [cat in \"{catList}\"] '{dataPath}' " ++
      s!"using (stringcolumn(3) eq cat ? $2 : 1/0):xtic(1) title cat\n"
  header ++ style ++ labels ++ xsetup ++ plotCmd

-- | Display PNG via viu (fallback to xdg-open), wait for keypress
private def displayPng (png : String) : IO Unit := do
  let viu ← IO.Process.output { cmd := "which", args := #["viu"] }
  if viu.exitCode == 0 then
    -- viu needs direct terminal access (inherited stdio)
    let child ← IO.Process.spawn
      { cmd := "viu", args := #[png]
        stdin := .inherit, stdout := .inherit, stderr := .inherit }
    let _ ← child.wait
  else
    let child ← IO.Process.spawn
      { cmd := "xdg-open", args := #[png]
        stdin := .inherit, stdout := .inherit, stderr := .inherit }
    let _ ← child.wait
  -- wait for keypress before restoring TUI
  let tty ← IO.FS.Handle.mk "/dev/tty" .read
  let _ ← tty.read 1

-- | Log error and return current stack unchanged
private def err (s : ViewStack T) (msg : String) : IO (Option (ViewStack T)) := do
  Log.write "plot" msg; pure (some s)

-- | Check if column type name is numeric
private def isNumericType (typ : String) : Bool :=
  typ == "int" || typ == "float" || typ == "decimal"

-- | Run gnuplot and display result
private def plotAndShow (s : ViewStack T) (dataPath xName yName : String)
    (catVals : Option (Array String)) (bar xIsStr xIsTime : Bool)
    : IO (Option (ViewStack T)) := do
  let script := gnuplotScript dataPath xName yName catVals bar xIsStr xIsTime
  IO.FS.writeFile "/tmp/tc-plot.gp" script
  Log.write "plot" "running gnuplot..."
  let gp ← IO.Process.output { cmd := "gnuplot", args := #["/tmp/tc-plot.gp"] }
  Log.write "plot" s!"gnuplot exit={gp.exitCode}"
  if gp.exitCode != 0 then return ← err s s!"gnuplot failed: {gp.stderr.trimAscii.toString}"
  Log.write "plot" "displaying..."
  Term.shutdown
  displayPng "/tmp/tc-plot.png"
  let _ ← Term.init
  pure (some s)

-- | Run plot: export data, generate gnuplot script, render, display
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
  let xIsTime := isTimeType (TblOps.colType n.tbl xIdx)
  -- try DB-side export (downsample in SQL, COPY to file)
  let step := if nr > maxPoints then nr / maxPoints else 1
  let catName? := if catIdx.isSome then some catName else none
  if let some cats := ← TblOps.plotExport n.tbl xName yName catName? xIsTime step then
    Log.write "plot" s!"DB export done, cats={cats.size}"
    let xIsStr := !isNumericType (TblOps.colType n.tbl xIdx)
    let catVals := if cats.isEmpty then none else some cats
    return ← plotAndShow s "/tmp/tc-plot.dat" xName yName catVals bar xIsStr xIsTime
  -- fallback: fetch columns and export in Lean
  let colIdxs := match catIdx with
    | some ci => #[xIdx, yIdx, ci]
    | none    => #[xIdx, yIdx]
  Log.write "plot" s!"getCols idxs={colIdxs} nr={nr} xIdx={xIdx} yIdx={yIdx}"
  let cols ← TblOps.getCols n.tbl colIdxs 0 nr
  Log.write "plot" s!"getCols returned {cols.size} columns"
  if cols.size < 2 then return ← err s "failed to extract columns"
  -- y-axis must be numeric
  if !isNumericCol (cols.getD 1 default) then return ← err s s!"y-axis '{yName}' is not numeric"
  let xIsStr := !isNumericCol (cols.getD 0 default)
  Log.write "plot" "exporting data..."
  let (dataPath, catVals) ← exportData cols catIdx.isSome xIsTime
  Log.write "plot" s!"exported to {dataPath} xIsTime={xIsTime}"
  plotAndShow s dataPath xName yName catVals bar xIsStr xIsTime

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
