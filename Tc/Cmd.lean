/-
  Command system: flat (obj, verb, arg) structure.
  Config source of truth: cfg/commands.sql
-/

-- | Parse typeclass (inverse of ToString)
class Parse (α : Type) where
  parse? : String → Option α

-- | Command: flat (obj, verb, arg) structure
structure Cmd where
  obj  : Char
  verb : Char
  arg  : String := ""
  deriving Repr, BEq, DecidableEq, Inhabited

namespace Cmd

-- Wire protocol: valid chars for 2-char obj+verb commands.
-- These define the PARSE FORMAT, not the command set — the SQL table defines what commands exist.
def isObj (c : Char) : Bool := "rcsiMFD".toList.contains c
def isVerb (c : Char) : Bool := "><~-+/\\[]{}:=!".toList.contains c || c.isDigit

-- Wire protocol: arg prefix → (obj, verb). Defines how socket/test strings are parsed.
-- 'g' = go-to-column (not 's' which clashes with stk obj char)
private def argMap : Array (Char × Char × Char) := #[
  (':', 'c', ':'), ('=', 'c', '='),
  ('\\', 'r', '\\'), ('/', 'r', '/'),
  ('g', 'c', '/'),
  ('e', 'e', '~'), ('W', 'W', '~'), ('L', 'L', '~'), ('J', 'J', '~')
]

def argObj (c : Char) : Option (Char × Char) :=
  argMap.findSome? fun (p, o, v) => if c == p then some (o, v) else none

def argPfx (o v : Char) : Option Char :=
  argMap.findSome? fun (p, o', v') => if o == o' && v == v' then some p else none

def isArgPfx (c : Char) : Bool := (argObj c).isSome

def fromArg (pfx : Char) (arg : String) : Cmd :=
  match argObj pfx with
  | some (o, v) => { obj := o, verb := v, arg }
  | none => { obj := pfx, verb := '~', arg }

-- Helper constructors
def row (v : Char) (arg := "") : Cmd := { obj := 'r', verb := v, arg }
def col (v : Char) (arg := "") : Cmd := { obj := 'c', verb := v, arg }
def stk (v : Char) (arg := "") : Cmd := { obj := 's', verb := v, arg }
def info (v : Char) (arg := "") : Cmd := { obj := 'i', verb := v, arg }
def metaV (v : Char) (arg := "") : Cmd := { obj := 'M', verb := v, arg }
def freq (v : Char) (arg := "") : Cmd := { obj := 'F', verb := v, arg }
def fld (v : Char) (arg := "") : Cmd := { obj := 'D', verb := v, arg }

instance : ToString Cmd where
  toString c :=
    if c.arg.isEmpty then s!"{c.obj}{c.verb}"
    else match argPfx c.obj c.verb with
      | some pfx => s!"{pfx}{c.arg}"
      | none => s!"{c.obj}{c.verb}{c.arg}"

instance : Parse Cmd where
  parse? s := do
    if let [o, v] := s.toList then
      if isObj o && isVerb v then return { obj := o, verb := v }
    if s.length > 1 then
      let pfx := s.front
      if let some (o, v) := argObj pfx then
        return { obj := o, verb := v, arg := (s.drop 1).toString }
    none

end Cmd

-- | Handler enum: typed dispatch target, replaces stringly-typed handler names.
-- Flat enum matching handler column in cfg/commands.sql.
inductive Handler where
  -- nav
  | rowInc | rowDec | rowPgUp | rowPgDn | rowTop | rowBot | rowSel
  | colInc | colDec | colFirst | colLast | colGrp | colHide | colExclude | colShiftL | colShiftR
  -- sort
  | sortAsc | sortDesc
  -- filter
  | colSearch | rowSearch | rowFilter | searchNext | searchPrev
  -- plot
  | plotArea | plotLine | plotScatter | plotBar | plotBox | plotStep | plotHist | plotDensity | plotViolin
  -- stk
  | stkDup | stkPop | stkSwap
  -- top-level
  | quit | xpose | diff | menu
  -- info/prec/scroll/heat
  | infoTog | precDec | precInc | prec0 | precMax | scrollUp | scrollDn
  | heat0 | heat1 | heat2 | heat3
  -- meta
  | metaPush | metaSetKey | metaSelNull | metaSelSingle
  -- freq
  | freqOpen | freqFilter
  -- folder
  | folderPush | folderEnter | folderParent | folderDel | folderDepthDec | folderDepthInc
  -- arg-only (used in runArgCmd)
  | split | derive | export_ | sessSave | sessLoad | join
  deriving Repr, BEq, Inhabited

namespace Handler

def fromString? : String → Option Handler
  | "nav.rowInc"  => some .rowInc  | "nav.rowDec"  => some .rowDec
  | "nav.rowPgUp" => some .rowPgUp | "nav.rowPgDn" => some .rowPgDn
  | "nav.rowTop"  => some .rowTop  | "nav.rowBot"  => some .rowBot
  | "nav.rowSel"  => some .rowSel
  | "nav.colInc"  => some .colInc  | "nav.colDec"  => some .colDec
  | "nav.colFirst" => some .colFirst | "nav.colLast" => some .colLast
  | "nav.colGrp"  => some .colGrp  | "nav.colHide" => some .colHide
  | "nav.colExclude" => some .colExclude
  | "nav.colShiftL" => some .colShiftL | "nav.colShiftR" => some .colShiftR
  | "sort.asc"    => some .sortAsc | "sort.desc"   => some .sortDesc
  | "filter.colSearch"  => some .colSearch
  | "filter.rowSearch"  => some .rowSearch  | "filter.rowFilter" => some .rowFilter
  | "filter.searchNext" => some .searchNext | "filter.searchPrev" => some .searchPrev
  | "plot.area"    => some .plotArea    | "plot.line"    => some .plotLine
  | "plot.scatter" => some .plotScatter | "plot.bar"     => some .plotBar
  | "plot.box"     => some .plotBox     | "plot.step"    => some .plotStep
  | "plot.hist"    => some .plotHist    | "plot.density" => some .plotDensity
  | "plot.violin"  => some .plotViolin
  | "stk.dup"  => some .stkDup  | "stk.pop"  => some .stkPop  | "stk.swap" => some .stkSwap
  | "quit"     => some .quit     | "xpose"    => some .xpose   | "diff"     => some .diff
  | "menu"     => some .menu
  | "infoTog"  => some .infoTog
  | "precDec"  => some .precDec  | "precInc"  => some .precInc
  | "prec0"    => some .prec0    | "precMax"  => some .precMax
  | "scrollUp" => some .scrollUp | "scrollDn" => some .scrollDn
  | "heat.0"   => some .heat0   | "heat.1"   => some .heat1
  | "heat.2"   => some .heat2   | "heat.3"   => some .heat3
  | "meta.push"      => some .metaPush  | "meta.setKey"    => some .metaSetKey
  | "meta.selNull"   => some .metaSelNull | "meta.selSingle" => some .metaSelSingle
  | "freq.open"   => some .freqOpen  | "freq.filter" => some .freqFilter
  | "folder.push"     => some .folderPush     | "folder.enter"    => some .folderEnter
  | "folder.parent"   => some .folderParent   | "folder.del"      => some .folderDel
  | "folder.depthDec" => some .folderDepthDec | "folder.depthInc" => some .folderDepthInc
  | "split"    => some .split    | "derive"   => some .derive
  | "export"   => some .export_  | "sessSave" => some .sessSave
  | "sessLoad" => some .sessLoad | "join"     => some .join
  | _ => none

end Handler

-- | Effect sub-types (grouped by domain)
inductive FzfEffect where | cmd | col | row | filter deriving Repr, BEq
inductive QueryEffect where
  | colMeta | freq (colNames : Array String)
  | freqFilter (cols : Array String) (row : Nat)
  | filter (expr : String)
  | sort (colIdx : Nat) (sels : Array Nat) (grp : Array Nat) (asc : Bool)
  | exclude (cols : Array String)
  deriving Repr, BEq
inductive FolderEffect where | push | enter | del | parent | depth (delta : Int) deriving Repr, BEq
inductive SearchEffect where | next | prev deriving Repr, BEq
inductive PlotKind where | line | bar | scatter | hist | box | area | density | step | violin deriving Repr, BEq

instance : ToString PlotKind where
  toString | .line => "line" | .bar => "bar" | .scatter => "scatter" | .hist => "hist" | .box => "box"
           | .area => "area" | .density => "density" | .step => "step" | .violin => "violin"
inductive MetaEffect where | selNull | selSingle | setKey deriving Repr, BEq
inductive ExportFmt where | csv | parquet | json | ndjson deriving Repr, BEq

inductive Effect where
  | none | quit
  | fzf : FzfEffect → Effect
  | query : QueryEffect → Effect
  | folder : FolderEffect → Effect
  | search : SearchEffect → Effect
  | plot : PlotKind → Effect
  | colMeta : MetaEffect → Effect
  | fetchMore
  | export : ExportFmt → Effect
  | sessionSave
  | sessionLoad
  | join
  | transpose
  | diff
  deriving Repr, BEq

namespace Effect
def isNone : Effect → Bool | .none => true | _ => false
end Effect
