/-
  Command system: Verb + Obj pattern for nav/toggle, ArgCmd for argument commands.
  - Verb: ~ < > [ ] { } - + / \ : = 0-9
  - Cmd: Obj + Verb (row, col, stk, info, metaV, freq, fld) or .arg ArgCmd
  - ArgCmd: prefix char + payload, bypasses fzf (socket/demo/test)
-/

-- | Parse typeclass (inverse of ToString)
class Parse (α : Type) where
  parse? : String → Option α

-- | Verb: action type (14 named + val 0-9)
inductive Verb where
  | inc | dec         -- > < step/forward/back
  | ent               -- ~ toggle/enter
  | del               -- - remove/subtract/prev
  | dup               -- + add/combine/next
  | search            -- / search/find
  | filter            -- \ filter/exclude
  | lbr | rbr         -- [ ] page/sort/join
  | lbc | rbc         -- { } home/end/min/max
  | split             -- : split column
  | derive            -- = derive column
  | val (n : UInt8)   -- 0-9 direct value selection
  deriving Repr, BEq, DecidableEq

namespace Verb

def toChar : Verb → Char
  | .inc => '>' | .dec => '<' | .ent => '~' | .del => '-' | .dup => '+' | .search => '/'
  | .filter => '\\' | .lbr => '[' | .rbr => ']' | .lbc => '{' | .rbc => '}'
  | .split => ':' | .derive => '='
  | .val n => Char.ofNat (n.toNat + '0'.toNat)

def ofChar? : Char → Option Verb
  | '>' => some .inc | '<' => some .dec | '~' => some .ent
  | '-' => some .del | '+' => some .dup | '/' => some .search | '\\' => some .filter
  | '[' => some .lbr | ']' => some .rbr | '{' => some .lbc | '}' => some .rbc
  | ':' => some .split | '=' => some .derive
  | c => if c.isDigit then some (.val (c.toNat - '0'.toNat).toUInt8) else none

instance : ToString Verb where toString v := v.toChar.toString
instance : Parse Verb where parse? s := s.toList.head?.bind ofChar?

end Verb

-- | Argument commands: prefix char + payload (bypass fzf, fully programmable via socket)
inductive ArgCmd where
  | split (arg : String)     -- :  split column by delimiter
  | derive (arg : String)    -- =  derive column (name = expr)
  | filter (arg : String)    -- \  filter rows by PRQL expression
  | search (arg : String)    -- /  search for value in current column
  | colJump (arg : String)   -- s  jump to column by name
  | export (arg : String)    -- e  export (csv/parquet/json/ndjson)
  | sessSave (arg : String)  -- W  save session with name
  | sessLoad (arg : String)  -- L  load session by name
  | join (arg : String)      -- J  join type by index
  deriving Repr, BEq, DecidableEq

namespace ArgCmd

def pfx : ArgCmd → Char
  | .split _ => ':' | .derive _ => '=' | .filter _ => '\\' | .search _ => '/'
  | .colJump _ => 's' | .export _ => 'e'
  | .sessSave _ => 'W' | .sessLoad _ => 'L' | .join _ => 'J'

def ofPfx? (c : Char) (arg : String) : Option ArgCmd :=
  match c with
  | ':' => some (.split arg) | '=' => some (.derive arg)
  | '\\' => some (.filter arg) | '/' => some (.search arg)
  | 's' => some (.colJump arg) | 'e' => some (.export arg)
  | 'W' => some (.sessSave arg) | 'L' => some (.sessLoad arg)
  | 'J' => some (.join arg) | _ => none

def isPfx (c : Char) : Bool := (ofPfx? c "").isSome

instance : ToString ArgCmd where toString ac := s!"{ac.pfx}{match ac with
  | .split a | .derive a | .filter a | .search a
  | .colJump a | .export a | .sessSave a | .sessLoad a | .join a => a}"

end ArgCmd

-- | Command: Obj + Verb pattern (7 objects)
inductive Cmd where
  | row (v : Verb)     -- row: ~togRow </>step [/]page {/}top/bot -prev +next /search \filter
  | col (v : Verb)     -- col: ~group </>step [/]sort {/}first/last -/+shift /search \hide :split =derive 0-8plot
  | stk (v : Verb)     -- stk: ~swap <pop >dup [/]joinL/R {quit }inner -diff +union /menu 1xpose 2diff
  | info (v : Verb)    -- info: ~toggle </>prec [/]scroll {0dp }17dp 0-3heat
  | metaV (v : Verb)   -- metaV: ~setKey +open 0selNull 1selSingle
  | freq (v : Verb)    -- freq: ~filter +open
  | fld (v : Verb)     -- fld: ~enter </>depth {parent -trash
  | arg (ac : ArgCmd)  -- argument commands (prefix + payload, bypass fzf)
  deriving Repr, BEq, DecidableEq

namespace Cmd

private def objs : Array (Char × (Verb → Cmd)) := #[
  ('r', .row), ('c', .col), ('s', .stk), ('i', .info),
  ('M', .metaV), ('F', .freq), ('D', .fld)
]

private def objChar : Cmd → Char
  | .row _ => 'r' | .col _ => 'c' | .stk _ => 's' | .info _ => 'i'
  | .metaV _ => 'M' | .freq _ => 'F' | .fld _ => 'D'
  | .arg ac => ac.pfx

private def verb : Cmd → Verb
  | .row v | .col v | .stk v | .info v | .metaV v | .freq v | .fld v => v
  | .arg _ => .ent

instance : ToString Cmd where toString
  | .arg ac => toString ac
  | c => s!"{c.objChar}{c.verb.toChar}"

instance : Parse Cmd where
  parse? s := do
    if let [o, vc] := s.toList then
      if let some v := Verb.ofChar? vc then
        if let some (_, mk) := objs.find? (·.1 == o) then
          return mk v
    if s.length > 1 then
      let pfx := s.front
      let a := (s.drop 1).toString
      if let some ac := ArgCmd.ofPfx? pfx a then return .arg ac
    none

end Cmd

-- | Effect sub-types (grouped by domain)
inductive FzfEffect where | cmd | col | row | filter deriving Repr, BEq
inductive QueryEffect where
  | colMeta | freq (colNames : Array String)
  | freqFilter (cols : Array String) (row : Nat)
  | filter (expr : String)
  | sort (colIdx : Nat) (sels : Array Nat) (grp : Array Nat) (asc : Bool)
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

class Update (α : Type) where
  update : α → Cmd → Option (α × Effect)
