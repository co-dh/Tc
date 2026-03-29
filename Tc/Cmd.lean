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

inductive PlotKind where | line | bar | scatter | hist | box | area | density | step | violin deriving Repr, BEq

instance : ToString PlotKind where
  toString | .line => "line" | .bar => "bar" | .scatter => "scatter" | .hist => "hist" | .box => "box"
           | .area => "area" | .density => "density" | .step => "step" | .violin => "violin"
inductive ExportFmt where | csv | parquet | json | ndjson deriving Repr, BEq

-- | Residual effects from pure code that can't do IO (View.update, ViewStack.update, Freq.update).
-- Most effects were eliminated by having dispatch call IO directly.
inductive Effect where
  | none | quit | fetchMore
  | sort (colIdx : Nat) (sels : Array Nat) (grp : Array Nat) (asc : Bool)
  | exclude (cols : Array String)
  | freq (colNames : Array String)
  | freqFilter (cols : Array String) (row : Nat)
  deriving Repr, BEq

namespace Effect
def isNone : Effect → Bool | .none => true | _ => false
end Effect
