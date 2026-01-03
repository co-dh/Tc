/-
  Command system: Verb + Obj pattern
  - Verb: movement, toggle, del, sort
  - Cmd: Obj + Verb (row, col, rowSel, colSel, grp)
-/

-- | Parse typeclass (inverse of ToString)
class Parse (α : Type) where
  parse? : String → Option α

-- | Verb: action type
inductive Verb where
  | inc | dec         -- +/- movement or adjustment
  | toggle            -- toggle selection
  | del               -- delete
  | sortAsc | sortDesc  -- sort
  | dup               -- copy/dup
  deriving Repr, BEq, DecidableEq

namespace Verb

-- | Verb to char
def toChar : Verb → Char
  | .inc => '+' | .dec => '-' | .toggle => '~' | .del => 'd'
  | .sortAsc => '[' | .sortDesc => ']' | .dup => 'c'

-- | Char to verb
def ofChar? : Char → Option Verb
  | '+' => some .inc | '-' => some .dec | '~' => some .toggle | 'd' => some .del
  | '[' => some .sortAsc | ']' => some .sortDesc | 'c' => some .dup
  | _ => none

instance : ToString Verb where toString v := v.toChar.toString
instance : Parse Verb where parse? s := if s.length == 1 then ofChar? s.toList[0]! else none

-- | Isomorphism: ofChar? ∘ toChar = some
theorem ofChar_toChar (v : Verb) : ofChar? (toChar v) = some v := by
  cases v <;> rfl

end Verb

-- | Command: Obj + Verb pattern
inductive Cmd where
  | row (v : Verb)     -- row inc/dec (single step)
  | col (v : Verb)     -- col inc/dec/del (single step)
  | rowSel (v : Verb)  -- rowSel toggle
  | colSel (v : Verb)  -- colSel toggle/sortAsc/sortDesc
  | grp (v : Verb)     -- grp toggle
  | stk (v : Verb)     -- stk +push/-pop/~swap/cdup
  | hor (v : Verb)     -- hor -=home, +=end (column)
  | ver (v : Verb)     -- ver -=top, +=bottom (row)
  | hPage (v : Verb)   -- hPage -=prev, +=next page (column)
  | vPage (v : Verb)   -- vPage -=prev, +=next page (row)
  | prec (v : Verb)    -- prec -=dec, +=inc precision
  | width (v : Verb)   -- width -=dec, +=inc width
  deriving Repr, BEq, DecidableEq

namespace Cmd

-- | Obj chars: r=row, c=col, R=rowSel, C=colSel, g=grp, s=stk, h=hPage, v=vPage, H=hor, V=ver, p=prec, w=width
private def objs : Array (Char × (Verb → Cmd)) := #[
  ('r', .row), ('c', .col), ('R', .rowSel), ('C', .colSel), ('g', .grp), ('s', .stk),
  ('h', .hPage), ('v', .vPage), ('H', .hor), ('V', .ver), ('p', .prec), ('w', .width)
]

-- | Get obj char for Cmd
private def objChar : Cmd → Char
  | .row _ => 'r' | .col _ => 'c' | .rowSel _ => 'R' | .colSel _ => 'C'
  | .grp _ => 'g' | .stk _ => 's'
  | .hPage _ => 'h' | .vPage _ => 'v' | .hor _ => 'H' | .ver _ => 'V'
  | .prec _ => 'p' | .width _ => 'w'

-- | Get verb from Cmd
private def verb : Cmd → Verb
  | .row v | .col v | .rowSel v | .colSel v | .grp v | .stk v => v
  | .hor v | .ver v | .hPage v | .vPage v | .prec v | .width v => v

instance : ToString Cmd where toString c := s!"{c.objChar}{c.verb.toChar}"

instance : Parse Cmd where
  parse? s := do
    guard (s.length == 2)
    let v ← Verb.ofChar? s.toList[1]!
    let (_, mk) ← objs.find? (·.1 == s.toList[0]!)
    pure (mk v)

-- | Parse space-separated command string
def parseMany (s : String) : Array Cmd :=
  (s.splitOn " ").toArray.filterMap Parse.parse?

-- | Isomorphism: parse? ∘ toString = some
theorem parse_toString (c : Cmd) : Parse.parse? (toString c) = some c := by
  cases c with
  | row v => cases v <;> native_decide
  | col v => cases v <;> native_decide
  | rowSel v => cases v <;> native_decide
  | colSel v => cases v <;> native_decide
  | grp v => cases v <;> native_decide
  | stk v => cases v <;> native_decide
  | hor v => cases v <;> native_decide
  | ver v => cases v <;> native_decide
  | hPage v => cases v <;> native_decide
  | vPage v => cases v <;> native_decide
  | prec v => cases v <;> native_decide
  | width v => cases v <;> native_decide

end Cmd
