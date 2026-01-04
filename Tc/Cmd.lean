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
  | dup               -- copy/dup (info: select single-val cols)
  | del               -- delete
  | ent            -- toggle selection
  | sortAsc | sortDesc  -- sort
  | search            -- search/jump (col=fzf name, row=fzf row#)
  | filter            -- filter (col=select cols, row=PRQL filter)
  deriving Repr, BEq, DecidableEq

namespace Verb

-- | Verb to char
def toChar : Verb → Char
  | .inc => '+' | .dec => '-' | .ent => '~' | .del => 'd'
  | .sortAsc => '[' | .sortDesc => ']' | .dup => 'c'
  | .search => 's' | .filter => 'f'

-- | Char to verb
def ofChar? : Char → Option Verb
  | '+' => some .inc | '-' => some .dec | '~' => some .ent | 'd' => some .del
  | '[' => some .sortAsc | ']' => some .sortDesc | 'c' => some .dup
  | 's' => some .search | 'f' => some .filter | _ => none

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
  | hPage (v : Verb)   -- hPage -=prev, +=next page (column)
  | vPage (v : Verb)   -- vPage -=prev, +=next page (row)
  | hor (v : Verb)     -- hor -=home, +=end (column)
  | ver (v : Verb)     -- ver -=top, +=bottom (row)

  | rowSel (v : Verb)  -- rowSel toggle
  | colSel (v : Verb)  -- colSel toggle/sortAsc/sortDesc
  | grp (v : Verb)     -- grp toggle

  | stk (v : Verb)     -- stk +push/-pop/~swap/cdup

  | prec (v : Verb)    -- prec -=dec, +=inc precision
  | width (v : Verb)   -- width -=dec, +=inc width
  | info (v : Verb) -- info c=push, -/0=selNull, +/1=selSingle, ~=setKeyCols
  | freq (v : Verb) -- freq ~=filter by selected value
  deriving Repr, BEq, DecidableEq

namespace Cmd

-- | Obj chars: r=row, c=col, R=rowSel, C=colSel, g=grp, s=stk, h=hPage, v=vPage, H=hor, V=ver, p=prec, w=width, M=info, f=freq
private def objs : Array (Char × (Verb → Cmd)) := #[
  ('r', .row), ('c', .col), ('R', .rowSel), ('C', .colSel), ('g', .grp), ('s', .stk),
  ('h', .hPage), ('v', .vPage), ('H', .hor), ('V', .ver), ('p', .prec), ('w', .width),
  ('M', .info), ('f', .freq)
]

-- | Get obj char for Cmd
private def objChar : Cmd → Char
  | .row _ => 'r' | .col _ => 'c' | .rowSel _ => 'R' | .colSel _ => 'C'
  | .grp _ => 'g' | .stk _ => 's'
  | .hPage _ => 'h' | .vPage _ => 'v' | .hor _ => 'H' | .ver _ => 'V'
  | .prec _ => 'p' | .width _ => 'w' | .info _ => 'M' | .freq _ => 'f'

-- | Get verb from Cmd
private def verb : Cmd → Verb
  | .row v | .col v | .rowSel v | .colSel v | .grp v | .stk v => v
  | .hor v | .ver v | .hPage v | .vPage v | .prec v | .width v | .info v | .freq v => v

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
  | info v => cases v <;> native_decide
  | freq v => cases v <;> native_decide

end Cmd
