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
  | inc | dec         -- +/- movement (colSel:[/], rowSel:/\, grp:n/N)
  | dup               -- copy/dup (info: select single-val cols)
  | del               -- delete
  | ent               -- toggle/enter (col:s, rowSel:T, grp:!)
  deriving Repr, BEq, DecidableEq

namespace Verb

-- | Verb to char
def toChar : Verb → Char
  | .inc => '+' | .dec => '-' | .ent => '~' | .del => 'd' | .dup => 'c'

-- | Char to verb
def ofChar? : Char → Option Verb
  | '+' => some .inc | '-' => some .dec | '~' => some .ent
  | 'd' => some .del | 'c' => some .dup | _ => none

instance : ToString Verb where toString v := v.toChar.toString
instance : Parse Verb where parse? s := s.toList.head?.bind ofChar?

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

  | rowSel (v : Verb)  -- rowSel +=search(/), -=filter(\), ~=toggle(T)
  | colSel (v : Verb)  -- colSel +=sortAsc([), -=sortDesc(]), ~=toggle(t)
  | grp (v : Verb)     -- grp +=next(n), -=prev(N), ~=toggle(!)

  | stk (v : Verb)     -- stk +push/-pop/~swap/cdup

  | prec (v : Verb)    -- prec -=dec, +=inc precision
  | width (v : Verb)   -- width -=dec, +=inc width
  | thm (v : Verb)     -- thm -=prev, +=next theme
  | info (v : Verb)    -- info +=show, -=hide, ~=toggle
  | metaV (v : Verb)   -- metaV c=push, -/0=selNull, +/1=selSingle, ~=enter
  | freq (v : Verb)    -- freq c=push, ~=filter
  | fld (v : Verb)     -- fld c=push, +/-=depth, ~=enter dir/file
  | plot (v : Verb)    -- plot +=line, -=bar
  deriving Repr, BEq, DecidableEq

namespace Cmd

-- | Obj chars
private def objs : Array (Char × (Verb → Cmd)) := #[
  ('r', .row), ('c', .col), ('R', .rowSel), ('C', .colSel), ('g', .grp), ('s', .stk),
  ('h', .hPage), ('v', .vPage), ('H', .hor), ('V', .ver), ('p', .prec), ('w', .width),
  ('T', .thm), ('i', .info), ('M', .metaV), ('F', .freq), ('D', .fld),
  ('P', .plot)
]

-- | Get obj char for Cmd
private def objChar : Cmd → Char
  | .row _ => 'r' | .col _ => 'c' | .rowSel _ => 'R' | .colSel _ => 'C'
  | .grp _ => 'g' | .stk _ => 's'
  | .hPage _ => 'h' | .vPage _ => 'v' | .hor _ => 'H' | .ver _ => 'V'
  | .prec _ => 'p' | .width _ => 'w' | .thm _ => 'T' | .info _ => 'i'
  | .metaV _ => 'M' | .freq _ => 'F' | .fld _ => 'D' | .plot _ => 'P'

-- | Get verb from Cmd
private def verb : Cmd → Verb
  | .row v | .col v | .rowSel v | .colSel v | .grp v | .stk v => v
  | .hor v | .ver v | .hPage v | .vPage v | .prec v | .width v => v
  | .thm v | .info v | .metaV v | .freq v | .fld v | .plot v => v

instance : ToString Cmd where toString c := s!"{c.objChar}{c.verb.toChar}"

instance : Parse Cmd where
  parse? s := do
    let [o, vc] := s.toList | none
    let v ← Verb.ofChar? vc
    let (_, mk) ← objs.find? (·.1 == o)
    pure (mk v)

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
  | thm v => cases v <;> native_decide
  | info v => cases v <;> native_decide
  | metaV v => cases v <;> native_decide
  | freq v => cases v <;> native_decide
  | fld v => cases v <;> native_decide
  | plot v => cases v <;> native_decide

end Cmd

-- | Exec typeclass: handle Cmd, return updated state or none (IO version)
class Exec (α : Type) where
  exec : α → Cmd → IO (Option α)

-- | Effect: describes an IO operation to perform (Runner interprets)
inductive Effect where
  | none | quit
  | fzfCmd | fzfCol
  | fzfRow (colIdx : Nat) (colName : String)
  | fzfFilter (colIdx : Nat) (colName : String)
  | queryMeta
  | queryFreq (colNames : Array String)
  | freqFilter (cols : Array String) (row : Nat)
  | queryFilter (expr : String)
  | querySort (colIdx : Nat) (sels : Array Nat) (grp : Array Nat) (asc : Bool)
  | queryDel (colIdx : Nat) (sels : Array Nat) (grp : Array String)
  | folderPush | folderEnter | folderDel
  | folderDepth (delta : Int)
  | findNext | findPrev
  | themeLoad (delta : Int)
  | plotLine | plotBar
  | fetchMore
  deriving Repr, BEq

namespace Effect
def isNone : Effect → Bool | .none => true | _ => false
end Effect

-- | Update typeclass: pure state transition returning Effect
class Update (α : Type) where
  update : α → Cmd → Option (α × Effect)
