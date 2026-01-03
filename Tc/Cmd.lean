/-
  Command system: Verb + Obj pattern
  - Verb: movement, toggle, del, sort
  - Cmd: Obj + Verb (row, col, rowSel, colSel, grp)
-/

-- | Verb: action type
inductive Verb where
  | next | prev | pgNext | pgPrev | home | end_  -- movement
  | toggle                                        -- toggle selection
  | del                                           -- delete
  | sortAsc | sortDesc                            -- sort
  deriving Repr, BEq, DecidableEq

namespace Verb

-- | Verb to char: n/p/N/P/h/e/t/d/[/]
def toChar : Verb → Char
  | .next => 'n' | .prev => 'p' | .pgNext => 'N' | .pgPrev => 'P'
  | .home => 'h' | .end_ => 'e' | .toggle => 't' | .del => 'd'
  | .sortAsc => '[' | .sortDesc => ']'

-- | Char to verb
def ofChar? : Char → Option Verb
  | 'n' => some .next | 'p' => some .prev | 'N' => some .pgNext | 'P' => some .pgPrev
  | 'h' => some .home | 'e' => some .end_ | 't' => some .toggle | 'd' => some .del
  | '[' => some .sortAsc | ']' => some .sortDesc
  | _ => none

-- | Isomorphism: ofChar? ∘ toChar = some
theorem ofChar_toChar (v : Verb) : ofChar? (toChar v) = some v := by
  cases v <;> rfl

end Verb

-- | Command: Obj + Verb pattern
inductive Cmd where
  | row (v : Verb)     -- row next/prev/...
  | col (v : Verb)     -- col next/prev/.../del
  | rowSel (v : Verb)  -- rowSel toggle
  | colSel (v : Verb)  -- colSel toggle/sortAsc/sortDesc
  | grp (v : Verb)     -- grp toggle
  deriving Repr, BEq, DecidableEq

namespace Cmd

-- | Cmd to string: "rn" "cp" "C[" etc
def toString : Cmd → String
  | .row v    => s!"r{v.toChar}"
  | .col v    => s!"c{v.toChar}"
  | .rowSel v => s!"R{v.toChar}"
  | .colSel v => s!"C{v.toChar}"
  | .grp v    => s!"g{v.toChar}"

-- | String to cmd: "rn" → row next, "C[" → colSel sortAsc
def ofString? (s : String) : Option Cmd :=
  if s.length != 2 then none else
  let obj := s.toList[0]!
  let vrb := s.toList[1]!
  match Verb.ofChar? vrb with
  | none => none
  | some v => match obj with
    | 'r' => some (.row v)
    | 'c' => some (.col v)
    | 'R' => some (.rowSel v)
    | 'C' => some (.colSel v)
    | 'g' => some (.grp v)
    | _ => none

-- | Parse space-separated command string
def parseMany (s : String) : Array Cmd :=
  (s.splitOn " ").toArray.filterMap ofString?

-- | Isomorphism: ofString? ∘ toString = some
theorem ofString_toString (c : Cmd) : ofString? (toString c) = some c := by
  cases c with
  | row v => cases v <;> native_decide
  | col v => cases v <;> native_decide
  | rowSel v => cases v <;> native_decide
  | colSel v => cases v <;> native_decide
  | grp v => cases v <;> native_decide

end Cmd
