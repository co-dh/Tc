/-
  Key mapping: Term.Event → Cmd
-/
import Tc.Nav
import Tc.Term

open Tc

-- Special keys: quit (q/Esc), g-prefix
inductive SpecialKey where | quit | gPrefix deriving BEq

-- Arrow key to Cmd (shift = page)
private def arrowCmd (ev : Term.Event) : Option Cmd :=
  let shift := ev.mod &&& Term.modShift != 0
  let v := fun fwd => if shift then (if fwd then Verb.pgNext else .pgPrev)
                      else (if fwd then Verb.next else .prev)
  if ev.key == Term.keyArrowDown       then some (.row (v true))
  else if ev.key == Term.keyArrowUp    then some (.row (v false))
  else if ev.key == Term.keyArrowRight then some (.col (v true))
  else if ev.key == Term.keyArrowLeft  then some (.col (v false))
  else none

-- hjkl/HJKL to Cmd (lower=step, upper=page)
private def hjklCmd (ev : Term.Event) : Option Cmd :=
  let c := Char.ofNat ev.ch.toNat
  if c == 'j' then some (.row .next) else if c == 'J' then some (.row .pgNext)
  else if c == 'k' then some (.row .prev) else if c == 'K' then some (.row .pgPrev)
  else if c == 'l' then some (.col .next) else if c == 'L' then some (.col .pgNext)
  else if c == 'h' then some (.col .prev) else if c == 'H' then some (.col .pgPrev)
  else none

-- Arrow/hjkl to row/col+fwd (for g-prefix)
private def dirKey (ev : Term.Event) : Option (Bool × Bool) :=
  let c := Char.ofNat ev.ch.toNat
  if ev.key == Term.keyArrowDown || c == 'j'       then some (true, true)
  else if ev.key == Term.keyArrowUp || c == 'k'    then some (true, false)
  else if ev.key == Term.keyArrowRight || c == 'l' then some (false, true)
  else if ev.key == Term.keyArrowLeft || c == 'h'  then some (false, false)
  else none

-- Convert Term.Event to Cmd (with g-prefix state)
def evToCmd (ev : Term.Event) (gPrefix : Bool) : Option Cmd :=
  if ev.type != Term.eventKey then none
  else if gPrefix then
    match dirKey ev with
    | some (isRow, fwd) => some (if isRow then .row (if fwd then .end_ else .home)
                                          else .col (if fwd then .end_ else .home))
    | none => none
  else
    arrowCmd ev <|> hjklCmd ev <|>
    if ev.key == Term.keyPageDown then some (.row .pgNext)
    else if ev.key == Term.keyPageUp then some (.row .pgPrev)
    else if ev.key == Term.keyHome then some (.row .home)
    else if ev.key == Term.keyEnd then some (.row .end_)
    else match Char.ofNat ev.ch.toNat with
      | 't' => some (.colSel .toggle)
      | 'T' => some (.rowSel .toggle)
      | '!' => some (.grp .toggle)
      | 'd' => some (.col .del)
      | _   => none

-- Check for special keys (quit, g-prefix)
def evToSpecial (ev : Term.Event) : Option SpecialKey :=
  if ev.type != Term.eventKey then none
  else if ev.ch == 'q'.toNat.toUInt32 || ev.key == Term.keyEsc then some .quit
  else if ev.ch == 'g'.toNat.toUInt32 then some .gPrefix
  else none
