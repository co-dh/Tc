/-
  Key mapping for typeclass-based navigation
-/
import Tc.Nav
import Tc.Term

open Tc

-- Arrow key to NavCmd (shift = page)
def arrowCmd (ev : Term.Event) : Option NavCmd :=
  let shift := ev.mod &&& Term.modShift != 0
  let v := fun fwd => if shift then (if fwd then Verb.pgNext else .pgPrev)
                      else (if fwd then Verb.next else .prev)
  if ev.key == Term.keyArrowDown       then some (.move .row (v true))
  else if ev.key == Term.keyArrowUp    then some (.move .row (v false))
  else if ev.key == Term.keyArrowRight then some (.move .col (v true))
  else if ev.key == Term.keyArrowLeft  then some (.move .col (v false))
  else none

-- hjkl/HJKL to NavCmd (lower=step, upper=page)
def hjklCmd (ev : Term.Event) : Option NavCmd :=
  let c := Char.ofNat ev.ch.toNat
  if c == 'j' then some (.move .row .next) else if c == 'J' then some (.move .row .pgNext)
  else if c == 'k' then some (.move .row .prev) else if c == 'K' then some (.move .row .pgPrev)
  else if c == 'l' then some (.move .col .next) else if c == 'L' then some (.move .col .pgNext)
  else if c == 'h' then some (.move .col .prev) else if c == 'H' then some (.move .col .pgPrev)
  else none

-- Arrow/hjkl to axis+fwd (for g-prefix)
def dirKey (ev : Term.Event) : Option (Axis × Bool) :=
  let c := Char.ofNat ev.ch.toNat
  if ev.key == Term.keyArrowDown || c == 'j'       then some (.row, true)
  else if ev.key == Term.keyArrowUp || c == 'k'    then some (.row, false)
  else if ev.key == Term.keyArrowRight || c == 'l' then some (.col, true)
  else if ev.key == Term.keyArrowLeft || c == 'h'  then some (.col, false)
  else none

-- Map key to NavCmd
def keyToCmd (ev : Term.Event) (gPrefix : Bool) : Option NavCmd :=
  if ev.type != Term.eventKey then none
  else if gPrefix then
    -- g prefix: home/end commands
    match dirKey ev with
    | some (axis, fwd) => some (.move axis (if fwd then .end_ else .home))
    | none => none
  else
    -- Arrow keys (with shift detection)
    match arrowCmd ev with
    | some cmd => some cmd
    | none => match hjklCmd ev with
      | some cmd => some cmd
      | none =>
        -- Special keys
        if ev.key == Term.keyPageDown      then some (.move .row .pgNext)
        else if ev.key == Term.keyPageUp   then some (.move .row .pgPrev)
        else if ev.key == Term.keyHome     then some (.move .row .home)
        else if ev.key == Term.keyEnd      then some (.move .row .end_)
        -- Selection toggle: t col, T row
        else if ev.ch == 't'.toNat.toUInt32 then some (.sel .col)
        else if ev.ch == 'T'.toNat.toUInt32 then some (.sel .row)
        -- Group toggle
        else if ev.ch == '!'.toNat.toUInt32 then some .grp
        -- Delete columns: d
        else if ev.ch == 'd'.toNat.toUInt32 then some .del
        else none
