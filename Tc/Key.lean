/-
  Key mapping: returns Cmd (Obj Verb pattern)
-/
import Tc.Nav
import Tc.Term

open Tc

-- Arrow key to Cmd (shift = page)
def arrowCmd (ev : Term.Event) : Option Cmd :=
  let shift := ev.mod &&& Term.modShift != 0
  let v := fun fwd => if shift then (if fwd then Verb.pgNext else .pgPrev)
                      else (if fwd then Verb.next else .prev)
  if ev.key == Term.keyArrowDown       then some (.row (v true))
  else if ev.key == Term.keyArrowUp    then some (.row (v false))
  else if ev.key == Term.keyArrowRight then some (.col (v true))
  else if ev.key == Term.keyArrowLeft  then some (.col (v false))
  else none

-- hjkl/HJKL to Cmd (lower=step, upper=page)
def hjklCmd (ev : Term.Event) : Option Cmd :=
  let c := Char.ofNat ev.ch.toNat
  if c == 'j' then some (.row .next) else if c == 'J' then some (.row .pgNext)
  else if c == 'k' then some (.row .prev) else if c == 'K' then some (.row .pgPrev)
  else if c == 'l' then some (.col .next) else if c == 'L' then some (.col .pgNext)
  else if c == 'h' then some (.col .prev) else if c == 'H' then some (.col .pgPrev)
  else none

-- Arrow/hjkl to row/col flag (for g-prefix): true=row, false=col
def dirKey (ev : Term.Event) : Option (Bool × Bool) :=
  let c := Char.ofNat ev.ch.toNat
  if ev.key == Term.keyArrowDown || c == 'j'       then some (true, true)   -- row, fwd
  else if ev.key == Term.keyArrowUp || c == 'k'    then some (true, false)  -- row, back
  else if ev.key == Term.keyArrowRight || c == 'l' then some (false, true)  -- col, fwd
  else if ev.key == Term.keyArrowLeft || c == 'h'  then some (false, false) -- col, back
  else none

-- Map key to Cmd
def keyToCmd (ev : Term.Event) (gPrefix : Bool) : Option Cmd :=
  if ev.type != Term.eventKey then none
  else if gPrefix then
    -- g prefix: home/end commands
    match dirKey ev with
    | some (isRow, fwd) =>
      let v := if fwd then Verb.end_ else .home
      some (if isRow then .row v else .col v)
    | none => none
  else
    -- Arrow keys (with shift detection)
    match arrowCmd ev with
    | some cmd => some cmd
    | none => match hjklCmd ev with
      | some cmd => some cmd
      | none =>
        -- Special keys
        if ev.key == Term.keyPageDown      then some (.row .pgNext)
        else if ev.key == Term.keyPageUp   then some (.row .pgPrev)
        else if ev.key == Term.keyHome     then some (.row .home)
        else if ev.key == Term.keyEnd      then some (.row .end_)
        -- Selection toggle: t col, T row
        else if ev.ch == 't'.toNat.toUInt32 then some (.colSel .toggle)
        else if ev.ch == 'T'.toNat.toUInt32 then some (.rowSel .toggle)
        -- Group toggle
        else if ev.ch == '!'.toNat.toUInt32 then some (.grp .toggle)
        -- Delete columns: d
        else if ev.ch == 'd'.toNat.toUInt32 then some (.col .del)
        else none
