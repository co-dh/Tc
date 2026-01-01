/-
  Key mapping for typeclass-based navigation
-/
import Tv.Term

-- Arrow key to axis and forward flag (shift = page)
def arrowCmd (ev : Term.Event) : Option String :=
  let shift := ev.mod &&& Term.modShift != 0
  let v := if shift then (fun fwd => if fwd then '>' else '<')
           else (fun fwd => if fwd then '+' else '-')
  if ev.key == Term.keyArrowDown  then some (String.ofList ['r', v true])
  else if ev.key == Term.keyArrowUp    then some (String.ofList ['r', v false])
  else if ev.key == Term.keyArrowRight then some (String.ofList ['c', v true])
  else if ev.key == Term.keyArrowLeft  then some (String.ofList ['c', v false])
  else none

-- hjkl/HJKL to command (lower=step, upper=page)
def hjklCmd (ev : Term.Event) : Option String :=
  let c := Char.ofNat ev.ch.toNat
  if c == 'j' then some "r+" else if c == 'J' then some "r>"
  else if c == 'k' then some "r-" else if c == 'K' then some "r<"
  else if c == 'l' then some "c+" else if c == 'L' then some "c>"
  else if c == 'h' then some "c-" else if c == 'H' then some "c<"
  else none

-- Arrow/hjkl to axis+fwd (for g-prefix)
def dirKey (ev : Term.Event) : Option (Char × Bool) :=
  let c := Char.ofNat ev.ch.toNat
  if ev.key == Term.keyArrowDown || c == 'j'  then some ('r', true)
  else if ev.key == Term.keyArrowUp || c == 'k'    then some ('r', false)
  else if ev.key == Term.keyArrowRight || c == 'l' then some ('c', true)
  else if ev.key == Term.keyArrowLeft || c == 'h'  then some ('c', false)
  else none

-- Map key to command
def keyToCmd (ev : Term.Event) (gPrefix : Bool) : Option String :=
  if ev.type != Term.eventKey then none
  else if gPrefix then
    -- g prefix: end commands
    match dirKey ev with
    | some (axis, fwd) => some (String.ofList [axis, if fwd then '$' else '0'])
    | none => none
  else
    -- Arrow keys (with shift detection)
    match arrowCmd ev with
    | some cmd => some cmd
    | none => match hjklCmd ev with
      | some cmd => some cmd
      | none =>
        -- Special keys
        if ev.key == Term.keyPageDown then some "r>"
        else if ev.key == Term.keyPageUp   then some "r<"
        else if ev.key == Term.keyHome     then some "r0"
        else if ev.key == Term.keyEnd      then some "r$"
        -- Selection toggle: t row, T col
        else if ev.ch == 't'.toNat.toUInt32 then some "R^"
        else if ev.ch == 'T'.toNat.toUInt32 then some "C^"
        -- Group toggle
        else if ev.ch == '!'.toNat.toUInt32 then some "G^"
        else none
