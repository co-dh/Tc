/-
  Key mapping: Term.Event → Cmd via lookup tables
  Arrows normalized to hjkl, then unified processing
-/
import Tc.Nav
import Tc.Term

open Tc

-- Special keys: quit (q/Esc), g-prefix
inductive SpecialKey where | quit | gPrefix deriving BEq

-- Lookup in (key, value) array
private def lookup [BEq α] (tbl : Array (α × β)) (k : α) : Option β :=
  tbl.findSome? fun (k', v) => if k == k' then some v else none

-- Arrow key → hjkl char
private def arrowToChar : Array (UInt16 × Char) := #[
  (Term.keyArrowDown, 'j'), (Term.keyArrowUp, 'k'),
  (Term.keyArrowRight, 'l'), (Term.keyArrowLeft, 'h')
]

-- Navigation chars: (char, isRow, isFwd)
private def navDirs : Array (Char × Bool × Bool) := #[
  ('j', true, true), ('k', true, false), ('l', false, true), ('h', false, false)
]

-- Special key → Cmd (PageUp/Down, Home/End)
private def keyCmds : Array (UInt16 × Cmd) := #[
  (Term.keyPageDown, .row .pgNext), (Term.keyPageUp, .row .pgPrev),
  (Term.keyHome, .row .home), (Term.keyEnd, .row .end_)
]

-- Other char → Cmd (selection, group, delete, sort)
private def charCmds : Array (Char × Cmd) := #[
  ('t', .colSel .toggle), ('T', .rowSel .toggle),
  ('!', .grp .toggle), ('d', .col .del),
  ('[', .colSel .sortAsc), (']', .colSel .sortDesc)
]

-- Normalize event to char (arrow→hjkl, or raw char)
private def evToChar (ev : Term.Event) : Char :=
  (lookup arrowToChar ev.key).getD (Char.ofNat ev.ch.toNat)

-- Navigation cmd from char + shift state
private def navCmd (c : Char) (shift : Bool) : Option Cmd :=
  navDirs.findSome? fun (ch, isRow, fwd) =>
    if c.toLower == ch then
      let pg := shift || c.isUpper  -- shift or uppercase = page
      let v := if fwd then (if pg then Verb.pgNext else .next)
               else (if pg then Verb.pgPrev else .prev)
      some (if isRow then .row v else .col v)
    else none

-- Convert Term.Event to Cmd
def evToCmd (ev : Term.Event) (gPrefix : Bool) : Option Cmd :=
  if ev.type != Term.eventKey then none else
  let c := evToChar ev
  let shift := ev.mod &&& Term.modShift != 0
  if gPrefix then
    -- g-prefix: home/end
    navDirs.findSome? fun (ch, isRow, fwd) =>
      if c.toLower == ch then
        some (if isRow then .row (if fwd then .end_ else .home)
                       else .col (if fwd then .end_ else .home))
      else none
  else
    navCmd c shift <|> lookup charCmds c <|> lookup keyCmds ev.key

-- Check for special keys (quit, g-prefix)
def evToSpecial (ev : Term.Event) : Option SpecialKey :=
  if ev.type != Term.eventKey then none
  else if ev.ch == 'q'.toNat.toUInt32 || ev.key == Term.keyEsc then some .quit
  else if ev.ch == 'g'.toNat.toUInt32 then some .gPrefix
  else none
