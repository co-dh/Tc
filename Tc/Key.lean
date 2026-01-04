/-
  Key mapping: Term.Event → Cmd via lookup tables
  Arrows normalized to hjkl, then unified processing
-/
import Tc.Nav
import Tc.Term

open Tc

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
  (Term.keyPageDown, .vPage .inc), (Term.keyPageUp, .vPage .dec),  -- vPage +=down, -=up
  (Term.keyHome, .ver .dec), (Term.keyEnd, .ver .inc)  -- ver -=top, +=bottom
]

-- Ctrl char → Cmd (Ctrl-D=pgdn, Ctrl-U=pgup)
private def ctrlCmds : Array (UInt32 × Cmd) := #[
  (Term.ctrlD, .vPage .inc), (Term.ctrlU, .vPage .dec)
]

-- Other char → Cmd (selection, group, colSel ops, stack)
private def charCmds : Array (Char × Cmd) := #[
  ('t', .colSel .toggle), ('T', .rowSel .toggle),
  ('!', .grp .toggle), ('d', .colSel .del),
  ('[', .colSel .sortAsc), (']', .colSel .sortDesc),
  ('M', .colSel .colMeta), ('F', .colSel .freq),
  ('s', .col .search),     -- col search: fzf jump to column
  ('@', .row .search),     -- row search: fzf jump to row#
  ('/', .row .filter),     -- row filter: fzf PRQL filter
  ('\\', .col .filter),    -- col filter: fzf select columns
  ('q', .stk .dec), ('S', .stk .toggle)  -- stack: q=pop, S=swap
]

-- Normalize event to char (arrow→hjkl, or raw char)
private def evToChar (ev : Term.Event) : Char :=
  (lookup arrowToChar ev.key).getD (Char.ofNat ev.ch.toNat)

-- Navigation cmd from char + shift state
private def navCmd (c : Char) (shift : Bool) : Option Cmd :=
  navDirs.findSome? fun (ch, isRow, fwd) =>
    if c.toLower == ch then
      let pg := shift || c.isUpper  -- shift or uppercase = page
      let v := if fwd then Verb.inc else .dec
      some (if pg then (if isRow then .vPage v else .hPage v)
                  else (if isRow then .row v else .col v))
    else none

-- +/- prefix targets: h=hPage, v=vPage, H=hor, V=ver, p=prec, w=width, hjkl=ver/hor
private def prefixObjs : Array (Char × (Verb → Cmd)) := #[
  ('h', .hPage), ('v', .vPage), ('H', .hor), ('V', .ver), ('p', .prec), ('w', .width),
  ('j', .ver), ('k', .ver), ('l', .hor)  -- hjkl maps to ver/hor (end navigation)
]

-- Convert Term.Event to Cmd
-- verbPfx: none = normal, some .inc = after +, some .dec = after -
def evToCmd (ev : Term.Event) (verbPfx : Option Verb) : Option Cmd :=
  if ev.type != Term.eventKey then none else
  let c := evToChar ev
  let shift := ev.mod &&& Term.modShift != 0
  match verbPfx with
  | some v =>
    -- +/- prefix: map to hor/ver/prec/width objects
    prefixObjs.findSome? fun (ch, mk) => if c.toLower == ch then some (mk v) else none
  | none =>
    navCmd c shift <|> lookup charCmds c <|> lookup keyCmds ev.key <|> lookup ctrlCmds ev.ch

