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

-- Ctrl key → Cmd (Ctrl-D=pgdn, Ctrl-U=pgup) - termbox reports in ev.key, not ev.ch
private def ctrlCmds : Array (UInt16 × Cmd) := #[
  (Term.ctrlD.toUInt16, .vPage .inc), (Term.ctrlU.toUInt16, .vPage .dec)
]

-- Other char → Cmd (selection, group, colSel ops, stack, info)
private def charCmds : Array (Char × Cmd) := #[
  ('t', .colSel .ent), ('T', .rowSel .ent),
  ('!', .grp .ent), ('d', .colSel .del),
  ('[', .colSel .sortAsc), (']', .colSel .sortDesc),
  ('M', .info .dup), ('F', .freq .dup),   -- M=meta, F=freq view (dup=constructor)
  ('0', .info .dec), ('1', .info .inc),  -- meta: 0=selNull, 1=selSingle
  ('\r', .info .ent),  -- Enter: meta set key cols
  ('s', .col .search),     -- col search: fzf jump to column
  ('/', .row .search),     -- row search: fzf jump to row (vim-style)
  ('n', .rowSel .inc),     -- search next: repeat last search forward
  ('N', .rowSel .dec),     -- search prev: repeat last search backward
  ('\\', .row .filter),    -- row filter: fzf PRQL filter (backslash)
  ('q', .stk .dec), ('S', .stk .ent)  -- stack: q=pop, S=swap
]

-- Normalize event to char (arrow→hjkl, Enter→\r, or raw char)
private def evToChar (ev : Term.Event) : Char :=
  if ev.key == Term.keyEnter then '\r'
  else (lookup arrowToChar ev.key).getD (Char.ofNat ev.ch.toNat)

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
    navCmd c shift <|> lookup charCmds c <|> lookup keyCmds ev.key <|> lookup ctrlCmds ev.key

