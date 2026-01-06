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

-- Other char → Cmd (selection, group, colSel ops, stack, info, folder)
private def charCmds : Array (Char × Cmd) := #[
  ('t', .colSel .ent), ('T', .rowSel .ent),
  ('!', .grp .ent), ('d', .colSel .del),
  ('[', .colSel .sortAsc), (']', .colSel .sortDesc),
  ('M', .metaV .dup), ('F', .freq .dup),   -- M=meta, F=freq view (dup=constructor)
  ('D', .fld .dup),                        -- D=folder view (dup=constructor)
  ('0', .metaV .dec), ('1', .metaV .inc),  -- meta: 0=selNull, 1=selSingle
  ('\r', .view .ent),  -- Enter: view-specific action
  ('s', .col .search),     -- col search: fzf jump to column
  ('/', .row .search),     -- row search: fzf jump to row (vim-style)
  ('n', .rowSel .inc),     -- search next: repeat last search forward
  ('N', .rowSel .dec),     -- search prev: repeat last search backward
  ('\\', .row .filter),    -- row filter: fzf PRQL filter (backslash)
  ('q', .stk .dec), ('S', .stk .ent),  -- stack: q=pop, S=swap
  ('I', .info .ent)  -- info: toggle overlay
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

-- +/- prefix targets with descriptions for fzf
def prefixMenu : Array (Char × String × (Verb → Cmd)) := #[
  ('t', "theme",      .thm),
  ('p', "precision",  .prec),
  ('w', "width",      .width),
  ('h', "horiz page", .hPage),
  ('v', "vert page",  .vPage),
  ('H', "horiz end",  .hor),
  ('V', "vert end",   .ver),
  ('j', "bottom",     .ver),
  ('k', "top",        .ver),
  ('l', "end",        .hor),
  ('d', "depth",      .fld)   -- folder depth +d/-d
]

-- +/- prefix targets: h=hPage, v=vPage, H=hor, V=ver, p=prec, w=width, t=thm, hjkl=ver/hor
private def prefixObjs : Array (Char × (Verb → Cmd)) :=
  prefixMenu.map fun (c, _, mk) => (c, mk)

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

-- | Parse key notation: <ret> → \r, <C-d> → Ctrl-D, etc.
def parseKeys (s : String) : String :=
  s.replace "<ret>" "\r"
   |>.replace "<esc>" "\x1b"
   |>.replace "<C-d>" "\x04"
   |>.replace "<C-u>" "\x15"
   |>.replace "<backslash>" "\\"
   |>.replace "<key>" "!"

-- | Convert char to synthetic Term.Event (matches termbox behavior)
def charToEvent (c : Char) : Term.Event :=
  let ch := c.toNat.toUInt32
  -- Ctrl chars: termbox reports key=ctrl_code, ch=0, mod=2
  if ch < 32 then ⟨Term.eventKey, 2, ch.toUInt16, 0, 0, 0⟩
  else ⟨Term.eventKey, 0, 0, ch, 0, 0⟩

-- | Check if event is a key press for given char
def isKey (ev : Term.Event) (c : Char) : Bool :=
  ev.type == Term.eventKey && ev.ch == c.toNat.toUInt32

-- | Get next event from replay keys or poll
def nextEvent (keys : Array Char) : IO (Term.Event × Array Char) :=
  if h : keys.size > 0 then pure (charToEvent keys[0], keys.extract 1 keys.size)
  else do let e ← Term.pollEvent; pure (e, #[])

