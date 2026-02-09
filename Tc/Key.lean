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

-- | Key mapping tables grouped by category
namespace KeyMap
  -- Arrow/special keys → navigation char (hjkl)
  private def arrow : Array (UInt16 × Char) := #[
    (Term.keyArrowDown, 'j'), (Term.keyArrowUp, 'k'),
    (Term.keyArrowRight, 'l'), (Term.keyArrowLeft, 'h')
  ]

  -- Navigation chars → (isRow, isFwd) for directional dispatch
  private def nav : Array (Char × Bool × Bool) := #[
    ('j', true, true), ('k', true, false), ('l', false, true), ('h', false, false)
  ]

  -- Special keys → Cmd (PageUp/Down, Home/End)
  private def special : Array (UInt16 × Cmd) := #[
    (Term.keyPageDown, .vPage .inc), (Term.keyPageUp, .vPage .dec),  -- vPage +=down, -=up
    (Term.keyHome, .ver .dec), (Term.keyEnd, .ver .inc)  -- ver -=top, +=bottom
  ]

  -- Ctrl keys → Cmd (Ctrl-D=pgdn, Ctrl-U=pgup) - termbox reports in ev.key, not ev.ch
  private def ctrl : Array (UInt16 × Cmd) := #[
    (Term.ctrlD.toUInt16, .vPage .inc), (Term.ctrlU.toUInt16, .vPage .dec)
  ]

  -- Regular chars → Cmd (selection, group, colSel ops, stack, info, folder)
  private def char : Array (Char × Cmd) := #[
    ('t', .colSel .ent), ('T', .rowSel .ent),
    ('!', .grp .ent), ('d', .colSel .del),
    ('[', .colSel .inc), (']', .colSel .dec),
    ('M', .metaV .dup), ('F', .freq .dup),   -- M=meta, F=freq view (dup=constructor)
    ('D', .fld .dup),                        -- D=folder view (dup=constructor)
    ('0', .metaV .dec), ('1', .metaV .inc),  -- meta: 0=selNull, 1=selSingle
    ('s', .col .ent),        -- col search: fzf jump to column
    ('/', .rowSel .inc),     -- row search: fzf jump to row (vim-style)
    ('n', .grp .inc),        -- search next: repeat last search forward
    ('N', .grp .dec),        -- search prev: repeat last search backward
    ('\\', .rowSel .dec),    -- row filter: fzf PRQL filter (backslash)
    ('q', .stk .dec), ('S', .stk .ent),  -- stack: q=pop, S=swap
    ('I', .info .ent),  -- info: toggle overlay
    ('.', .plot .inc)   -- plot: line chart
  ]
end KeyMap

-- Normalize event to char (arrow→hjkl, Enter→\r, or raw char)
private def evToChar (ev : Term.Event) : Char :=
  if ev.key == Term.keyEnter then '\r'
  else (lookup KeyMap.arrow ev.key).getD (Char.ofNat ev.ch.toNat)

-- Navigation cmd from char + shift state
private def navCmd (c : Char) (shift : Bool) : Option Cmd :=
  KeyMap.nav.findSome? fun (ch, isRow, fwd) =>
    if c.toLower == ch then
      let pg := shift || c.isUpper  -- shift or uppercase = page
      let v := if fwd then Verb.inc else .dec
      some (if pg then (if isRow then .vPage v else .hPage v)
                  else (if isRow then .row v else .col v))
    else none

-- | Object menu for command mode (space key)
def objMenu : Array (Char × String × (Verb → Cmd)) := #[
  -- navigation
  ('r', "row    : cursor up/down",       .row),
  ('c', "col    : cursor left/right",    .col),
  ('v', "vPage  : vertical page",        .vPage),
  ('h', "hPage  : horizontal page",      .hPage),
  ('V', "ver    : top/bottom",           .ver),
  ('H', "hor    : first/last column",    .hor),
  -- selection
  ('R', "rowSel : filter/search/toggle", .rowSel),
  ('C', "colSel : sort/toggle/delete",   .colSel),
  ('g', "grp    : group prev/next",      .grp),
  -- options
  ('s', "stk    : view stack pop/swap",  .stk),
  ('p', "prec   : decimal precision",    .prec),
  ('w', "width  : column width",         .width),
  ('T', "thm    : theme cycle",          .thm),
  ('i', "info   : overlay toggle",       .info),
  -- views
  ('M', "metaV  : meta view",            .metaV),
  ('F', "freq   : frequency view",       .freq),
  ('D', "fld    : folder view",          .fld),
  ('P', "plot   : gnuplot chart",       .plot)
]

-- | Verb menu for command mode, context-sensitive per object and view kind
def verbsFor (obj : Char) (vk : ViewKind) : Array (Char × String × Verb) :=
  match obj with
  -- navigation
  | 'r' => #[(',', "up",    .dec), ('.', "down",  .inc)]
  | 'c' => #[(',', "left",  .dec), ('.', "right", .inc), ('~', "fzf jump", .ent)]
  | 'v' => #[(',', "page up",   .dec), ('.', "page down",  .inc)]
  | 'h' => #[(',', "page left", .dec), ('.', "page right", .inc)]
  | 'V' => #[(',', "top",    .dec), ('.', "bottom", .inc)]
  | 'H' => #[(',', "first",  .dec), ('.', "last",   .inc)]
  -- selection
  | 'R' => #[(',', "filter", .dec), ('.', "search", .inc), ('~', "toggle", .ent)]
  | 'C' => #[(',', "sort desc", .dec), ('.', "sort asc", .inc), ('~', "toggle", .ent), ('d', "delete", .del)]
  | 'g' => #[(',', "prev match", .dec), ('.', "next match", .inc), ('~', "toggle group", .ent)]
  -- options
  | 's' => #[(',', "pop", .dec), ('~', "swap", .ent), ('c', "dup", .dup)]
  | 'p' => #[(',', "less digits",  .dec), ('.', "more digits",  .inc)]
  | 'w' => #[(',', "narrower",     .dec), ('.', "wider",        .inc)]
  | 'T' => #[(',', "prev theme",   .dec), ('.', "next theme",   .inc)]
  | 'i' => #[('~', "toggle info", .ent)]
  -- views
  | 'M' => #[(',', "sel nulls", .dec), ('.', "sel singles", .inc), ('~', "enter", .ent), ('c', "push meta", .dup)]
  | 'F' => match vk with
    | .freqV _ _ => #[('~', "filter by row", .ent), ('c', "push freq", .dup)]
    | _ => #[('c', "push freq", .dup)]
  | 'D' => #[(',', "depth--", .dec), ('.', "depth++", .inc), ('~', "enter", .ent), ('d', "trash", .del), ('c', "push folder", .dup)]
  | 'P' => #[('.', "line chart", .inc), (',', "bar chart", .dec)]
  | _   => #[]

-- | Enter key → context-specific command based on view kind
def enterCmd (vk : ViewKind) : Option Cmd :=
  match vk with
  | .freqV _ _ => some (.freq .ent)  -- filter by row
  | .colMeta  => some (.metaV .ent)  -- set group key
  | .fld _ _  => some (.fld .ent)    -- enter dir/file
  | .tbl      => none                -- no action in table view

-- | Convert Term.Event to Cmd (view-aware for Enter key)
def evToCmd (ev : Term.Event) (vk : ViewKind) : Option Cmd :=
  if ev.type != Term.eventKey then none else
  if ev.key == Term.keyEnter then enterCmd vk else  -- context-sensitive Enter
  let c := evToChar ev
  let shift := ev.mod &&& Term.modShift != 0
  navCmd c shift <|> lookup KeyMap.char c <|> lookup KeyMap.special ev.key <|> lookup KeyMap.ctrl ev.key

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

