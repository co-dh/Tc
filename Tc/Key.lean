/-
  Key mapping: Term.Event → Cmd via lookup tables
  Arrows normalized to hjkl, then unified processing
-/
import Tc.Nav
import Tc.Term

open Tc

-- Lookup in (key, value) array
def lookup [BEq α] (tbl : Array (α × β)) (k : α) : Option β :=
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
    (Term.keyPageDown, .row (.val 8)), (Term.keyPageUp, .row (.val 1)),
    (Term.keyHome, .row (.val 0)), (Term.keyEnd, .row (.val 9))
  ]

  -- Ctrl keys → Cmd (Ctrl-D=pgdn, Ctrl-U=pgup) - termbox reports in ev.key, not ev.ch
  private def ctrl : Array (UInt16 × Cmd) := #[
    (Term.ctrlD.toUInt16, .row (.val 8)), (Term.ctrlU.toUInt16, .row (.val 1))
  ]

  -- | Single-key shortcuts — the single source of truth for all one-key mappings.
  -- Every entry is a Cmd (obj+verb). Sorted ascending by estimated usage frequency.
  def char : Array (Char × Cmd) := #[
    ('{', .prev .dec),              -- preview scroll up
    ('}', .prev .inc),              -- preview scroll down
    ('[', .colSel .inc),           -- sort ascending
    (']', .colSel .dec),           -- sort descending
    ('!', .grp .ent),              -- toggle group
    ('T', .rowSel .ent),           -- toggle row filter
    ('t', .colSel .ent),           -- toggle column sort
    ('n', .grp .inc),              -- search next
    ('N', .grp .dec),              -- search prev
    (' ', .col .dup),              -- command menu (cc)
    ('q', .stk .dec)               -- pop view / back (s<)
  ]
end KeyMap

-- Normalize event to char (arrow→hjkl, Enter→\r, or raw char)
def evToChar (ev : Term.Event) : Char :=
  if ev.key == Term.keyEnter then '\r'
  else (lookup KeyMap.arrow ev.key).getD (Char.ofNat ev.ch.toNat)

-- Navigation cmd from char + shift state
private def navCmd (c : Char) (shift : Bool) : Option Cmd :=
  KeyMap.nav.findSome? fun (ch, isRow, fwd) =>
    if c == ch then
      let v := if fwd then Verb.inc else .dec
      some (if isRow then .row v else .col v)
    else none

-- | Object menu for command mode (space key)
def objMenu : Array (Char × String × (Verb → Cmd)) := #[
  -- navigation
  ('r', "", .row),
  ('c', "", .col),
  -- selection
  ('R', "", .rowSel),
  ('C', "", .colSel),
  ('g', "", .grp),
  -- options
  ('s', "", .stk),
  ('p', "", .prec),
  ('w', "", .width),
  ('T', "", .thm),
  ('i', "", .info),
  -- views
  ('M', "", .metaV),
  ('F', "", .freq),
  ('D', "", .fld),
  ('P', "", .plot),
  ('m', "", .heat)
]

-- | Verb menu for command mode, context-sensitive per object and view kind
def verbsFor (obj : Char) (vk : ViewKind) : Array (Char × String × Verb) :=
  match obj with
  -- navigation
  | 'r' => #[('<', "up", .dec), ('>', "down", .inc), ('1', "page up", .val 1), ('8', "page down", .val 8), ('0', "top", .val 0), ('9', "bottom", .val 9), ('^', "yank row", .up), ('c', "yank cell", .dup)]
  | 'c' => #[('<', "left", .dec), ('>', "right", .inc), ('1', "page left", .val 1), ('8', "page right", .val 8), ('0', "first", .val 0), ('9', "last", .val 9), ('5', "shift left", .val 5), ('6', "shift right", .val 6), ('~', "fzf jump", .ent), ('^', "yank col", .up)]
  -- selection
  | 'R' => #[('<', "filter", .dec), ('>', "search", .inc), ('~', "toggle", .ent)]
  | 'C' => #[('<', "sort desc", .dec), ('>', "sort asc", .inc), ('~', "toggle", .ent), ('h', "hide", .dup)]
  | 'g' => #[('<', "prev match", .dec), ('>', "next match", .inc), ('~', "toggle group", .ent)]
  -- options
  | 's' => #[('<', "pop", .dec), ('~', "swap", .ent), ('c', "dup", .dup), ('d', "quit", .del), ('^', "transpose", .up), ('0', "diff", .val 0)]
  | 'p' => #[('<', "less digits",  .dec), ('>', "more digits",  .inc)]
  | 'w' => #[('<', "narrower",     .dec), ('>', "wider",        .inc)]
  | 'T' => #[('<', "prev theme",   .dec), ('>', "next theme",   .inc)]
  | 'i' => #[('~', "toggle info", .ent)]
  -- views
  | 'M' => #[('0', "sel nulls", .val 0), ('1', "sel singles", .val 1), ('~', "enter", .ent), ('c', "push meta", .dup)]
  | 'F' => match vk with
    | .freqV _ _ => #[('~', "filter by row", .ent), ('c', "push freq", .dup)]
    | _ => #[('c', "push freq", .dup)]
  | 'D' => #[('<', "depth--", .dec), ('>', "depth++", .inc), ('~', "enter", .ent), ('d', "trash", .del), ('c', "push folder", .dup)]
  | 'P' => #[('0', "line", .val 0), ('1', "bar", .val 1), ('2', "scatter", .val 2), ('3', "histogram", .val 3), ('4', "boxplot", .val 4), ('5', "area", .val 5), ('6', "density", .val 6), ('7', "step", .val 7), ('8', "violin", .val 8)]
  | 'm' => #[('0', "off", .val 0), ('1', "numeric", .val 1), ('2', "categorical", .val 2), ('3', "both", .val 3)]
  | _   => #[]

-- | Enter key → context-specific command based on view kind
def enterCmd (vk : ViewKind) : Option Cmd :=
  match vk with
  | .freqV _ _ => some (.freq .ent)  -- filter by row
  | .colMeta  => some (.metaV .ent)  -- set group key
  | .fld _ _  => some (.fld .ent)    -- enter dir/file
  | .tbl      => none                -- no action in table view

theorem enterCmd_tbl_none : enterCmd .tbl = none := by rfl

theorem enterCmd_freq : ∀ cols total, enterCmd (.freqV cols total) = some (.freq .ent) := by
  intros; rfl

theorem enterCmd_meta : enterCmd .colMeta = some (.metaV .ent) := by rfl

theorem enterCmd_fld : ∀ p d, enterCmd (.fld p d) = some (.fld .ent) := by
  intros; rfl

-- | Convert Term.Event to Cmd: special terminal keys (Enter/Backspace/Shift+Arrow) + nav/special/ctrl.
-- KeyMap.char is handled separately in mainLoop via lookup.
def evToCmd (ev : Term.Event) (vk : ViewKind) : Option Cmd :=
  if ev.type != Term.eventKey then none else
  if ev.key == Term.keyEnter then enterCmd vk else
  -- Backspace in folder view → go to parent directory
  if (ev.key == Term.keyBackspace || ev.key == Term.keyBackspace2) && vk matches .fld _ _ then some (.fld .up) else
  let c := evToChar ev
  let shift := ev.mod &&& Term.modShift != 0
  -- Shift+Arrow left/right → reorder key columns (before nav normalization)
  if shift && ev.key == Term.keyArrowLeft then some (.col (.val 5))
  else if shift && ev.key == Term.keyArrowRight then some (.col (.val 6))
  else navCmd c shift <|> lookup KeyMap.special ev.key <|> lookup KeyMap.ctrl ev.key

-- | Parse key notation: <ret> → \r, <C-d> → Ctrl-D, etc.
-- Arrow keys use \x1c-\x1f (FS/GS/RS/US — unmapped control chars)
def parseKeys (s : String) : String :=
  s.replace "<ret>" "\r"
   |>.replace "<esc>" "\x1b"
   |>.replace "<C-d>" "\x04"
   |>.replace "<C-u>" "\x15"
   |>.replace "<S-left>" "\x11"
   |>.replace "<S-right>" "\x12"
   |>.replace "<down>" "\x1c"
   |>.replace "<up>" "\x1d"
   |>.replace "<right>" "\x1e"
   |>.replace "<left>" "\x1f"
   |>.replace "<bs>" "\x7f"
   |>.replace "<backslash>" "\\"
   |>.replace "<key>" "!"
   |>.replace "<wait>" "\x16"  -- test-only: pause for socket commands

-- | Convert char to synthetic Term.Event (matches termbox behavior)
def charToEvent (c : Char) : Term.Event :=
  let ch := c.toNat.toUInt32
  -- Synthetic shift+arrow for test mode (\x11/\x12 shadow Ctrl-Q/R)
  if ch == 0x11 then ⟨Term.eventKey, Term.modShift, Term.keyArrowLeft, 0, 0, 0⟩
  else if ch == 0x12 then ⟨Term.eventKey, Term.modShift, Term.keyArrowRight, 0, 0, 0⟩
  -- Synthetic arrow keys (\x1c-\x1f)
  else if ch == 0x1c then ⟨Term.eventKey, 0, Term.keyArrowDown, 0, 0, 0⟩
  else if ch == 0x1d then ⟨Term.eventKey, 0, Term.keyArrowUp, 0, 0, 0⟩
  else if ch == 0x1e then ⟨Term.eventKey, 0, Term.keyArrowRight, 0, 0, 0⟩
  else if ch == 0x1f then ⟨Term.eventKey, 0, Term.keyArrowLeft, 0, 0, 0⟩
  -- Backspace (0x7f): termbox reports key=0x7f, ch=0
  else if ch == 0x7f then ⟨Term.eventKey, 0, Term.keyBackspace2, 0, 0, 0⟩
  -- Ctrl chars: termbox reports key=ctrl_code, ch=0, mod=2
  else if ch < 32 then ⟨Term.eventKey, 2, ch.toUInt16, 0, 0, 0⟩
  else ⟨Term.eventKey, 0, 0, ch, 0, 0⟩

-- | Check if event is a key press for given char
def isKey (ev : Term.Event) (c : Char) : Bool :=
  ev.type == Term.eventKey && ev.ch == c.toNat.toUInt32

-- | Get next event from replay keys or poll
def nextEvent (keys : Array Char) : IO (Term.Event × Array Char) :=
  if h : keys.size > 0 then pure (charToEvent keys[0], keys.extract 1 keys.size)
  else do let e ← Term.pollEvent; pure (e, #[])

