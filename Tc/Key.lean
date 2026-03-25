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

-- | Action triggered by a single key press
inductive KeyAction where
  | cmd (c : Cmd)  -- standard command (goes through update → runEffect)
  | quit           -- Q: exit application
  | fzfCmd         -- Space: open command menu
  | transpose      -- X: transpose push
  | diff           -- V: diff / show-same toggle
  deriving BEq

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
    (Term.keyPageDown, .vPage .inc), (Term.keyPageUp, .vPage .dec),
    (Term.keyHome, .ver .dec), (Term.keyEnd, .ver .inc)
  ]

  -- Ctrl keys → Cmd (Ctrl-D=pgdn, Ctrl-U=pgup) - termbox reports in ev.key, not ev.ch
  private def ctrl : Array (UInt16 × Cmd) := #[
    (Term.ctrlD.toUInt16, .vPage .inc), (Term.ctrlU.toUInt16, .vPage .dec)
  ]

  -- | Single-key shortcuts — the single source of truth for all one-key mappings.
  -- Sorted ascending by estimated usage frequency.
  def char : Array (Char × KeyAction) := #[
    -- rarely used
    ('{', .cmd (.prev .dec)),       -- preview scroll up
    ('}', .cmd (.prev .inc)),       -- preview scroll down
    -- occasionally used
    ('0', .cmd (.metaV (.val 0))),   -- alias for M0: select null cols
    ('1', .cmd (.metaV (.val 1))),   -- alias for M1: select single-val cols
    ('X', .transpose),              -- transpose push
    ('V', .diff),                   -- diff / show-same toggle
    ('I', .cmd (.info .ent)),       -- toggle info overlay
    ('D', .cmd (.fld .dup)),        -- open folder view
    ('M', .cmd (.metaV .dup)),      -- open meta view
    ('S', .cmd (.stk .ent)),        -- stack swap
    -- frequently used
    ('F', .cmd (.freq .dup)),       -- frequency view
    ('[', .cmd (.colSel .inc)),     -- sort ascending
    (']', .cmd (.colSel .dec)),     -- sort descending
    ('!', .cmd (.grp .ent)),        -- toggle group
    ('T', .cmd (.rowSel .ent)),     -- toggle row filter
    ('t', .cmd (.colSel .ent)),     -- toggle column sort
    ('n', .cmd (.grp .inc)),        -- search next
    ('N', .cmd (.grp .dec)),        -- search prev
    -- very frequently used
    (' ', .fzfCmd),                 -- command menu
    ('q', .cmd (.stk .dec)),        -- pop view / back
    ('Q', .quit)                    -- exit application
  ]
end KeyMap

-- Normalize event to char (arrow→hjkl, Enter→\r, or raw char)
def evToChar (ev : Term.Event) : Char :=
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
  ('C', "colSel : sort/toggle/hide",     .colSel),
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
  ('P', "plot   : ggplot2 chart (group ! for x-axis, cursor on numeric y)", .plot),
  ('K', "colShift : reorder key cols", .colShift),
  ('m', "heat   : heatmap mode 0-3", .heat),
  ('y', "yank   : copy to clipboard", .yank)
]

-- | Verb menu for command mode, context-sensitive per object and view kind
def verbsFor (obj : Char) (vk : ViewKind) : Array (Char × String × Verb) :=
  match obj with
  -- navigation
  | 'r' => #[('<', "up",    .dec), ('>', "down",  .inc)]
  | 'c' => #[('<', "left",  .dec), ('>', "right", .inc), ('~', "fzf jump", .ent)]
  | 'v' => #[('<', "page up",   .dec), ('>', "page down",  .inc)]
  | 'h' => #[('<', "page left", .dec), ('>', "page right", .inc)]
  | 'V' => #[('<', "top",    .dec), ('>', "bottom", .inc)]
  | 'H' => #[('<', "first",  .dec), ('>', "last",   .inc)]
  -- selection
  | 'R' => #[('<', "filter", .dec), ('>', "search", .inc), ('~', "toggle", .ent)]
  | 'C' => #[('<', "sort desc", .dec), ('>', "sort asc", .inc), ('~', "toggle", .ent), ('h', "hide", .dup)]
  | 'g' => #[('<', "prev match", .dec), ('>', "next match", .inc), ('~', "toggle group", .ent)]
  -- options
  | 's' => #[('<', "pop", .dec), ('~', "swap", .ent), ('c', "dup", .dup)]
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
  | 'P' => #[('>', "line — group ! sets x-axis, cursor on numeric y, group 2nd col for color", .inc), ('<', "bar — group ! sets x-axis, cursor on numeric y, group 2nd col for color", .dec), ('s', "scatter — group ! sets x-axis, cursor on numeric y, group 2nd col for color", .ent), ('h', "histogram — just put cursor on a numeric column, no grouping needed", .del), ('b', "boxplot — group ! sets x-axis, cursor on numeric y, group 2nd col for color", .dup), ('a', "area — group ! sets x-axis, cursor on numeric y, group 2nd col for color", .up)]
  | 'K' => #[('<', "shift left", .dec), ('>', "shift right", .inc)]
  | 'm' => #[('0', "off", .val 0), ('1', "numeric", .val 1), ('2', "categorical", .val 2), ('3', "both", .val 3)]
  | 'y' => #[('~', "cell", .ent), ('>', "row", .inc), ('<', "column", .dec)]
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
-- KeyMap.char is handled separately in mainLoop (returns KeyAction, not Cmd).
def evToCmd (ev : Term.Event) (vk : ViewKind) : Option Cmd :=
  if ev.type != Term.eventKey then none else
  if ev.key == Term.keyEnter then enterCmd vk else
  -- Backspace in folder view → go to parent directory
  if (ev.key == Term.keyBackspace || ev.key == Term.keyBackspace2) && vk matches .fld _ _ then some (.fld .up) else
  let c := evToChar ev
  let shift := ev.mod &&& Term.modShift != 0
  -- Shift+Arrow left/right → reorder key columns (before nav normalization)
  if shift && ev.key == Term.keyArrowLeft then some (.colShift .dec)
  else if shift && ev.key == Term.keyArrowRight then some (.colShift .inc)
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

