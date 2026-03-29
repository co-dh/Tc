/-
  Key mapping: Term.Event → Cmd via lookup tables
  Arrows normalized to hjkl, then unified processing
-/
import Tc.Nav
import Tc.Term

open Tc

def lookup [BEq α] (tbl : Array (α × β)) (k : α) : Option β :=
  tbl.findSome? fun (k', v) => if k == k' then some v else none

namespace KeyMap
  private def arrow : Array (UInt16 × Char) := #[
    (Term.keyArrowDown, 'j'), (Term.keyArrowUp, 'k'),
    (Term.keyArrowRight, 'l'), (Term.keyArrowLeft, 'h')
  ]

  private def nav : Array (Char × Bool × Bool) := #[
    ('j', true, true), ('k', true, false), ('l', false, true), ('h', false, false)
  ]

  private def special : Array (UInt16 × Cmd) := #[
    (Term.keyPageDown, .row .rbr), (Term.keyPageUp, .row .lbr),
    (Term.keyHome, .row .lbc), (Term.keyEnd, .row .rbc)
  ]

  private def ctrl : Array (UInt16 × Cmd) := #[
    (Term.ctrlD.toUInt16, .row .rbr), (Term.ctrlU.toUInt16, .row .lbr)
  ]

  def char : Array (Char × Cmd) := #[
    ('{', .info .lbr),              -- preview scroll up
    ('}', .info .rbr),              -- preview scroll down
    ('[', .col .lbr),               -- sort ascending
    (']', .col .rbr),               -- sort descending
    ('!', .col .bang),              -- toggle group
    ('T', .row .ent),               -- toggle row selection
    ('n', .row .dup),               -- search next match
    ('N', .row .del),               -- search prev match
    (' ', .stk .search),            -- command menu
    ('q', .stk .dec)                -- pop view / back
  ]
end KeyMap

def evToChar (ev : Term.Event) : Char :=
  if ev.key == Term.keyEnter then '\r'
  else (lookup KeyMap.arrow ev.key).getD (Char.ofNat ev.ch.toNat)

private def navCmd (c : Char) : Option Cmd :=
  KeyMap.nav.findSome? fun (ch, isRow, fwd) =>
    if c == ch then
      let v := if fwd then Verb.inc else .dec
      some (if isRow then .row v else .col v)
    else none

def objMenu : Array (Char × String × (Verb → Cmd)) := #[
  ('r', "", .row), ('c', "", .col), ('s', "", .stk), ('i', "", .info),
  ('M', "", .metaV), ('F', "", .freq), ('D', "", .fld)
]

def verbsFor (obj : Char) (vk : ViewKind) : Array (Char × String × Verb) :=
  match obj with
  | 'r' => #[('/', "Search for value in current column", .search),
             ('\\', "Filter rows by PRQL expression", .filter),
             ('~', "Select/deselect current row", .ent),
             ('+', "Jump to next search match", .dup), ('-', "Jump to previous search match", .del),
             ('[', "Page up", .lbr), (']', "Page down", .rbr),
             ('{', "Jump to top", .lbc), ('}', "Jump to bottom", .rbc),
             ('<', "Move cursor up", .dec), ('>', "Move cursor down", .inc)]
  | 'c' => #[('/', "Jump to column by name", .search),
             ('!', "Toggle group on current column", .bang),
             ('~', "Hide/unhide current column", .ent),
             ('\\', "Delete column(s) from query", .filter),
             ('[', "Sort ascending", .lbr), (']', "Sort descending", .rbr),
             (':', "Split column by delimiter", .split), ('=', "Derive new column (name = expr)", .derive),
             ('-', "Shift key column left", .del), ('+', "Shift key column right", .dup),
             ('{', "Jump to first column", .lbc), ('}', "Jump to last column", .rbc),
             ('<', "Move cursor left", .dec), ('>', "Move cursor right", .inc),
             ('0', "Plot: area chart", .val 0), ('1', "Plot: line chart", .val 1),
             ('2', "Plot: scatter plot", .val 2), ('3', "Plot: bar chart", .val 3),
             ('4', "Plot: boxplot", .val 4), ('5', "Plot: step chart", .val 5),
             ('6', "Plot: histogram", .val 6), ('7', "Plot: density plot", .val 7),
             ('8', "Plot: violin plot", .val 8)]
  | 's' => #[('/', "Open command menu", .search),
             ('~', "Swap top two views", .ent),
             ('<', "Close current view", .dec), ('>', "Duplicate current view", .inc),
             ('{', "Quit application", .lbc),
             ('1', "Transpose table (rows ↔ columns)", .val 1),
             ('2', "Diff top two views", .val 2),
             ('}', "Join: inner", .rbc), ('[', "Join: left", .lbr), (']', "Join: right", .rbr),
             ('+', "Join: union", .dup), ('-', "Join: set difference", .del)]
  | 'i' => #[('~', "Toggle info overlay", .ent),
             ('<', "Decrease decimal precision", .dec), ('>', "Increase decimal precision", .inc),
             ('{', "Set precision to 0 decimals", .lbc), ('}', "Set precision to max (17)", .rbc),
             ('[', "Scroll cell preview up", .lbr), (']', "Scroll cell preview down", .rbr),
             ('0', "Heatmap: off", .val 0), ('1', "Heatmap: numeric columns", .val 1),
             ('2', "Heatmap: categorical columns", .val 2), ('3', "Heatmap: all columns", .val 3)]
  | 'M' => #[('+', "Open column metadata view", .dup), ('~', "Set selected rows as key columns", .ent),
             ('0', "Select columns with null values", .val 0), ('1', "Select columns with single value", .val 1)]
  | 'F' => match vk with
    | .freqV _ _ => #[('+', "Open frequency view", .dup), ('~', "Filter parent table by current row", .ent)]
    | _ => #[('+', "Open frequency view", .dup)]
  | 'D' => #[('~', "Open file or enter directory", .ent), ('{', "Go to parent directory", .lbc),
             ('-', "Move to trash", .del),
             ('<', "Decrease folder depth", .dec), ('>', "Increase folder depth", .inc)]
  | _   => #[]

def enterCmd (vk : ViewKind) : Option Cmd :=
  match vk with
  | .freqV _ _ => some (.freq .ent)
  | .colMeta  => some (.metaV .ent)
  | .fld _ _  => some (.fld .ent)
  | .tbl      => none

theorem enterCmd_tbl_none : enterCmd .tbl = none := by rfl
theorem enterCmd_freq : ∀ cols total, enterCmd (.freqV cols total) = some (.freq .ent) := by intros; rfl
theorem enterCmd_meta : enterCmd .colMeta = some (.metaV .ent) := by rfl
theorem enterCmd_fld : ∀ p d, enterCmd (.fld p d) = some (.fld .ent) := by intros; rfl

def evToCmd (ev : Term.Event) (vk : ViewKind) : Option Cmd :=
  if ev.type != Term.eventKey then none else
  if ev.key == Term.keyEnter then enterCmd vk else
  if (ev.key == Term.keyBackspace || ev.key == Term.keyBackspace2) && vk matches .fld _ _ then some (.fld .lbc) else
  let c := evToChar ev
  let shift := ev.mod &&& Term.modShift != 0
  if shift && ev.key == Term.keyArrowLeft then some (.col .del)
  else if shift && ev.key == Term.keyArrowRight then some (.col .dup)
  else navCmd c <|> lookup KeyMap.special ev.key <|> lookup KeyMap.ctrl ev.key

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
   |>.replace "<wait>" "\x16"

def charToEvent (c : Char) : Term.Event :=
  let ch := c.toNat.toUInt32
  if ch == 0x11 then ⟨Term.eventKey, Term.modShift, Term.keyArrowLeft, 0, 0, 0⟩
  else if ch == 0x12 then ⟨Term.eventKey, Term.modShift, Term.keyArrowRight, 0, 0, 0⟩
  else if ch == 0x1c then ⟨Term.eventKey, 0, Term.keyArrowDown, 0, 0, 0⟩
  else if ch == 0x1d then ⟨Term.eventKey, 0, Term.keyArrowUp, 0, 0, 0⟩
  else if ch == 0x1e then ⟨Term.eventKey, 0, Term.keyArrowRight, 0, 0, 0⟩
  else if ch == 0x1f then ⟨Term.eventKey, 0, Term.keyArrowLeft, 0, 0, 0⟩
  else if ch == 0x7f then ⟨Term.eventKey, 0, Term.keyBackspace2, 0, 0, 0⟩
  else if ch < 32 then ⟨Term.eventKey, 2, ch.toUInt16, 0, 0, 0⟩
  else ⟨Term.eventKey, 0, 0, ch, 0, 0⟩

def isKey (ev : Term.Event) (c : Char) : Bool :=
  ev.type == Term.eventKey && ev.ch == c.toNat.toUInt32

def nextEvent (keys : Array Char) : IO (Term.Event × Array Char) :=
  if h : keys.size > 0 then pure (charToEvent keys[0], keys.extract 1 keys.size)
  else do let e ← Term.pollEvent; pure (e, #[])
