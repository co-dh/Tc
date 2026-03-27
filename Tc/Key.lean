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
    ('!', .col .ent),               -- toggle group
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

private def navCmd (c : Char) (shift : Bool) : Option Cmd :=
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
  | 'r' => #[('<', "up", .dec), ('>', "down", .inc), ('[', "page up", .lbr), (']', "page down", .rbr),
             ('{', "top", .lbc), ('}', "bottom", .rbc),
             ('~', "toggle row", .ent), ('+', "next match", .dup), ('-', "prev match", .del),
             ('/', "search", .search), ('\\', "filter", .filter)]
  | 'c' => #[('<', "left", .dec), ('>', "right", .inc), ('{', "first", .lbc), ('}', "last", .rbc),
             ('[', "sort asc", .lbr), (']', "sort desc", .rbr),
             ('-', "shift left", .del), ('+', "shift right", .dup),
             ('~', "toggle group", .ent), ('\\', "hide", .filter), ('/', "search col", .search),
             (':', "split", .split), ('=', "derive", .derive),
             ('0', "area", .val 0), ('1', "line", .val 1), ('2', "scatter", .val 2), ('3', "bar", .val 3),
             ('4', "boxplot", .val 4), ('5', "step", .val 5), ('6', "histogram", .val 6), ('7', "density", .val 7),
             ('8', "violin", .val 8)]
  | 's' => #[('<', "pop", .dec), ('>', "dup", .inc), ('~', "swap", .ent),
             ('[', "join left", .lbr), (']', "join right", .rbr), ('{', "quit", .lbc), ('}', "inner join", .rbc),
             ('-', "set diff", .del), ('+', "union", .dup), ('/', "cmd menu", .search),
             ('1', "transpose", .val 1), ('2', "diff", .val 2)]
  | 'i' => #[('<', "prec dec", .dec), ('>', "prec inc", .inc), ('~', "toggle info", .ent),
             ('[', "scroll up", .lbr), (']', "scroll down", .rbr), ('{', "0 dp", .lbc), ('}', "17 dp max", .rbc),
             ('0', "heat off", .val 0), ('1', "heat numeric", .val 1), ('2', "heat categorical", .val 2), ('3', "heat all", .val 3)]
  | 'M' => #[('0', "sel nulls", .val 0), ('1', "sel singles", .val 1), ('~', "set key", .ent), ('+', "open", .dup)]
  | 'F' => match vk with
    | .freqV _ _ => #[('~', "filter by row", .ent), ('+', "open", .dup)]
    | _ => #[('+', "open", .dup)]
  | 'D' => #[('<', "depth--", .dec), ('>', "depth++", .inc), ('~', "enter", .ent), ('-', "trash", .del), ('{', "parent", .lbc)]
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
  else navCmd c shift <|> lookup KeyMap.special ev.key <|> lookup KeyMap.ctrl ev.key

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
