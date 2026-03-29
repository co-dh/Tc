/-
  Key mapping: Term.Event → handler name via lookup tables
  Arrows normalized to hjkl, then unified processing.
  No Cmd struct — maps directly to handler name strings.
-/
import Tc.Nav
import Tc.Term
import Tc.CmdConfig

open Tc

def lookup [BEq α] (tbl : Array (α × β)) (k : α) : Option β :=
  tbl.findSome? fun (k', v) => if k == k' then some v else none

namespace KeyMap
  private def arrow : Array (UInt16 × Char) := #[
    (Term.keyArrowDown, 'j'), (Term.keyArrowUp, 'k'),
    (Term.keyArrowRight, 'l'), (Term.keyArrowLeft, 'h')
  ]

  private def nav : Array (Char × String) := #[
    ('j', "nav.rowInc"), ('k', "nav.rowDec"),
    ('l', "nav.colInc"), ('h', "nav.colDec")
  ]

  private def special : Array (UInt16 × String) := #[
    (Term.keyPageDown, "nav.rowPgDn"), (Term.keyPageUp, "nav.rowPgUp"),
    (Term.keyHome, "nav.rowTop"), (Term.keyEnd, "nav.rowBot")
  ]

  private def ctrl : Array (UInt16 × String) := #[
    (Term.ctrlD.toUInt16, "nav.rowPgDn"), (Term.ctrlU.toUInt16, "nav.rowPgUp")
  ]

  -- Compile-time copy of SQL 'key' column — must stay in sync with cfg/commands.sql.
  -- Runtime uses CmdConfig.keyLookup for the full set.
  def char : Array (Char × String) := #[
    ('{', "scrollUp"), ('}', "scrollDn"),
    ('[', "sort.asc"),  (']', "sort.desc"),
    ('!', "nav.colGrp"),  ('T', "nav.rowSel"),
    ('H', "nav.colHide"),
    ('n', "filter.searchNext"),  ('N', "filter.searchPrev"),
    (' ', "menu"),  ('q', "stk.pop"),
    ('S', "stk.swap"), ('X', "xpose"), ('d', "diff"),
    ('I', "infoTog"),
    ('M', "meta.push"), ('F', "freq.open"), ('D', "folder.push"),
    ('e', "export"), ('W', "sessSave"), ('J', "join")
  ]
end KeyMap

def evToChar (ev : Term.Event) : Char :=
  if ev.key == Term.keyEnter then '\r'
  else (lookup KeyMap.arrow ev.key).getD (Char.ofNat ev.ch.toNat)

private def navHandler (c : Char) : Option String :=
  KeyMap.nav.findSome? fun (ch, h) => if c == ch then some h else none

-- | Context-sensitive Enter key handler
def enterHandler (vk : ViewKind) : Option String :=
  match vk with
  | .freqV _ _ => some "freq.filter"
  | .colMeta  => some "meta.setKey"
  | .fld _ _  => some "folder.enter"
  | .tbl      => none

-- | Map event → handler name (for special/context-sensitive keys)
def evToHandler (ev : Term.Event) (vk : ViewKind) : Option String :=
  if ev.type != Term.eventKey then none else
  if ev.key == Term.keyEnter then enterHandler vk else
  if (ev.key == Term.keyBackspace || ev.key == Term.keyBackspace2) && vk matches .fld _ _ then some "folder.parent" else
  let c := evToChar ev
  let shift := ev.mod &&& Term.modShift != 0
  if shift && ev.key == Term.keyArrowLeft then some "nav.colShiftL"
  else if shift && ev.key == Term.keyArrowRight then some "nav.colShiftR"
  else navHandler c <|> lookup KeyMap.special ev.key <|> lookup KeyMap.ctrl ev.key

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
