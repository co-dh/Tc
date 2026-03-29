/-
  Key mapping: Term.Event → Cmd via lookup tables
  Arrows normalized to hjkl, then unified processing
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

  private def nav : Array (Char × Bool × Bool) := #[
    ('j', true, true), ('k', true, false), ('l', false, true), ('h', false, false)
  ]

  private def special : Array (UInt16 × Cmd) := #[
    (Term.keyPageDown, Cmd.row ']'), (Term.keyPageUp, Cmd.row '['),
    (Term.keyHome, Cmd.row '{'), (Term.keyEnd, Cmd.row '}')
  ]

  private def ctrl : Array (UInt16 × Cmd) := #[
    (Term.ctrlD.toUInt16, Cmd.row ']'), (Term.ctrlU.toUInt16, Cmd.row '[')
  ]

  -- Compile-time copy of SQL 'key' column — for native_decide theorems only.
  -- Runtime uses CmdConfig.keyLookup. Must stay in sync with cfg/commands.sql.
  def char : Array (Char × Cmd) := #[
    ('{', Cmd.info '['), ('}', Cmd.info ']'),
    ('[', Cmd.col '['),  (']', Cmd.col ']'),
    ('!', Cmd.col '!'),  ('T', Cmd.row '~'),
    ('n', Cmd.row '+'),  ('N', Cmd.row '-'),
    (' ', Cmd.stk '/'),  ('q', Cmd.stk '<')
  ]
end KeyMap

def evToChar (ev : Term.Event) : Char :=
  if ev.key == Term.keyEnter then '\r'
  else (lookup KeyMap.arrow ev.key).getD (Char.ofNat ev.ch.toNat)

private def navCmd (c : Char) : Option Cmd :=
  KeyMap.nav.findSome? fun (ch, isRow, fwd) =>
    if c == ch then
      let v := if fwd then '>' else '<'
      some (if isRow then Cmd.row v else Cmd.col v)
    else none

-- | Context-sensitive Enter key mapping
def enterCmd (vk : ViewKind) : Option Cmd :=
  match vk with
  | .freqV _ _ => some (Cmd.freq '~')
  | .colMeta  => some (Cmd.metaV '~')
  | .fld _ _  => some (Cmd.fld '~')
  | .tbl      => none

theorem enterCmd_tbl_none : enterCmd .tbl = none := by rfl
theorem enterCmd_freq : ∀ cols total, enterCmd (.freqV cols total) = some (Cmd.freq '~') := by intros; rfl
theorem enterCmd_meta : enterCmd .colMeta = some (Cmd.metaV '~') := by rfl
theorem enterCmd_fld : ∀ p d, enterCmd (.fld p d) = some (Cmd.fld '~') := by intros; rfl

def evToCmd (ev : Term.Event) (vk : ViewKind) : Option Cmd :=
  if ev.type != Term.eventKey then none else
  if ev.key == Term.keyEnter then enterCmd vk else
  if (ev.key == Term.keyBackspace || ev.key == Term.keyBackspace2) && vk matches .fld _ _ then some (Cmd.fld '{') else
  let c := evToChar ev
  let shift := ev.mod &&& Term.modShift != 0
  if shift && ev.key == Term.keyArrowLeft then some (Cmd.col '-')
  else if shift && ev.key == Term.keyArrowRight then some (Cmd.col '+')
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
