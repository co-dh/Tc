/-
  Key normalization: Term.Event → readable key string.
  No handler names here — all key→handler mapping lives in CmdConfig.
-/
import Tc.Nav
import Tc.Term
import Tc.CmdConfig

open Tc

namespace KeyMap
  -- Arrow keys normalized to hjkl for unified nav
  private def arrow : Array (UInt16 × Char) := #[
    (Term.keyArrowDown, 'j'), (Term.keyArrowUp, 'k'),
    (Term.keyArrowRight, 'l'), (Term.keyArrowLeft, 'h')
  ]
end KeyMap

-- | Normalize terminal event to readable key string.
-- Regular chars → "j", "!", " "; special keys → "<ret>", "<pgdn>", "<C-d>", "<S-left>" etc.
def evToKey (ev : Term.Event) : String :=
  if ev.type != Term.eventKey then "" else
  let shift := ev.mod &&& Term.modShift != 0
  -- Shift+Arrow: "<S-left>", "<S-right>"
  if shift && ev.key == Term.keyArrowLeft then "<S-left>"
  else if shift && ev.key == Term.keyArrowRight then "<S-right>"
  -- Special keys by key code
  else if ev.key == Term.keyEnter then "<ret>"
  else if ev.key == Term.keyBackspace || ev.key == Term.keyBackspace2 then "<bs>"
  else if ev.key == Term.keyPageDown then "<pgdn>"
  else if ev.key == Term.keyPageUp then "<pgup>"
  else if ev.key == Term.keyHome then "<home>"
  else if ev.key == Term.keyEnd then "<end>"
  -- Arrow keys → hjkl
  else match KeyMap.arrow.findSome? fun (k, c) => if ev.key == k then some c else none with
  | some c => c.toString
  -- Ctrl keys: ch or key field holds the control code (mod=2 for ctrl from charToEvent)
  | none =>
    let code := if ev.ch > 0 then ev.ch else ev.key.toUInt32
    if code == Term.ctrlD then "<C-d>"
    else if code == Term.ctrlU then "<C-u>"
    -- Regular printable chars
    else if ev.ch > 0 then (Char.ofNat ev.ch.toNat).toString
    else ""

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
