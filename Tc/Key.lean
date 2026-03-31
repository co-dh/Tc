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
  -- Ctrl keys: ch or key field holds the control code (mod=2 for ctrl)
  | none =>
    let code := if ev.ch > 0 then ev.ch else ev.key.toUInt32
    if code == Term.ctrlD then "<C-d>"
    else if code == Term.ctrlU then "<C-u>"
    -- Regular printable chars
    else if ev.ch > 0 then (Char.ofNat ev.ch.toNat).toString
    else ""

-- | Tokenize a `-c` key string into descriptive key tokens.
-- "jj<ret><C-d>" → #["j", "j", "<ret>", "<C-d>"]
-- Aliases: <backslash> → "\", <key> → "!"
def tokenizeKeys (s : String) : Array String := Id.run do
  let chars := s.toList
  let mut acc : Array String := #[]
  let mut i := 0
  while h : i < chars.length do
    if chars[i] == '<' then
      match chars.drop (i + 1) |>.takeWhile (· != '>') with
      | [] => acc := acc.push "<"; i := i + 1
      | tag =>
        let close := i + 1 + tag.length
        if close < chars.length && chars[close]! == '>' then
          let tok := String.ofList ('<' :: tag ++ ['>'])
          -- resolve aliases: arrow keys → hjkl (matching evToKey normalization)
          let tok := match tok with
            | "<backslash>" => "\\" | "<key>" => "!"
            | "<down>" => "j" | "<up>" => "k" | "<right>" => "l" | "<left>" => "h"
            | t => t
          acc := acc.push tok
          i := close + 1
        else acc := acc.push "<"; i := i + 1
    else acc := acc.push (chars[i].toString); i := i + 1
  acc

def nextKey (keys : Array String) : IO (String × Array String) :=
  if h : keys.size > 0 then pure (keys[0], keys.extract 1 keys.size)
  else do let e ← Term.pollEvent; pure (evToKey e, #[])
