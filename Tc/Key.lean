/-
  Key normalization: Term.Event → readable key string.
  No handler names here — all key→handler mapping lives in CmdConfig.
-/
import Tc.Nav
import Tc.Term
import Tc.CmdConfig

open Tc

-- Special key code → (bare name, modifier name)
-- Bare: unmodified output. Modifier: used with S-/C-/A- prefixes.
-- Arrows map to hjkl bare but left/right/up/down when modified.
private def keyNames : Array (UInt16 × String × String) := #[
  (Term.keyArrowDown, "j", "down"), (Term.keyArrowUp, "k", "up"),
  (Term.keyArrowLeft, "h", "left"), (Term.keyArrowRight, "l", "right"),
  (Term.keyEnter, "ret", "ret"), (Term.keyBackspace, "bs", "bs"), (Term.keyBackspace2, "bs", "bs"),
  (Term.keyPageDown, "pgdn", "pgdn"), (Term.keyPageUp, "pgup", "pgup"),
  (Term.keyHome, "home", "home"), (Term.keyEnd, "end", "end"), (Term.keyEsc, "esc", "esc")
]

private def modPfx (ev : Term.Event) : String :=
  (if ev.mod &&& Term.modShift != 0 then "S-" else "") ++
  (if ev.mod &&& Term.modCtrl  != 0 then "C-" else "") ++
  (if ev.mod &&& Term.modAlt   != 0 then "A-" else "")

-- | Normalize terminal event to readable key string.
-- Regular chars → "j", " "; special keys → "<ret>", "<pgdn>"; modifiers → "<S-left>", "<C-d>", "<A-x>"
def evToKey (ev : Term.Event) : String :=
  if ev.type != Term.eventKey then "" else
  let pfx := modPfx ev
  match keyNames.findSome? fun (k, bare, modN) => if ev.key == k then some (bare, modN) else none with
  | some (bare, modN) =>
    if pfx.isEmpty then (if bare.length == 1 then bare else s!"<{bare}>")
    else s!"<{pfx}{modN}>"
  | none =>
    let code := if ev.ch > 0 then ev.ch else ev.key.toUInt32
    if code > 0 && code < 32 then s!"<C-{(Char.ofNat (code.toNat + 96)).toString}>"
    else if ev.ch > 0 then
      let c := (Char.ofNat ev.ch.toNat).toString
      if pfx.isEmpty then c else s!"<{pfx}{c}>"
    else ""

-- | Tokenize a `-c` key string into descriptive key tokens.
-- "jj<ret><C-d>" → #["j", "j", "<ret>", "<C-d>"]
-- Aliases: arrow keys → hjkl
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
