/-
  Preview box: shows full cell content when text is truncated.
  Word-wraps long text, supports {/} for page up/down.
-/
import Tc.Term
import Tc.Theme

namespace Tc.UI.Preview

-- | Word-wrap a string to fit within maxW characters
def wrapText (s : String) (maxW : Nat) : Array String := Id.run do
  if maxW == 0 then return #[s]
  let mut lines : Array String := #[]
  let mut cur := ""
  for c in s.toList do
    if c == '\n' then
      lines := lines.push cur
      cur := ""
    else if cur.length >= maxW then
      lines := lines.push cur
      cur := c.toString
    else
      cur := cur.push c
  if !cur.isEmpty || lines.isEmpty then lines := lines.push cur
  lines

-- | Render preview box at bottom-left
def render (screenH screenW : Nat) (text : String) (scroll : Nat) : IO Unit := do
  let maxW := min 60 (screenW - 4)
  if maxW < 4 then return
  let lines := wrapText text maxW
  let nLines := lines.size
  let maxVisible := max 1 (screenH - 6)
  let visible := min nLines maxVisible
  let maxScroll := if nLines > visible then nLines - visible else 0
  let scroll := min scroll maxScroll
  let contentW := lines.foldl (fun mx l => max mx l.length) 0
  let innerW := min contentW maxW
  let innerW := max innerW 4  -- minimum inner width
  -- bottom-left: x0=0, box ends at screenH - 3 (above tab + status lines)
  let x0 : Nat := 0
  let y1 := screenH - 3                  -- last row of box (bottom border)
  let y0 := if y1 + 1 > visible + 2 then y1 - visible - 1 else 0  -- top border
  let actualVisible := y1 - y0 - 1       -- rows between borders
  let s ← Theme.getStyles
  let fg := Theme.styleFg s Theme.sBar; let bg := Theme.styleBg s Theme.sBar
  let dfg := Theme.styleFg s Theme.sBarDim; let dbg := Theme.styleBg s Theme.sBarDim
  -- top border
  Term.print x0.toUInt32 y0.toUInt32 fg bg
    ("┌" ++ "".pushn '─' innerW ++ "┐")
  -- content lines with side borders
  for i in [:actualVisible] do
    let line := lines.getD (scroll + i) ""
    let trimmed := if line.length > innerW then (line.take innerW).toString else line
    let padded := trimmed ++ "".pushn ' ' (innerW - trimmed.length)
    Term.print x0.toUInt32 (y0 + 1 + i).toUInt32 fg bg
      ("│" ++ padded ++ "│")
  -- bottom border with scroll indicator
  let indicator := if nLines > actualVisible
    then s!" {scroll + 1}-{scroll + actualVisible}/{nLines} " ++ "{/}"
    else ""
  let dashW := if innerW > indicator.length then innerW - indicator.length else 0
  let border := "└" ++ "".pushn '─' dashW ++ indicator ++ "┘"
  Term.print x0.toUInt32 y1.toUInt32 dfg dbg border

end Tc.UI.Preview
