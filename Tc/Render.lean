/-
  Render logic for typeclass-based navigation
  ViewState holds scroll offsets and cached widths
-/
import Tc.Nav
import Tc.Term
import Tc.Error

open Tc

-- ViewState: scroll offsets + cached widths + last cursor for direction
structure ViewState where
  rowOff  : Nat := 0           -- first visible row
  colOff  : Nat := 0           -- first visible column (display index)
  widths  : Array Nat := #[]   -- cached column widths (empty = need compute)
  lastCol : Nat := 0           -- last cursor column (for tooltip direction)

-- Default ViewState
def ViewState.default : ViewState := ⟨0, 0, #[], 0⟩

-- Max column width cap
def maxColWidth : Nat := 50

-- Reserved lines: 1 header + 1 footer + 1 tab + 1 status
def reservedLines : Nat := 4

-- Column page size (fixed, since widths vary)
def colPageSize : Nat := 5

-- Styles: fg, bg pairs for 8 states (match C STYLE_* defines)
def styles : Array UInt32 := #[
  Term.black, Term.white,     -- cursor
  Term.black, Term.green,     -- selected row
  Term.black, Term.magenta,   -- selected col + cursor row
  Term.magenta, Term.default, -- selected col
  Term.default, Term.default, -- cursor row
  Term.yellow, Term.default,  -- cursor col
  Term.default, Term.default, -- default
  Term.white, Term.blue       -- header
]

-- RenderTable: tables that can render themselves
-- Takes input widths (empty = compute), returns computed widths
-- moveDir: -1 = moved left, 0 = none, 1 = moved right (for tooltip direction)
class RenderTable (α : Type) [ReadTable α] where
  render : {nRows nCols : Nat} → NavState nRows nCols α
         → (inWidths : Array Nat) → (colOff rowStart rowEnd : Nat)
         → (moveDir : Int) → (styles : Array UInt32) → IO (Array Nat)

-- Cumulative width in display order (dispIdxs maps display->original)
-- Returns x position where column i starts (sum of widths 0..i-1)
def cumWidthDisp (widths : Array Nat) (dispIdxs : Array Nat) (i : Nat) : Nat :=
  (Array.range i).foldl (init := 0) fun acc d =>
    acc + min (widths.getD (dispIdxs.getD d 0) 0) maxColWidth + 1

-- Adjust column offset so cursor is visible
-- cur = cursor display index, colOff = first visible display index
def adjColOff (cur colOff : Nat) (widths : Array Nat) (dispIdxs : Array Nat) (screenW : Nat) : Nat :=
  let cumW := cumWidthDisp widths dispIdxs
  let curEnd := cumW (cur + 1)  -- right edge of cursor column
  let offX := cumW colOff       -- left edge of visible area
  -- scroll right: cursor's right edge past screen
  if curEnd > offX + screenW then
    -- find smallest offset where cursor fits: cumW(off) + screenW >= curEnd
    (List.range (cur + 1)).find? (fun i => cumW i + screenW >= curEnd) |>.getD cur
  -- scroll left: cursor's left edge before visible area
  else if cumW cur < offX then cur
  else colOff

-- Render table to terminal, returns updated ViewState
def render {nRows nCols : Nat} {t : Type} [ReadTable t] [RenderTable t]
    (nav : NavState nRows nCols t) (view : ViewState) : IO ViewState := do
  Term.clear
  let h ← Term.height
  let w ← Term.width
  let visRows := h.toNat - reservedLines
  -- adjust row offset
  let rowOff := adjOff nav.curRow view.rowOff visRows
  -- first render: use colOff=0, no scroll adjust (no widths yet)
  -- subsequent: use cached widths for scroll (widths in orig order, dispColIdxs for lookup)
  let colOff := if view.widths.isEmpty then 0
                else adjColOff nav.curDispCol view.colOff view.widths nav.dispColIdxs w.toNat
  -- compute move direction: 1 = moved right, -1 = moved left, 0 = same
  let moveDir : Int := if nav.curColIdx > view.lastCol then 1
                       else if nav.curColIdx < view.lastCol then -1
                       else 0
  -- render via RenderTable, get widths back
  let t0 ← IO.monoNanosNow
  let outWidths ← RenderTable.render nav view.widths colOff rowOff (min nRows (rowOff + visRows)) moveDir styles
  let t1 ← IO.monoNanosNow
  Log.timing "render" ((t1 - t0) / 1000)
  -- cap widths (keep in original order for C)
  let widths := outWidths.map (min maxColWidth)
  -- status: current column name + position info
  let total := ReadTable.totalRows nav.tbl
  let rowInfo := if total > nRows then s!"{nRows}/{total}" else s!"{nRows}"
  let colName := nav.colNames.getD nav.curColIdx ""
  let status := s!"{colName} r{nav.curRow}/{rowInfo} c{nav.curColIdx}/{nCols} grp={nav.nKeys} sel={nav.selRows.size}"
  Term.print 0 (h - 1) Term.cyan Term.default status
  -- help
  let help := "hjkl:nav HJKL:pg g?:end t/T:sel !:grp q:q"
  Term.print (w - help.length.toUInt32) (h - 1) Term.yellow Term.default help
  pure ⟨rowOff, colOff, widths, nav.curColIdx⟩

-- | Render tab line: [current] | parent1 | parent2 ...
def renderTabLine (tabs : Array String) (curIdx : Nat) : IO Unit := do
  let h ← Term.height
  let w ← Term.width
  let marked := tabs.mapIdx fun i t => if i == curIdx then s!"[{t}]" else t
  let line := marked.toList |> String.intercalate " │ "
  Term.print 0 (h - 2) Term.white Term.blue line
  -- pad rest of line with bg color
  if line.length < w.toNat then
    Term.print line.length.toUInt32 (h - 2) Term.white Term.blue ("".pushn ' ' (w.toNat - line.length))
