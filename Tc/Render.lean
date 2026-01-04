/-
  Render logic for typeclass-based navigation
  ViewState holds scroll offsets and cached widths
-/
import Tc.Nav
import Tc.Term
import Tc.Error

open Tc

-- ViewState: scroll offsets (widths moved to View for type safety)
structure ViewState where
  rowOff   : Nat := 0           -- first visible row
  colOff   : Nat := 0           -- first visible column (display index)
  lastCol  : Nat := 0           -- last cursor column (for tooltip direction)
  showInfo : Bool := false      -- show info overlay (toggle with I)

-- Default ViewState
def ViewState.default : ViewState := ⟨0, 0, 0, false⟩

-- Max column width cap
def maxColWidth : Nat := 50

-- Reserved lines: 1 header + 1 footer + 1 tab + 1 status
def reservedLines : Nat := 4

-- Column page size (fixed, since widths vary)
def colPageSize : Nat := 5

-- Styles: fg, bg pairs for 9 states (match C STYLE_* defines)
def styles : Array UInt32 := #[
  Term.black, Term.white,     -- cursor
  Term.black, Term.green,     -- selected row
  Term.black, Term.magenta,   -- selected col + cursor row
  Term.magenta, Term.default, -- selected col
  Term.default, Term.default, -- cursor row
  Term.yellow, Term.default,  -- cursor col
  Term.default, Term.default, -- default
  Term.white, Term.blue,      -- header
  Term.white, Term.cyan       -- group/key column
]

-- RenderTable: tables that can render themselves
-- Takes input widths (empty = compute), returns computed widths
-- moveDir: -1 = moved left, 0 = none, 1 = moved right (for tooltip direction)
-- precAdj/widthAdj: precision and width adjustments from View
class RenderTable (α : Type) [ReadTable α] where
  render : {nRows nCols : Nat} → NavState nRows nCols α
         → (inWidths : Array Nat) → (colOff rowStart rowEnd : Nat)
         → (moveDir : Int) → (styles : Array UInt32)
         → (precAdj widthAdj : Int) → IO (Array Nat)

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

-- Render table to terminal, returns (ViewState, widths)
def render {nRows nCols : Nat} {t : Type} [ReadTable t] [RenderTable t]
    (nav : NavState nRows nCols t) (view : ViewState) (inWidths : Array Nat)
    (precAdj widthAdj : Int) : IO (ViewState × Array Nat) := do
  Term.clear
  let h ← Term.height
  let w ← Term.width
  let visRows := h.toNat - reservedLines
  -- adjust row offset
  let rowOff := adjOff nav.row.cur.val view.rowOff visRows
  -- first render: use colOff=0, no scroll adjust (no widths yet)
  -- subsequent: use cached widths for scroll (widths in orig order, dispColIdxs for lookup)
  let colOff := if inWidths.isEmpty then 0
                else adjColOff nav.col.cur.val view.colOff inWidths nav.dispColIdxs w.toNat
  -- compute move direction: 1 = moved right, -1 = moved left, 0 = same
  let moveDir : Int := if nav.curColIdx > view.lastCol then 1
                       else if nav.curColIdx < view.lastCol then -1
                       else 0
  -- render via RenderTable, get widths back
  let t0 ← IO.monoNanosNow
  let outWidths ← RenderTable.render nav inWidths colOff rowOff (min nRows (rowOff + visRows)) moveDir styles precAdj widthAdj
  let t1 ← IO.monoNanosNow
  Log.timing "render" ((t1 - t0) / 1000)
  -- cap widths (keep in original order for C)
  let widths := outWidths.map (min maxColWidth)
  -- status: current column name + position info + precAdj/widthAdj if non-zero
  let total := ReadTable.totalRows nav.tbl
  let rowInfo := if total > nRows then s!"{nRows}/{total}" else s!"{nRows}"
  let colName := nav.colNames.getD nav.curColIdx ""
  let adj := (if precAdj != 0 then s!" p{precAdj}" else "") ++
             (if widthAdj != 0 then s!" w{widthAdj}" else "")
  let status := s!"{colName} r{nav.row.cur.val}/{rowInfo} c{nav.curColIdx}/{nCols} grp={nav.grp.size} sel={nav.row.sels.size}{adj}"
  Term.print 0 (h - 1) Term.cyan Term.default status
  pure (⟨rowOff, colOff, nav.curColIdx, view.showInfo⟩, widths)

-- | Render tab line: parent2 │ parent1 │ [current] (stack top on right)
def renderTabLine (tabs : Array String) (curIdx : Nat) : IO Unit := do
  let h ← Term.height
  let w ← Term.width
  let marked := tabs.mapIdx fun i t => if i == curIdx then s!"[{t}]" else t
  let line := marked.toList.reverse |> String.intercalate " │ "
  Term.print 0 (h - 2) Term.white Term.blue line
  -- pad rest of line with bg color
  if line.length < w.toNat then
    Term.print line.length.toUInt32 (h - 2) Term.white Term.blue ("".pushn ' ' (w.toNat - line.length))

-- | Key hints for info overlay (key | description)
def keyHints : Array (String × String) := #[
  ("j/k", "up/down"), ("h/l", "left/right"),
  ("g/G", "top/end"), ("^D/^U", "page"),
  ("0/$", "first/last"), ("[/]", "sort"),
  ("/\\@", "filter"), ("F", "freq"),
  ("M", "meta"), ("D", "delete"),
  ("t/T", "sel/swap"), ("!", "key col"),
  ("s", "col jump"), ("+/-", "adjust"),
  ("S", "stack"), ("I", "info"),
  ("q", "quit")
]

-- | Render info overlay at bottom-right
def infoOverlay (screenH screenW : Nat) : IO Unit := do
  let nRows := keyHints.size
  let keyW : Nat := 5; let hintW : Nat := 10
  let boxW := keyW + 1 + hintW
  let x0 := screenW - boxW - 2
  let y0 := screenH - nRows - 3
  for i in [:nRows] do
    let (k, d) := keyHints.getD i ("", "")
    let kpad := "".pushn ' ' (keyW - k.length) ++ k
    let dpad := d.take hintW ++ "".pushn ' ' (hintW - min d.length hintW)
    Term.print x0.toUInt32 (y0 + i).toUInt32 Term.black Term.yellow (kpad ++ " " ++ dpad)
