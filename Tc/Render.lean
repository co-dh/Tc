/-
  Render logic for typeclass-based navigation
  ViewState holds scroll offsets and cached widths
-/
import Tc.Nav
import Tc.Term
import Tc.Error

open Tc

-- | Pure model of the bug: C render adds widthAdj to widths, we store them,
-- next render adds widthAdj again → accumulation
def applyWidthAdj (w : Nat) (adj : Int) : Nat := Int.toNat (w + adj)

-- | BUG TEST: This theorem is FALSE, proving the bug exists.
-- If we store width after applying adj, then apply adj again, it accumulates.
-- Expected: apply(apply(10, 1), 1) = apply(10, 1) = 11  (idempotent)
-- Actual:   apply(apply(10, 1), 1) = apply(11, 1) = 12  (accumulates!)
theorem widthAdj_accumulates : applyWidthAdj (applyWidthAdj 10 1) 1 = 12 := rfl

-- | What we WANT: stored width should not include adj, so re-applying gives same result
def storedWidth (rendered : Nat) (adj : Int) : Nat := Int.toNat (rendered - adj)

-- | FIX TEST: subtract adj before storing, then re-apply → same as original
theorem widthAdj_no_accumulate_example :
    applyWidthAdj (storedWidth (applyWidthAdj 10 1) 1) 1 = applyWidthAdj 10 1 := rfl

-- ViewState: scroll offsets (widths moved to View for type safety)
structure ViewState where
  rowOff   : Nat := 0           -- first visible row
  colOff   : Nat := 0           -- first visible column (display index)
  lastCol  : Nat := 0           -- last cursor column (for tooltip direction)

-- Default ViewState
def ViewState.default : ViewState := ⟨0, 0, 0⟩

-- Max column width cap
def maxColWidth : Nat := 50

-- Reserved lines: 1 header + 1 footer + 1 tab + 1 status
def reservedLines : Nat := 4

-- Max visible rows (no terminal should exceed this)
def maxVisRows : Nat := 200

-- Theorem: clamped visible rows bounded by maxVisRows
theorem visRows_bounded (h : Nat) : min maxVisRows (h - reservedLines) ≤ maxVisRows := by
  simp only [maxVisRows]
  omega

-- Render row count: r1 - r0 where r1 = min nRows (r0 + visRows)
def renderRowCount (nRows r0 visRows : Nat) : Nat := min nRows (r0 + visRows) - r0

-- Theorem: render row count bounded by visRows
theorem renderRowCount_le_visRows (nRows r0 visRows : Nat) :
    renderRowCount nRows r0 visRows ≤ visRows := by
  simp only [renderRowCount]
  omega

-- Column page size (fixed, since widths vary)
def colPageSize : Nat := 5

-- Styles: loaded from Theme, or use default
-- 9 states: cursor, selRow, selColCurRow, selCol, curRow, curCol, default, header, group

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
    (styles : Array UInt32) (precAdj widthAdj : Int) : IO (ViewState × Array Nat) := do
  Term.clear
  let h ← Term.height
  let w ← Term.width
  -- clamp visRows to maxVisRows to prevent runaway rendering
  let visRows := min maxVisRows (h.toNat - reservedLines)
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
  let outWidths ← RenderTable.render nav inWidths colOff rowOff (min nRows (rowOff + visRows)) moveDir styles precAdj widthAdj
  -- cap widths and subtract widthAdj to prevent accumulation (see widthAdj_no_accumulate_example)
  let widths := outWidths.map fun w => min maxColWidth (storedWidth w widthAdj)
  -- status: left=colName+col+grp+sel+adj, right=row info (right-aligned)
  let colName := nav.colNames.getD nav.curColIdx ""
  let adj := (if precAdj != 0 then s!" p{precAdj}" else "") ++
             (if widthAdj != 0 then s!" w{widthAdj}" else "")
  let right := s!"c{nav.curColIdx}/{nCols} grp={nav.grp.size} sel={nav.row.sels.size}{adj} r{nav.row.cur.val}/{ReadTable.totalRows nav.tbl}"
  let w ← Term.width
  let pad := w.toNat - colName.length - right.length
  let status := colName ++ "".pushn ' ' (max 1 pad) ++ right
  Term.print 0 (h - 1) Term.cyan Term.default status
  pure (⟨rowOff, colOff, nav.curColIdx⟩, widths)

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

-- | Wait for 'q' key press
partial def waitForQ : IO Unit := do
  let ev ← Term.pollEvent
  if ev.type == Term.eventKey && ev.ch == 'q'.toNat.toUInt32 then return
  waitForQ

-- | Render error popup centered on screen, returns on 'q' press
def errorPopup (msg : String) : IO Unit := do
  let h ← Term.height; let w ← Term.width
  let help := "press q to dismiss"
  let boxW := max msg.length help.length + 4
  let x0 := (w.toNat - boxW) / 2
  let y0 := h.toNat / 2 - 1
  let pad := fun s => " " ++ s ++ "".pushn ' ' (boxW - s.length - 2) ++ " "
  Term.print x0.toUInt32 y0.toUInt32 Term.white Term.red (pad ("".pushn ' ' (boxW - 2)))
  Term.print x0.toUInt32 (y0 + 1).toUInt32 Term.white Term.red (pad msg)
  Term.print x0.toUInt32 (y0 + 2).toUInt32 Term.brBlack Term.red (pad help)
  Term.present
  waitForQ

-- | Compile-time check: errorPopup has correct signature
#check (errorPopup : String → IO Unit)
