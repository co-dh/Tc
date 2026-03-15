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
  lastCol  : Nat := 0           -- last cursor column (for tooltip direction)

-- Default ViewState
def ViewState.default : ViewState := ⟨0, 0⟩

-- Reserved lines: 1 header + 1 footer + 1 tab + 1 status (+ 1 sparkline when active)
def reservedLines (sparkOn : Bool := false) : Nat := if sparkOn then 5 else 4

-- Max visible rows (no terminal should exceed this)
def maxVisRows : Nat := 200

-- Column page size (fixed, since widths vary)
def colPageSize : Nat := 5

-- Default row page size (fallback when terminal height unknown)
def defaultRowPg : Nat := 20

-- Styles: loaded from Theme, or use default
-- 9 states: cursor, selRow, selColCurRow, selCol, curRow, curCol, default, header, group

-- RenderTable removed: render method now in Table class (Types.lean)

-- | Shared render helper: adjusts cursor/selections for window, calls C FFI
def renderCols (cols : Array Column) (names : Array String) (fmts : Array Char)
    (totalRows : Nat) (ctx : RenderCtx) (r0 nVisible : Nat) : IO (Array Nat) :=
  let adjCur := ctx.curRow - r0
  let adjSel := ctx.rowSels.filterMap fun r =>
    if r >= r0 && r < r0 + nVisible then some (r - r0) else none
  Term.renderTable cols names fmts ctx.inWidths ctx.dispIdxs
    totalRows.toUInt64 ctx.nGrp.toUInt64 0
    0 nVisible.toUInt64 adjCur.toUInt64 ctx.curCol.toUInt64
    ctx.moveDir.toInt64 ctx.selColIdxs adjSel ctx.hiddenIdxs
    ctx.styles ctx.precAdj.toInt64 ctx.widthAdj.toInt64
    (if ctx.heatOn then 1 else 0) ctx.sparklines

-- | Render table to terminal, returns (ViewState, widths)
-- Calls TblOps.render with NavState fields unpacked
def render {nRows nCols : Nat} {t : Type} [TblOps t]
    (nav : NavState nRows nCols t) (view : ViewState) (inWidths : Array Nat)
    (styles : Array UInt32) (precAdj widthAdj : Int) (vkind : ViewKind := .tbl)
    (heatOn : Bool := false) (sparklines : Array String := #[]) : IO (ViewState × Array Nat) := do
  Term.clear
  let h ← Term.height; let w ← Term.width
  let sparkOn := sparklines.any (!·.isEmpty)
  let visRows := min maxVisRows (h.toNat - reservedLines sparkOn)
  let rowOff := adjOff nav.row.cur.val view.rowOff visRows
  let moveDir := if nav.curColIdx > view.lastCol then 1 else if nav.curColIdx < view.lastCol then -1 else 0
  let ctx : RenderCtx := {
    inWidths, dispIdxs := nav.dispIdxs, nGrp := nav.grp.size,
    r0 := rowOff, r1 := min nRows (rowOff + visRows),
    curRow := nav.row.cur.val, curCol := nav.curColIdx, moveDir,
    selColIdxs := nav.selColIdxs, rowSels := nav.row.sels,
    hiddenIdxs := nav.hiddenIdxs, styles, precAdj, widthAdj, heatOn, sparklines }
  let outWidths ← TblOps.render nav.tbl ctx
  let widths := outWidths  -- C returns base widths (no widthAdj), store as-is
  -- status line: colName left, stats right
  -- freqV shows total distinct groups, others show table totalRows
  let total := match vkind with
    | .freqV _ t => t
    | _ => TblOps.totalRows nav.tbl
  let colName := nav.colNames.getD nav.curColIdx ""
  let adj := (if precAdj != 0 then s!" p{precAdj}" else "") ++ (if widthAdj != 0 then s!" w{widthAdj}" else "")
  let right := s!"c{nav.curColIdx}/{nCols} grp={nav.grp.size} sel={nav.row.sels.size}{adj} r{nav.row.cur.val}/{total}"
  let pad := w.toNat - colName.length - right.length
  Term.print 0 (h - 1) Term.cyan Term.default (colName ++ "".pushn ' ' (max 1 pad) ++ right)
  pure (⟨rowOff, nav.curColIdx⟩, widths)

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

-- | Show a brief status message on the bottom line (non-blocking)
-- No-op if terminal not initialized (height=0)
def statusMsg (msg : String) : IO Unit := do
  let h ← Term.height
  if h == 0 then return
  let w ← Term.width
  let padLen := if w.toNat > msg.length then w.toNat - msg.length else 0
  Term.print 0 (h - 1) Term.cyan Term.default (msg ++ "".pushn ' ' padLen)
  Term.present
