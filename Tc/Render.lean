/-
  Render logic for typeclass-based navigation
  ViewState holds scroll offsets and cached widths
-/
import Tc.Nav
import Tc.Term
import Tc.Error

open Tc

-- ViewState: scroll offsets + cached widths
structure ViewState where
  rowOff : Nat := 0           -- first visible row
  colOff : Nat := 0           -- first visible column (display index)
  widths : Array Nat := #[]   -- cached column widths (empty = need compute)

-- Default ViewState
def ViewState.default : ViewState := ⟨0, 0, #[]⟩

-- Max column width cap
def maxColWidth : Nat := 50

-- Reserved lines: 1 header + 1 footer + 1 status
def reservedLines : Nat := 3

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
class RenderTable (α : Type) [ReadTable α] where
  render : {nRows nCols : Nat} → NavState nRows nCols α
         → (inWidths : Array Nat) → (colOff rowStart rowEnd : Nat)
         → (styles : Array UInt32) → IO (Array Nat)

-- Cumulative width for scroll calculation
def cumWidth (widths : Array Nat) (i : Nat) : Nat :=
  widths.foldl (init := (0, 0)) (fun (idx, acc) w =>
    if idx < i then (idx + 1, acc + min w maxColWidth + 1) else (idx + 1, acc)) |>.2

-- Adjust column offset for visibility (simple version without Fin)
def adjColOffSimple (cur colOff : Nat) (widths : Array Nat) (screenW : Nat) : Nat :=
  let cumW := cumWidth widths
  let scrollX := cumW colOff
  let scrollMin := cumW (cur + 1) - screenW
  let scrollMax := cumW cur
  if scrollX < scrollMin then
    -- scroll right: find smallest offset where cumW >= scrollMin
    (List.range widths.size).foldl (init := colOff) fun off i =>
      if cumW i >= scrollMin && i > off then off else if cumW i >= scrollMin then i else off
  else if scrollX > scrollMax then
    -- scroll left: find largest offset where cumW <= scrollMax
    (List.range (cur + 1)).foldl (init := 0) fun off i =>
      if cumW i <= scrollMax then i else off
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
  -- subsequent: use cached widths for scroll
  let colOff := if view.widths.isEmpty then 0
                else adjColOffSimple nav.curDispCol view.colOff view.widths w.toNat
  -- render via RenderTable, get widths back
  let t0 ← IO.monoNanosNow
  let outWidths ← RenderTable.render nav view.widths colOff rowOff (min nRows (rowOff + visRows)) styles
  let t1 ← IO.monoNanosNow
  Log.timing "render" ((t1 - t0) / 1000)
  -- cap widths
  let widths := outWidths.map (min maxColWidth)
  -- status
  let status := s!"r{nav.curRow}/{nRows} c{nav.curColIdx}/{nCols} grp={nav.nKeys} sel={nav.selRows.size}"
  Term.print 0 (h - 1) Term.cyan Term.default status
  -- help
  let help := "hjkl:nav HJKL:pg g?:end t/T:sel !:grp q:q"
  Term.print (w - help.length.toUInt32) (h - 1) Term.yellow Term.default help
  Term.present
  pure ⟨rowOff, colOff, widths⟩
