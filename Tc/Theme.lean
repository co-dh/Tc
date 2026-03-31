/-
  Theme: CSV-based color themes with fzf picker and live preview via socket.
  Format: theme,variant,cursor,selRow,...  (pivoted: style names are columns, cells are "fg bg")
-/
import Tc.Types
import Tc.Term
import Tc.Fzf

namespace Tc.Theme

-- | Theme state
structure State where
  styles   : Array UInt32
  themeIdx : Nat


-- | Style names → index into styles array. Styles 0-8 used by C render, 9+ by Lean UI.
def styleNames : Array String := #[
  "cursor", "selRow", "selColCurRow", "selCol",
  "curRow", "curCol", "default", "header", "group",
  "status", "statusDim", "bar", "barDim", "error", "errorDim", "hint"
]

-- | Style index constants for Lean-side UI (C render uses 0-8 directly)
def sStatus    : Nat := 9
def sStatusDim : Nat := 10
def sBar       : Nat := 11
def sBarDim    : Nat := 12
def sError     : Nat := 13
def sErrorDim  : Nat := 14
def sHint      : Nat := 15

def styleFg (styles : Array UInt32) (idx : Nat) : UInt32 := styles.getD (idx * 2) 0
def styleBg (styles : Array UInt32) (idx : Nat) : UInt32 := styles.getD (idx * 2 + 1) 0

def parseStyle (s : String) : Option Nat := styleNames.idxOf? s

-- | Default dark theme (fallback if CSV fails)
private def c (s : String) : UInt32 := Term.parseColor s
def defaultDark : Array UInt32 := #[
  c "black", c "brWhite",       -- 0: cursor
  c "black", c "rgb354",        -- 1: selRow
  c "black", c "rgb435",        -- 2: selColCurRow
  c "brMagenta", c "default",   -- 3: selCol
  c "default", c "gray2",       -- 4: curRow
  c "default", c "gray6",       -- 5: curCol
  c "default", c "default",     -- 6: default
  c "green", c "rgb112",        -- 7: header
  c "default", c "gray5",       -- 8: group
  c "cyan", c "default",        -- 9: status
  c "brBlack", c "default",     -- 10: statusDim
  c "white", c "blue",          -- 11: bar
  c "brBlack", c "blue",        -- 12: barDim
  c "white", c "red",           -- 13: error
  c "brBlack", c "red",         -- 14: errorDim
  c "black", c "yellow"         -- 15: hint
]

-- | Global styles ref — lets errorPopup/statusMsg access theme without parameter threading
initialize stylesRef : IO.Ref (Array UInt32) ← IO.mkRef defaultDark
def getStyles : IO (Array UInt32) := stylesRef.get

-- | Detect terminal background: dark (true) or light (false)
def isDark : IO Bool := do
  match (← IO.getEnv "COLORFGBG") with
  | some s =>
    match s.splitOn ";" |>.getLast?.bind (·.toNat?) with
    | some bg => pure (bg < 7)
    | none => pure true
  | none => pure true

-- | Available themes (must match theme.csv)
def themes : Array (String × String) := #[
  ("default", "dark"), ("default", "light"),
  ("ansi", "dark"), ("ansi", "light"),
  ("nord", "dark"), ("nord", "light"),
  ("dracula", "dark"), ("dracula", "light"),
  ("gruvbox", "dark"), ("gruvbox", "light"),
  ("monokai", "dark"), ("monokai", "light")
]

def themeName (idx : Nat) : String :=
  let (t, v) := themes.getD idx ("default", "dark")
  s!"{t} ({v})"

-- | Embedded CSV as compile-time fallback
private def builtinCsv : String := include_str "../theme.csv"

-- | Find theme.csv: next to binary, then CWD, then builtin
private def loadCsv : IO String := do
  let bin ← IO.appPath
  let dir := bin.toString.splitOn "/" |>.dropLast |> "/".intercalate
  for p in #[s!"{dir}/theme.csv", "theme.csv"] do
    try let _ ← IO.FS.Handle.mk p .read; return ← IO.FS.readFile p
    catch _ => pure ()
  pure builtinCsv

-- | Load theme by name. CSV is pivoted: columns are style names, cells are "fg bg".
def load (theme variant : String) : IO (Array UInt32) := do
  let content ← loadCsv
  let lines := content.splitOn "\n" |>.filter (·.length > 0)
  let header := (lines.headD "").splitOn ","
  let colNames := header.drop 2  -- style names from header
  let row := lines.drop 1 |>.find? fun line =>
    let cols := line.splitOn ","
    cols.getD 0 "" == theme && cols.getD 1 "" == variant
  match row with
  | none => return defaultDark
  | some r =>
    let cols := r.splitOn ","
    let mut styles := defaultDark
    for i in [:colNames.length] do
      if let some idx := parseStyle (colNames.getD i "") then
        let cell := (cols.getD (i + 2) "").splitOn " "
        styles := styles.set! (idx * 2) (Term.parseColor (cell.getD 0 "default"))
        styles := styles.set! (idx * 2 + 1) (Term.parseColor (cell.getD 1 "default"))
    return styles

-- | Load theme by index
def loadIdx (idx : Nat) : IO (Array UInt32) := do
  let (theme, variant) := themes.getD idx ("default", "dark")
  load theme variant

namespace State

def init : IO State := do
  let dark ← isDark
  let variant := if dark then "dark" else "light"
  let styles ← load "default" variant
  Theme.stylesRef.set styles
  let idx := themes.findIdx? (· == ("default", variant)) |>.getD 0
  pure ⟨styles, idx⟩

def applyIdx (s : State) (idx : Nat) : IO State := do
  let styles ← loadIdx idx
  Theme.stylesRef.set styles
  pure { s with styles, themeIdx := idx }

end State

-- | fzf theme picker with live preview.
-- applyAndRender: called with loaded styles when user moves focus (for live re-render).
-- Returns (selectedIdx, styles), or none on cancel.
def run (cur : State) (applyAndRender : Array UInt32 → IO Unit) : IO (Option State) := do
  let curIdx := cur.themeIdx
  if ← Fzf.getTestMode then
    let idx := (curIdx + 1) % themes.size
    return some { styles := ← loadIdx idx, themeIdx := idx }
  let sockPath ← Socket.getPath
  let script ← Tc.tmpPath "theme-pick.sh"
  IO.FS.writeFile script s!"#!/bin/sh\necho \"theme.preview $1\" | socat - UNIX-CONNECT:{sockPath}"
  let _ ← IO.Process.output { cmd := "chmod", args := #["+x", script] }
  let items := themes.mapIdx fun i _ => s!"{i}\t{themeName i}"
  let poll : IO Unit := do
    match ← Socket.pollCmd with
    | some cmdStr =>
      let parts := cmdStr.splitOn " "
      if parts.headD "" == "theme.preview" then
        if let some idx := (parts.getD 1 "").toNat? then
          applyAndRender (← loadIdx idx)
    | none => pure ()
  let opts := #[
    "--prompt=theme: ", "--with-nth=2..", "--delimiter=\t",
    s!"--bind=focus:execute-silent({script} \{1})"]
  let out ← Fzf.fzfCore opts (items.joinWith "\n") poll
  if out.isEmpty then return none
  match out.splitOn "\t" |>.head? |>.bind String.toNat? with
  | some idx => some <$> cur.applyIdx idx
  | none => return none

end Tc.Theme
