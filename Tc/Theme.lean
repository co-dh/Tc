/-
  Theme: CSV-based color themes with fzf picker and live preview via socket.
  Format: theme,variant,name,fg,bg
-/
import Tc.Types
import Tc.Term
import Tc.Fzf

namespace Tc.Theme

-- | Theme state
structure State where
  styles   : Array UInt32
  themeIdx : Nat


-- | Style names (index into the 18-element styles array)
def styleNames : Array String := #[
  "cursor", "selRow", "selColCurRow", "selCol",
  "curRow", "curCol", "default", "header", "group"
]

def parseStyle (s : String) : Option Nat := styleNames.idxOf? s

-- | Default dark theme (fallback if CSV fails)
def defaultDark : Array UInt32 := #[
  Term.black, Term.brWhite,  -- cursor
  Term.black, 158,           -- selRow     (rgb354 = mint)
  Term.black, 183,           -- selColCurRow (rgb435 = lavender)
  Term.brMagenta, Term.default, -- selCol
  Term.default, 234,         -- curRow     (gray2)
  Term.default, 238,         -- curCol     (gray6)
  Term.default, Term.default, -- default
  Term.green, 60,            -- header     (rgb112 = slate)
  Term.default, 237          -- group      (gray5)
]

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

-- | Load theme by name
def load (theme variant : String) : IO (Array UInt32) := do
  let content ← loadCsv
  let matching := content.splitOn "\n"
    |>.filter (·.length > 0)
    |>.drop 1
    |>.map (·.splitOn ",")
    |>.filter fun r => r.getD 0 "" == theme && r.getD 1 "" == variant
  let mut styles := defaultDark
  for row in matching do
    if let some idx := parseStyle (row.getD 2 "") then
      styles := styles.set! (idx * 2) (Term.parseColor (row.getD 3 "default"))
      styles := styles.set! (idx * 2 + 1) (Term.parseColor (row.getD 4 "default"))
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
  let idx := themes.findIdx? (· == ("default", variant)) |>.getD 0
  pure ⟨styles, idx⟩

def applyIdx (s : State) (idx : Nat) : IO State := do
  pure { s with styles := ← loadIdx idx, themeIdx := idx }

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
