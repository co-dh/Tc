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

-- | Color name → terminal value (for theme.csv parsing)
private def colorMap : Std.HashMap String UInt32 :=
  .ofList [
    ("default", Term.default), ("black", Term.black),
    ("red", Term.red), ("green", Term.green), ("yellow", Term.yellow),
    ("blue", Term.blue), ("magenta", Term.magenta), ("cyan", Term.cyan), ("white", Term.white),
    ("brBlack", Term.brBlack), ("brRed", Term.brRed), ("brGreen", Term.brGreen),
    ("brYellow", Term.brYellow), ("brBlue", Term.brBlue), ("brMagenta", Term.brMagenta),
    ("brCyan", Term.brCyan), ("brWhite", Term.brWhite),
    ("slate", Term.slate), ("sky", Term.sky), ("mint", Term.mint),
    ("peach", Term.peach), ("lavender", Term.lavender),
    ("gray234", Term.gray234), ("gray236", Term.gray236), ("gray238", Term.gray238),
    ("gray240", Term.gray240), ("gray250", Term.gray250), ("gray252", Term.gray252),
    ("frost", Term.frost), ("aurora", Term.aurora), ("polar", Term.polar), ("snow", Term.snow),
    ("pink", Term.pink), ("purple", Term.purple),
    ("orange", Term.orange), ("olive", Term.olive), ("sand", Term.sand),
    ("cream", Term.cream), ("brown", Term.brown), ("coral", Term.coral), ("violet", Term.violet)]

@[inline] def parseColor (s : String) : UInt32 := colorMap.getD s Term.default

-- | Style names (index into the 18-element styles array)
def styleNames : Array String := #[
  "cursor", "selRow", "selColCurRow", "selCol",
  "curRow", "curCol", "default", "header", "group"
]

def parseStyle (s : String) : Option Nat := styleNames.idxOf? s

-- | Default dark theme (fallback if CSV fails)
def defaultDark : Array UInt32 := #[
  Term.black, Term.brWhite,      -- cursor
  Term.black, Term.mint,         -- selRow
  Term.black, Term.lavender,     -- selColCurRow
  Term.brMagenta, Term.default,  -- selCol
  Term.default, Term.gray234,    -- curRow
  Term.default, Term.gray238,    -- curCol
  Term.default, Term.default,    -- default
  Term.green, Term.slate,        -- header
  Term.default, Term.sky         -- group
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

-- | Theme CSV path (next to binary, or CWD)
def csvPath : IO String := do
  let bin ← IO.appPath
  let dir := bin.toString.splitOn "/" |>.dropLast |> "/".intercalate
  let p := s!"{dir}/theme.csv"
  try let _ ← IO.FS.Handle.mk p .read; pure p
  catch _ => pure "theme.csv"

-- | Load theme from CSV by theme/variant name
def load (path theme variant : String) : IO (Array UInt32) := do
  let content ← IO.FS.readFile path
  let matching := content.splitOn "\n"
    |>.filter (·.length > 0)
    |>.drop 1
    |>.map (·.splitOn ",")
    |>.filter fun r => r.getD 0 "" == theme && r.getD 1 "" == variant
  let mut styles := defaultDark
  for row in matching do
    if let some idx := parseStyle (row.getD 2 "") then
      styles := styles.set! (idx * 2) (parseColor (row.getD 3 "default"))
      styles := styles.set! (idx * 2 + 1) (parseColor (row.getD 4 "default"))
  return styles

-- | Load theme by index
def loadIdx (idx : Nat) : IO (Array UInt32) := do
  let (theme, variant) := themes.getD idx ("default", "dark")
  load (← csvPath) theme variant <|> pure defaultDark

namespace State

def init : IO State := do
  let dark ← isDark
  let variant := if dark then "dark" else "light"
  let styles ← load (← csvPath) "default" variant <|> pure defaultDark
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
    "--prompt=theme: ", "--with-nth=2..",
    s!"--bind=focus:execute-silent({script} \{1})"]
  let out ← Fzf.fzfCore opts (items.joinWith "\n") poll
  if out.isEmpty then return none
  match out.splitOn "\t" |>.head? |>.bind String.toNat? with
  | some idx => some <$> cur.applyIdx idx
  | none => return none

end Tc.Theme
