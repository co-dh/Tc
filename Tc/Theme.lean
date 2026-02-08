/-
  Theme: CSV-based color themes
  Format: theme,variant,name,fg,bg
-/
import Tc.Cmd
import Tc.Term

namespace Tc.Theme

-- | Theme state
structure State where
  styles   : Array UInt32
  themeIdx : Nat

-- | Color name to UInt32 lookup
def colorMap : Array (String × UInt32) := #[
  ("default", Term.default),
  ("black", Term.black),
  ("red", Term.red),
  ("green", Term.green),
  ("yellow", Term.yellow),
  ("blue", Term.blue),
  ("magenta", Term.magenta),
  ("cyan", Term.cyan),
  ("white", Term.white),
  ("brBlack", Term.brBlack),
  ("brRed", Term.brRed),
  ("brGreen", Term.brGreen),
  ("brYellow", Term.brYellow),
  ("brBlue", Term.brBlue),
  ("brMagenta", Term.brMagenta),
  ("brCyan", Term.brCyan),
  ("brWhite", Term.brWhite),
  ("slate", Term.slate),
  ("sky", Term.sky),
  ("mint", Term.mint),
  ("peach", Term.peach),
  ("lavender", Term.lavender),
  ("gray234", Term.gray234),
  ("gray240", Term.gray240),
  ("gray252", Term.gray252)
]

-- | Parse color name to UInt32
def parseColor (s : String) : UInt32 :=
  colorMap.findSome? (fun (n, c) => if n == s then some c else none) |>.getD Term.default

-- | Style name to index
def styleIdx : Array String := #[
  "cursor", "selRow", "selColCurRow", "selCol",
  "curRow", "curCol", "default", "header", "group"
]

-- | Parse style name to index
def parseStyle (s : String) : Option Nat := styleIdx.idxOf? s

-- | Default dark theme (fallback if CSV fails)
def defaultDark : Array UInt32 := #[
  Term.black, Term.brWhite,      -- cursor
  Term.black, Term.mint,         -- selRow
  Term.black, Term.lavender,     -- selColCurRow
  Term.brMagenta, Term.default,  -- selCol
  Term.default, Term.gray234,    -- curRow
  Term.brYellow, Term.default,   -- curCol
  Term.default, Term.default,    -- default
  Term.brWhite, Term.slate,      -- header
  Term.default, Term.sky         -- group
]

-- | Detect terminal background: dark (true) or light (false)
-- Uses COLORFGBG env var (format: "fg;bg", bg < 7 = dark)
def isDark : IO Bool := do
  match (← IO.getEnv "COLORFGBG") with
  | some s =>
    let parts := s.splitOn ";"
    match parts.getLast?.bind (·.toNat?) with
    | some bg => pure (bg < 7)  -- 0-6 = dark colors
    | none => pure true  -- default dark
  | none => pure true  -- default dark

-- | Available themes: (theme, variant) pairs
def themes : Array (String × String) := #[
  ("default", "dark"), ("default", "light"),
  ("ansi", "dark"), ("ansi", "light")
]

-- | Get theme index from name/variant
def themeIdx (theme variant : String) : Nat :=
  themes.findIdx? (· == (theme, variant)) |>.getD 0

-- | Cycle theme index by delta, return new (theme, variant)
def cycleTheme (idx : Nat) (delta : Int) : String × String :=
  let n := themes.size
  let newIdx := ((idx : Int) + delta).toNat % n
  themes.getD newIdx ("default", "dark")

-- | Load theme CSV, filter by theme/variant, return styles array
def load (path : String) (theme variant : String) : IO (Array UInt32) := do
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n" |>.filter (·.length > 0)
  let rows := lines.drop 1 |>.map (·.splitOn ",")  -- skip header
  -- filter matching theme/variant
  let matching := rows.filter fun r =>
    r.getD 0 "" == theme && r.getD 1 "" == variant
  -- build styles array (9 styles × 2 = 18 values: fg0,bg0,fg1,bg1,...)
  let mut styles := defaultDark  -- start with default
  for row in matching do
    if let some idx := parseStyle (row.getD 2 "") then
      let fg := parseColor (row.getD 3 "default")
      let bg := parseColor (row.getD 4 "default")
      styles := styles.set! (idx * 2) fg
      styles := styles.set! (idx * 2 + 1) bg
  return styles

-- | Cycle theme by delta, returns (newStyles, newIdx)
def doCycle (idx : Nat) (delta : Int) : IO (Array UInt32 × Nat) := do
  let (theme, variant) := cycleTheme idx delta
  let newIdx := themeIdx theme variant
  let styles ← load "theme.csv" theme variant <|> pure defaultDark
  pure (styles, newIdx)

namespace State

-- | Initialize theme: detect dark/light, load default theme
def init : IO State := do
  let dark ← isDark
  let variant := if dark then "dark" else "light"
  let styles ← load "theme.csv" "default" variant <|> pure defaultDark
  pure ⟨styles, Theme.themeIdx "default" variant⟩

-- | Pure update: returns Effect.themeLoad to defer IO
def update (s : State) (cmd : Cmd) : Option (State × Effect) :=
  match cmd with
  | .thm .inc => some (s, .themeLoad 1)    -- runner will load and update
  | .thm .dec => some (s, .themeLoad (-1))
  | _ => none

instance : Update State where update := update

-- | Execute theme effect: load theme with delta
def runEffect (s : State) (delta : Int) : IO State := do
  let (sty, idx) ← doCycle s.themeIdx delta
  pure ⟨sty, idx⟩

end State
end Tc.Theme
