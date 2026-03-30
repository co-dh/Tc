/-
  Screen tests whose core logic is now covered by pure theorems in TestPure.lean.
  Kept as backup — these additionally test the rendering pipeline (C FFI, termbox buffer).
  Run: lake run runscreen
-/
import Tc.Data.ADBC.Table
import test.TestUtil

namespace ScreenBackup

open Tc TestUtil

-- === Navigation ===
-- Pure: key_j, key_k, key_l, key_h (key→cmd mapping)
-- Pure: nav_row_inverse (j then k = identity)
-- Pure: nav_col_inverse (l then h = identity)

def test_nav_down : IO Unit := do
  log "nav_down"
  assert (contains (footer (← run "j" "data/basic.csv")).2 "r1/") "j moves to row 1"

def test_nav_right : IO Unit := do
  log "nav_right"
  assert (contains (footer (← run "l" "data/basic.csv")).2 "c1/") "l moves to col 1"

def test_nav_up : IO Unit := do
  log "nav_up"
  assert (contains (footer (← run "jk" "data/basic.csv")).2 "r0/") "jk returns to row 0"

def test_nav_left : IO Unit := do
  log "nav_left"
  assert (contains (footer (← run "lh" "data/basic.csv")).2 "c0/") "lh returns to col 0"

-- === Key columns ===
-- Pure: key_bang (! → grp.ent)
-- Pure: grp_toggle_inverse (!! returns to empty groups)
-- Pure: nav_grp_col (l! groups c1)
-- Pure: nav_disp_grp_first (grouped col appears at position 0)
-- Pure: dispOrder_grp_first in Nav.lean

def test_key_toggle : IO Unit := do
  log "key_col"
  assert (contains (header (← run "!" "data/basic.csv")) "║") "! adds key separator"

def test_key_remove : IO Unit := do
  log "key_remove"
  assert (!contains (header (← run "!!" "data/basic.csv")) "║") "!! removes key separator"

def test_key_reorder : IO Unit := do
  log "key_reorder"
  assert ((header (← run "l!" "data/basic.csv")).take 5 |>.any (· == 'b')) "Key col moves to front"

-- === Hide ===
-- H key bound to nav.colHide.
-- Logic covered by pure theorems: hidden_toggle_inverse, nav_hide in TestPure.lean.

-- === Selection ===
-- Pure: key_T (T → rowSel.ent)
-- Pure: sel_accumulation (TjT produces #[0, 1])

def test_row_select : IO Unit := do
  log "row_select"
  assert (contains (footer (← run "T" "data/basic.csv")).2 "sel=1") "T selects row"

def test_multi_select : IO Unit := do
  log "multi_row_select"
  assert (contains (footer (← run "TjT" "data/full.csv")).2 "sel=2") "TjT selects 2 rows"

-- === Stack ===
-- Pure: key_S (S → stk.ent), key_q (q → stk.dec)
-- Pure: ViewStack.update stk.ent (swap is identity on single stack)
-- Pure: ViewStack.update stk.dec (pop with parent, quit on empty)

def test_stack_swap : IO Unit := do
  log "stack_swap"
  assert (contains (footer (← run "S" "data/basic.csv")).1 "basic.csv") "S swaps/dups view"

def test_meta_quit : IO Unit := do
  log "meta_quit"
  assert (!contains (footer (← run "Mq" "data/basic.csv")).1 "meta") "Mq returns from meta"

def test_freq_quit : IO Unit := do
  log "freq_quit"
  assert (!contains (footer (← run "Fq" "data/basic.csv")).1 "freq") "Fq returns from freq"

-- === Info overlay ===
-- Pure: key_I (I → info.ent)
-- Pure: Info.State.update (toggle vis on/off, returns Effect.none)

-- Info overlay hidden by default; I toggles it on
def test_info : IO Unit := do
  log "info"
  let output ← run "I" "data/basic.csv"
  assert (contains output "derive" || contains output "export") "Info overlay shown after I toggle"

-- === Cursor tracking ===
-- Pure: nav_grp_col (l! groups c1, cursor tracks)

def test_key_cursor : IO Unit := do
  log "key_cursor"
  assert (contains (footer (← run "l!" "data/basic.csv")).2 "c0/") "Cursor tracks after key toggle"

-- === Quit ===
-- Pure: ViewStack.update stk.dec on empty stack → Effect.quit

def test_q_quit : IO Unit := do
  log "q_quit_empty_stack"
  let out ← IO.Process.output { cmd := bin, args := #["data/basic.csv", "-c", "q"] }
  assert (out.exitCode == 0) "q on empty stack exits cleanly"

-- === Run all backup screen tests ===

def tests : Array (String × IO Unit) := #[
  ("nav_down", test_nav_down), ("nav_right", test_nav_right),
  ("nav_up", test_nav_up), ("nav_left", test_nav_left),
  ("key_toggle", test_key_toggle), ("key_remove", test_key_remove),
  ("key_reorder", test_key_reorder),
  ("row_select", test_row_select), ("multi_select", test_multi_select),
  ("stack_swap", test_stack_swap), ("meta_quit", test_meta_quit),
  ("freq_quit", test_freq_quit), ("info", test_info),
  ("key_cursor", test_key_cursor), ("q_quit", test_q_quit)
]

def main (args : List String) : IO Unit := do
  IO.FS.writeFile "test.log" ""
  let err ← Tc.AdbcTable.init
  if !err.isEmpty then throw (IO.userError s!"Backend init failed: {err}")
  let filter := args.head?
  let selected := match filter with
    | none => tests
    | some f => tests.filter fun (name, _) => (name.splitOn f).length > 1
  if selected.isEmpty then
    IO.eprintln s!"No tests matching '{filter.getD ""}'"
    return
  IO.println s!"Running {selected.size} backup screen test(s)...\n"
  for (_, action) in selected do action
  Tc.AdbcTable.shutdown
  IO.println "\nAll backup screen tests passed!"

end ScreenBackup

def main (args : List String) : IO Unit := ScreenBackup.main args
