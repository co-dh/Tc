/-
  Filter: fzf-based column/row filtering and search.
  dispatch returns IO action directly; no intermediate Effect.
-/
import Tc.Fzf
import Tc.Render
import Tc.View
import Tc.Data.ADBC.Ops

namespace Tc.ViewStack

variable {T : Type} [TblOps T]

-- | Move row cursor to target index (pure helper)
private def moveRowTo (s : ViewStack T) (rowIdx : Nat) (search : Option (Nat × String) := none) : ViewStack T :=
  let v := s.cur
  let delta : Int := rowIdx - v.nav.row.cur
  let v' := (View.navL ∘ₗ NavState.rowCurL).modify (clampShift · delta v.nav.nRows) v
  s.setCur { v' with search := search.orElse (fun _ => v.search) }

-- | Move col cursor to target index (pure helper)
private def moveColTo (s : ViewStack T) (colIdx : Nat) : ViewStack T :=
  let v := s.cur
  let delta : Int := colIdx - v.nav.col.cur
  (View.navL ∘ₗ NavState.colCurL).modify (clampShift · delta v.nav.nCols) v |> s.setCur

-- | col search: fzf jump to column by name (IO version for backward compat)
def colSearch (test : Bool) (s : ViewStack T) : IO (ViewStack T) := do
  let some idx ← Fzf.fzfIdx test #["--prompt=Column: "] s.cur.nav.dispColNames | return s
  return moveColTo s idx

-- | Shared: resolve current column, fetch sorted distinct values
private def withDistinct (s : ViewStack T)
    (f : Nat → String → Array String → IO (ViewStack T)) : IO (ViewStack T) := do
  let v := s.cur
  let curCol := v.nav.curColIdx; let curName := v.nav.curColName
  let vals ← TblOps.distinct v.nav.tbl curCol
  vals.qsort (· < ·) |> f curCol curName

-- | row search (/): find value in current column, jump to matching row (IO)
def rowSearch (test : Bool) (s : ViewStack T) : IO (ViewStack T) :=
    withDistinct s fun curCol curName vals => do
  let some result ← Fzf.fzf test #[s!"--prompt=/{curName}: "] (vals.joinWith "\n") | return s
  let start := s.cur.nav.row.cur + 1
  let some rowIdx ← TblOps.findRow s.tbl curCol result start true | return s
  return moveRowTo s rowIdx (some (curCol, result))

-- | findRow with cache: fzf fires focus on every arrow key, so without caching
-- each fires a SQL query causing visible lag.
private def cachedFindRow [TblOps T] (cache : IO.Ref (Std.HashMap Nat (Option Nat)))
    (tbl : T) (col idx : Nat) (val : String) : IO (Option Nat) := do
  match (← cache.get).get? idx with
  | some hit => pure hit
  | none => let r ← TblOps.findRow tbl col val 0 true
            cache.modify (·.insert idx r); pure r

-- | Build poll callback for live search preview.
-- On each fzf focus change, finds the matching row (cached) and re-renders.
private def searchPoll [TblOps T] (tbl : T) (curCol : Nat) (vals : Array String)
    (sRef : IO.Ref (ViewStack T)) (preview : ViewStack T → IO Unit)
    : IO (IO.Ref (Std.HashMap Nat (Option Nat)) × IO Unit) := do
  let lastIdx ← IO.mkRef (none : Option Nat)
  let cache : IO.Ref (Std.HashMap Nat (Option Nat)) ← IO.mkRef {}
  let poll : IO Unit := do
    let some cmdStr ← Socket.pollCmd | pure ()
    let parts := cmdStr.splitOn " "
    if parts.headD "" != "search.preview" then return
    let some idx := (parts.getD 1 "").toNat? | pure ()
    if idx >= vals.size || (← lastIdx.get) == some idx then return
    lastIdx.set (some idx)
    let val := vals.getD idx ""
    let some rowIdx ← cachedFindRow cache tbl curCol idx val | pure ()
    let s' := moveRowTo (← sRef.get) rowIdx (some (curCol, val))
    sRef.set s'; preview s'
  pure (cache, poll)

-- | Row search with live preview: cursor moves as user browses fzf results.
-- fzf focus → shell script → socat → socket → poll → findRow → re-render.
def rowSearchLive (test : Bool) (s : ViewStack T) (preview : ViewStack T → IO Unit) : IO (ViewStack T) :=
  withDistinct s fun curCol curName vals => do
    if test then
      let result := vals.getD 0 ""
      if result.isEmpty then return s
      let some rowIdx ← TblOps.findRow s.tbl curCol result 0 true | return s
      return moveRowTo s rowIdx (some (curCol, result))
    -- setup: socat script for fzf execute-silent, indexed items for shell-safe args
    let sockPath ← Socket.getPath
    let script ← Tc.tmpPath "search-preview.sh"
    IO.FS.writeFile script s!"#!/bin/sh\necho \"search.preview $1\" | socat - UNIX-CONNECT:{sockPath}"
    let items := vals.mapIdx fun i v => s!"{i}\t{v}"
    let sRef ← IO.mkRef s
    let (cache, poll) ← searchPoll s.tbl curCol vals sRef preview
    -- run fzf with live preview polling
    let opts := #[s!"--prompt=/{curName}: ", "--with-nth=2..", "--delimiter=\t",
                  s!"--bind=focus:execute-silent(sh {script} \{1})"]
    let out ← Fzf.fzfCore test opts (items.joinWith "\n") poll
    if out.isEmpty then return ← sRef.get  -- cancelled: keep preview position
    -- apply final selection (reuse cache from preview)
    let some idx := out.splitOn "\t" |>.head? |>.bind String.toNat? | return s
    let result := vals.getD idx ""
    let some rowIdx ← cachedFindRow cache s.tbl curCol idx result | return s
    return moveRowTo s rowIdx (some (curCol, result))

-- | search in direction: fwd=true (n), bwd=false (N)
def searchDir (s : ViewStack T) (fwd : Bool) : IO (ViewStack T) := do
  let v := s.cur
  let some (col, val) := v.search | return s
  let start := if fwd then v.nav.row.cur + 1 else v.nav.row.cur
  let some rowIdx ← TblOps.findRow v.nav.tbl col val start fwd | return s
  return moveRowTo s rowIdx

-- | row filter (\): filter rows by expression, push filtered view (IO)
-- Uses TblOps.buildFilter for backend-specific syntax (PRQL vs q)
def rowFilter (test : Bool) (s : ViewStack T) : IO (ViewStack T) :=
    withDistinct s fun _curCol curName vals => do
  let typ := TblOps.colType s.tbl _curCol
  let header := TblOps.filterPrompt s.tbl curName (toString typ)
  let some result ← Fzf.fzf test #["--print-query", s!"--header={header}", "--prompt=filter > "] (vals.joinWith "\n") | return s
  let expr := TblOps.buildFilter s.tbl curName vals result typ.isNumeric
  if expr.isEmpty then return s
  let some tbl' ← TblOps.filter s.tbl expr | return s
  let some v' := s.cur.rebuild tbl' (row := 0) | return s
  return s.push { v' with disp := s!"\\{curName}" }

-- | Jump to column by name directly (no fzf). Called by socket/dispatch.
def colJumpWith (s : ViewStack T) (name : String) : IO (ViewStack T) := do
  if name.isEmpty then return s
  match s.cur.nav.dispColNames.findIdx? (· == name) with
  | some idx => pure (moveColTo s idx)
  | none => pure s

-- | Filter by expression directly (no fzf). Called by socket/dispatch.
def filterWith (s : ViewStack T) (expr : String) : IO (ViewStack T) := do
  if expr.isEmpty then return s
  let some tbl' ← TblOps.filter s.tbl expr | return s
  let some v' := s.cur.rebuild tbl' (row := 0) | return s
  return s.push { v' with disp := s!"\\{expr}" }

-- | Search for value directly (no fzf). Called by socket/dispatch.
def searchWith (s : ViewStack T) (val : String) : IO (ViewStack T) := do
  if val.isEmpty then return s
  let v := s.cur
  let curCol := v.nav.curColIdx
  let start := v.nav.row.cur + 1
  let some rowIdx ← TblOps.findRow s.tbl curCol val start true | return s
  return moveRowTo s rowIdx (some (curCol, val))

end Tc.ViewStack

namespace Tc.Filter

-- | Dispatch filter handler to IO action. Returns none if handler not recognized.
-- `test` suppresses fzf in colSearch/rowFilter when the -c harness is driving.
def dispatch (test : Bool) (s : ViewStack AdbcTable) (h : Cmd) : Option (IO (ViewStack AdbcTable)) :=
  match h with
  | .colSearch     => some (s.colSearch test)
  | .rowFilter     => some (s.rowFilter test)
  | .rowSearchNext => some (s.searchDir true)
  | .rowSearchPrev => some (s.searchDir false)
  | _ => none

end Tc.Filter

