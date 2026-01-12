/-
  App for tc-core: MemTable only, no ADBC/Kdb
-/
import Tc.Fzf
import Tc.Key
import Tc.Meta
import Tc.Render
import Tc.Term
import Tc.View.Mem
import Tc.Data.Mem.Meta
import Tc.Data.Mem.Freq
import Tc.UI.Info

open Tc

-- Uses Meta.sel and Meta.setKey for meta view operations

-- | AppState for tc-core (MemTable only)
abbrev AppState := GAppState MemTable

namespace AppState

def update (a : AppState) (cmd : Cmd) : Option (AppState × Effect) :=
  match GView.update a.stk.cur cmd 20 with
  | some (v', eff) => some ({ a with stk := a.stk.setCur v' }, eff)
  | none =>
    let n := a.stk.cur.nav; let names := ReadTable.colNames n.tbl
    match cmd with
    | .info .ent => some ({ a with info := { vis := !a.info.vis } }, .none)
    | .stk .dec => match a.stk.pop with
      | some s' => some ({ a with stk := s' }, .none)
      | none => some (a, .quit)  -- can't pop → quit
    | .stk .ent => some ({ a with stk := a.stk.swap }, .none)
    | .stk .dup => some ({ a with stk := a.stk.dup }, .none)
    | .metaV .dup => some (a, .queryMeta)
    | .metaV .dec => some ({ a with stk := Meta.sel a.stk Meta.selNull }, .none)
    | .metaV .inc => some ({ a with stk := Meta.sel a.stk Meta.selSingle }, .none)
    | .metaV .ent => Meta.setKey a.stk |>.map fun s' => ({ a with stk := s' }, .none)
    | .freq .dup =>
      let curCol := colIdxAt n.grp names n.col.cur.val
      let curName := names.getD curCol ""
      let colNames := if n.grp.contains curName then n.grp else n.grp.push curName
      let colIdxs' := colNames.filterMap names.idxOf?
      some (a, .queryFreq colIdxs' colNames)
    | .freq .ent => match a.stk.cur.vkind with
      | .freqV cols => some (a, .freqFilter cols n.row.cur.val)
      | _ => none
    | .col .ent => some (a, .fzfCol)  -- s: column picker
    | .rowSel .inc =>  -- /: row search
      let curCol := colIdxAt n.grp names n.col.cur.val
      some (a, .fzfRow curCol (names.getD curCol ""))
    | .rowSel .dec =>  -- \: row filter
      let curCol := colIdxAt n.grp names n.col.cur.val
      some (a, .fzfFilter curCol (names.getD curCol ""))
    | .grp .inc => some (a, .findNext)  -- n: search next
    | .grp .dec => some (a, .findPrev)  -- N: search prev
    | _ => none

end AppState

partial def runEffect (a : AppState) (eff : Effect) : IO AppState := do
  match eff with
  | .none | .quit => pure a
  | .fzfCmd =>
    match ← Fzf.cmdMode a.stk.cur.vkind with
    | some cmd => match a.update cmd with
      | some (a', eff') => if eff'.isNone then pure a' else runEffect a' eff'
      | none => pure a
    | none => pure a
  | .themeLoad delta => pure { a with theme := ← a.theme.runEffect delta }
  | .queryDel curCol sels grp =>
    let (tbl, grp') ← ModifyTable.del a.stk.cur.nav.tbl curCol sels grp
    match GView.fromTbl tbl a.stk.cur.path a.stk.cur.nav.col.cur.val grp' 0 with
    | some v => pure { a with stk := a.stk.setCur v }
    | none => pure a
  | .querySort col grpIdxs asc =>
    let tbl ← ModifyTable.sortBy (#[col] ++ grpIdxs) asc a.stk.cur.nav.tbl
    match GView.fromTbl tbl a.stk.cur.path col a.stk.cur.nav.grp a.stk.cur.nav.row.cur.val with
    | some v => pure { a with stk := a.stk.setCur v }
    | none => pure a
  | .queryMeta =>
    let m ← MemTable.queryMeta a.stk.cur.nav.tbl
    match GView.fromTbl (Meta.toMemTable m) a.stk.cur.path with
    | some v => pure { a with stk := a.stk.push { v with vkind := .colMeta, disp := "meta" } }
    | none => pure a
  | .queryFreq colIdxs colNames =>
    let f ← MemTable.queryFreq a.stk.cur.nav.tbl colIdxs
    match GView.fromTbl (Freq.toMemTable f) a.stk.cur.path 0 colNames with
    | some v => pure { a with stk := a.stk.push { v with vkind := .freqV colNames, disp := s!"freq {colNames.join ","}" } }
    | none => pure a
  | .freqFilter cols row =>
    match a.stk.cur.vkind, a.stk.pop with
    | .freqV _, some s' =>
      let expr := Freq.filterExpr a.stk.cur.nav.tbl cols row
      match ← MemTable.filter s'.cur.nav.tbl expr with
      | some tbl' => match GView.fromTbl tbl' s'.cur.path 0 s'.cur.nav.grp 0 with
        | some v => pure { a with stk := s'.push v }
        | none => pure a
      | none => pure a
    | _, _ => pure a
  | .fzfCol => pure { a with stk := ← a.stk.colSearch }
  | .fzfRow _ _ => pure { a with stk := ← a.stk.rowSearch }
  | .fzfFilter _ _ => pure { a with stk := ← a.stk.rowFilter }
  | .findNext => pure { a with stk := ← a.stk.searchNext }
  | .findPrev => pure { a with stk := ← a.stk.searchPrev }
  | _ => pure a

partial def mainLoop (a : AppState) (testMode : Bool) (keys : Array Char) : IO AppState := do
  let (vs', v') ← a.stk.cur.doRender a.vs a.theme.styles
  let a := { a with stk := a.stk.setCur v', vs := vs' }
  renderTabLine a.stk.tabNames 0
  if a.info.vis then UI.Info.render (← Term.height).toNat (← Term.width).toNat a.stk.cur.vkind
  Term.present
  if testMode && keys.isEmpty then IO.print (← Term.bufferStr); return a
  let (ev, keys') ← nextEvent keys
  if isKey ev 'Q' then return a
  if isKey ev ' ' then mainLoop (← runEffect a .fzfCmd) testMode keys'
  else
    let some cmd := evToCmd ev a.stk.cur.vkind | mainLoop a testMode keys'
    let some (a', eff) := a.update cmd | mainLoop a testMode keys'
    if eff == .quit then return a'
    mainLoop (← if eff.isNone then pure a' else runEffect a' eff) testMode keys'

def main (args : List String) : IO Unit := do
  let (path, keys) := match args with
    | p :: "-c" :: k :: _ => (p, (parseKeys k).toList.toArray)
    | p :: _ => (p, #[])
    | [] => ("", #[])
  if path.isEmpty then IO.eprintln "Usage: tc-core <file.csv>"; return
  let testMode := !keys.isEmpty  -- only from -c flag, not env var
  let envTest := (← IO.getEnv "TC_TEST_MODE").isSome
  unless testMode || envTest || (← Term.isattyStdin) do IO.eprintln "Error: requires TTY (or set TC_TEST_MODE=1)"; return
  Fzf.setTestMode (testMode || envTest)
  match ← View.fromFile path with
  | some v =>
    let _ ← Term.init
    let _ ← mainLoop { stk := ⟨#[v], by simp⟩, theme := ← Theme.State.init } testMode keys
    Term.shutdown
  | none => IO.eprintln "Cannot open file"
