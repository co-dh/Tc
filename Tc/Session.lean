/-
  Session: save/load view stack state to JSON files in ~/.cache/tv/sessions/.
  Serializes Prql.Query pipeline + view metadata; restores by re-executing queries.
  Uses Lean.Data.Json for serialization/deserialization.
-/
import Lean.Data.Json
import Tc.View
import Tc.Fzf
import Tc.Data.ADBC.Ops
import Tc.Folder

open Lean (Json ToJson FromJson toJson fromJson? JsonNumber)

namespace Tc.Session

-- | Session directory under ~/.cache/tv/
private def sessDir : IO String := do
  let dir : System.FilePath := s!"{← Log.dir}/sessions"
  IO.FS.createDirAll dir
  pure dir.toString

-- | Sanitize session name: keep only alphanumeric, dash, underscore, dot
def sanitize (name : String) : String :=
  String.ofList (name.toList.filter fun c => c.isAlphanum || c == '-' || c == '_' || c == '.')

-- | Session file path (name is sanitized to prevent path traversal)
private def sessPath (name : String) : IO String := do
  let safe := sanitize name
  if safe.isEmpty then throw (.userError "invalid session name")
  pure s!"{← sessDir}/{safe}.json"

/-! ## ToJson / FromJson instances -/

instance : ToJson Agg where toJson
  | .count => "count" | .sum => "sum" | .avg => "avg"
  | .min => "min" | .max => "max" | .stddev => "stddev" | .dist => "dist"

instance : FromJson Agg where fromJson? j := do
  match ← j.getStr? with
  | "count" => pure .count | "sum" => pure .sum | "avg" => pure .avg
  | "min" => pure .min | "max" => pure .max | "stddev" => pure .stddev | "dist" => pure .dist
  | s => throw s!"unknown agg: {s}"

instance : ToJson Op where toJson
  | .filter e => Json.mkObj [("type", "filter"), ("expr", toJson e)]
  | .sort cols => Json.mkObj [("type", "sort"), ("cols", toJson cols)]
  | .sel cols => Json.mkObj [("type", "sel"), ("cols", toJson cols)]
  | .exclude cols => Json.mkObj [("type", "exclude"), ("cols", toJson cols)]
  | .derive bs => Json.mkObj [("type", "derive"), ("bindings", toJson bs)]
  | .group keys aggs =>
    let as := aggs.map fun (fn, name, col) => Json.arr #[toJson fn, toJson name, toJson col]
    Json.mkObj [("type", "group"), ("keys", toJson keys), ("aggs", toJson as)]
  | .take n => Json.mkObj [("type", "take"), ("n", toJson n)]

instance : FromJson Op where fromJson? j := do
  match ← j.getObjValAs? String "type" with
  | "filter" => pure (.filter (← j.getObjValAs? String "expr"))
  | "sort" => pure (.sort (← j.getObjValAs? _ "cols"))
  | "sel" => pure (.sel (← j.getObjValAs? _ "cols"))
  | "exclude" => pure (.exclude (← j.getObjValAs? _ "cols"))
  | "derive" => pure (.derive (← j.getObjValAs? _ "bindings"))
  | "group" =>
    let keys ← j.getObjValAs? (Array String) "keys"
    let rawAggs ← j.getObjValAs? (Array (Array Json)) "aggs"
    let aggs ← rawAggs.mapM fun a => do
      if h : a.size ≥ 3 then
        let fn ← fromJson? a[0]
        let name ← fromJson? a[1]
        let col ← fromJson? a[2]
        pure (fn, name, col)
      else throw "agg triple expected"
    pure (.group keys aggs)
  | "take" => pure (.take (← j.getObjValAs? Nat "n"))
  | t => throw s!"unknown op type: {t}"

instance : ToJson ViewKind where toJson
  | .tbl => Json.mkObj [("kind", "tbl")]
  | .freqV cols total => Json.mkObj [("kind", "freqV"), ("cols", toJson cols), ("total", toJson total)]
  | .colMeta => Json.mkObj [("kind", "colMeta")]
  | .fld path depth => Json.mkObj [("kind", "fld"), ("path", toJson path), ("depth", toJson depth)]

instance : FromJson ViewKind where fromJson? j := do
  match ← j.getObjValAs? String "kind" with
  | "freqV" => pure (.freqV (← j.getObjValAs? _ "cols") (← j.getObjValAs? Nat "total"))
  | "colMeta" => pure .colMeta
  | "fld" => pure (.fld (← j.getObjValAs? String "path") (← j.getObjValAs? Nat "depth"))
  | _ => pure .tbl

/-! ## Serialization: View → JSON -/

private def viewToJson (v : View AdbcTable) : Json :=
  let q := v.nav.tbl.query
  let search : Json := match v.search with
    | some (i, s) => Json.mkObj [("col", toJson i), ("val", toJson s)]
    | none => .null
  Json.mkObj [
    ("path", toJson v.path), ("vkind", toJson v.vkind),
    ("disp", toJson v.disp), ("precAdj", toJson v.precAdj), ("widthAdj", toJson v.widthAdj),
    ("row", toJson v.nav.row.cur.val), ("col", toJson v.nav.col.cur.val),
    ("grp", toJson v.nav.grp), ("hidden", toJson v.nav.hidden),
    ("colSels", toJson v.nav.col.sels), ("search", search),
    ("query", Json.mkObj [("base", toJson q.base), ("ops", toJson q.ops)])
  ]

private def stackToJson (s : ViewStack AdbcTable) : String :=
  let views := (s.hd :: s.tl).map viewToJson
  Json.compress <| Json.mkObj [("version", (1 : Nat)), ("views", toJson views)]

/-! ## Deserialization: JSON → View state -/

-- | Restore a single view from JSON, re-executing the query pipeline.
--   Errors are caught per-view so partial restoration works.
private def restoreView (j : Json) : IO (Option (View AdbcTable)) := do
  let path := (j.getObjValAs? String "path").toOption.getD ""
  if path.isEmpty then return none
  let vkind := (j.getObjValAs? ViewKind "vkind").toOption.getD .tbl
  let disp := (j.getObjValAs? String "disp").toOption.getD ""
  let precAdj := (j.getObjValAs? Int "precAdj").toOption.getD 0
  let widthAdj := (j.getObjValAs? Int "widthAdj").toOption.getD 0
  let row := (j.getObjValAs? Nat "row").toOption.getD 0
  let col := (j.getObjValAs? Nat "col").toOption.getD 0
  let grp := (j.getObjValAs? (Array String) "grp").toOption.getD #[]
  let hidden := (j.getObjValAs? (Array String) "hidden").toOption.getD #[]
  let colSels := (j.getObjValAs? (Array String) "colSels").toOption.getD #[]
  let search : Option (Nat × String) := do
    let s ← (j.getObjVal? "search").toOption
    if s.isNull then none
    else some ((s.getObjValAs? Nat "col").toOption.getD 0,
               (s.getObjValAs? String "val").toOption.getD "")
  let qObj := j.getObjValD "query"
  let base := (qObj.getObjValAs? String "base").toOption.getD s!"from `{path}`"
  let ops := (qObj.getObjValAs? (Array Op) "ops").toOption.getD #[]
  let query : Prql.Query := { base, ops }
  let tbl? ← try
    match vkind with
    | .fld p depth => (·.map (·.nav.tbl)) <$> Folder.mkView p depth
    | _ => do let total ← AdbcTable.queryCount query; AdbcTable.requery query total
  catch e => Log.write "session" s!"skip view: {path} ({e})"; pure none
  let some tbl := tbl? | return none
  let nRows := TblOps.nRows tbl
  let nCols := (TblOps.colNames tbl).size
  if nRows == 0 || nCols == 0 then return none
  match View.fromTbl tbl path (min col (nCols - 1)) grp (min row (nRows - 1)) with
  | some view => pure (some { view with
      vkind, disp, precAdj, widthAdj, search
      nav := { view.nav with hidden, col := { view.nav.col with sels := colSels } } })
  | none => pure none

/-! ## Public API -/

-- | Derive session name from view stack (like export uses tabName)
def autoName (stk : ViewStack AdbcTable) : String :=
  let name := stk.cur.tabName.replace "/" "_" |>.replace " " "_"
  let stem := (name.splitOn ".").head?.filter (!·.isEmpty) |>.getD name
  sanitize stem

def save (stk : ViewStack AdbcTable) (name : String := autoName stk) : IO Unit := do
  let path ← sessPath name
  IO.FS.writeFile path (stackToJson stk)
  Log.write "session" s!"saved {(stk.hd :: stk.tl).length} view(s) to {path}"
  statusMsg s!"session saved: {name}"

def load (name : String) : IO (Option (ViewStack AdbcTable)) := do
  let path ← sessPath name
  let content ← try IO.FS.readFile path catch _ => return none
  let .ok json := Json.parse content | return none
  let views := (json.getObjValAs? (Array Json) "views").toOption.getD #[]
  if views.isEmpty then return none
  let mut restored : Array (View AdbcTable) := #[]
  for v in views do
    match ← restoreView v with
    | some view => restored := restored.push view
    | none => pure ()
  if h : restored.size > 0 then
    pure (some ⟨restored[0], (restored.extract 1 restored.size).toList⟩)
  else pure none

def list : IO (Array String) := do
  let dir ← sessDir
  let mut names : Array String := #[]
  try
    for entry in ← System.FilePath.readDir dir do
      let name := entry.fileName
      if name.endsWith ".json" then
        names := names.push ((name.take (name.length - 5)).toString)
  catch _ => pure ()  -- dir may not exist yet
  pure names

-- | Prompt for session name; returns none on cancel or empty input
def pickSaveName : IO (Option String) := do
  let existing ← list
  let input := "\n".intercalate existing.toList
  match ← Fzf.fzf #["--prompt=session name: ", "--print-query"] input with
  | none => pure none
  | some s =>
    let lines := s.splitOn "\n" |>.filter (!·.isEmpty)
    let name := sanitize (lines.getLast?.getD (lines.headD ""))
    pure (if name.isEmpty then none else some name)

def pickLoadName : IO (Option String) := do
  let existing ← list
  if existing.isEmpty then statusMsg "no saved sessions"; return none
  Fzf.fzf #["--prompt=load session: "] ("\n".intercalate existing.toList)
    |>.map (·.map (·.trimAscii.toString))

-- | Save session with explicit name (no fzf). Called by socket/dispatch.
def saveWith (stk : ViewStack AdbcTable) (name : String) : IO Unit :=
  if name.isEmpty then save stk  -- empty name → use auto name
  else save stk (sanitize name)

-- | Load session by name directly (no fzf). Called by socket/dispatch.
def loadWith (name : String) : IO (Option (ViewStack AdbcTable)) :=
  if name.isEmpty then pure none else load name

end Tc.Session
