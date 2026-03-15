/-
  Session: save/load view stack state to JSON files in ~/.cache/tc/sessions/.
  Serializes Prql.Query pipeline + view metadata; restores by re-executing queries.
-/
import Tc.View
import Tc.Fzf
import Tc.Data.ADBC.Ops
import Tc.Folder

namespace Tc.Session

-- | Session directory under ~/.cache/tc/
private def sessDir : IO String := do
  let home := (← IO.getEnv "HOME").getD "/tmp"
  let dir := s!"{home}/.cache/tc/sessions"
  let _ ← IO.Process.output { cmd := "mkdir", args := #["-p", dir] }
  pure dir

-- | Sanitize session name: keep only alphanumeric, dash, underscore, dot
private def sanitize (name : String) : String :=
  String.ofList (name.toList.filter fun c => c.isAlphanum || c == '-' || c == '_' || c == '.')

-- | Session file path (name is sanitized to prevent path traversal)
private def sessPath (name : String) : IO String := do
  let safe := sanitize name
  if safe.isEmpty then throw (.userError "invalid session name")
  pure s!"{← sessDir}/{safe}.json"

/-! ## Serialization: View → JSON lines -/

-- | Escape string for JSON (quotes, backslashes, control chars)
private def jsonStr (s : String) : String :=
  let s := (s.replace "\\" "\\\\").replace "\"" "\\\""
  let s := (s.replace "\n" "\\n").replace "\r" "\\r"
  let s := s.replace "\t" "\\t"
  "\"" ++ s ++ "\""

-- | Serialize Op to JSON object string
private def opToJson : Op → String
  | .filter e => s!"\{\"type\":\"filter\",\"expr\":{jsonStr e}}"
  | .sort cols =>
    let cs := cols.map fun (c, asc) => s!"[{jsonStr c},{asc}]"
    s!"\{\"type\":\"sort\",\"cols\":[{",".intercalate cs.toList}]}"
  | .sel cols =>
    let cs := cols.map jsonStr
    s!"\{\"type\":\"sel\",\"cols\":[{",".intercalate cs.toList}]}"
  | .derive bs =>
    let ds := bs.map fun (n, e) => s!"[{jsonStr n},{jsonStr e}]"
    s!"\{\"type\":\"derive\",\"bindings\":[{",".intercalate ds.toList}]}"
  | .group keys aggs =>
    let ks := keys.map jsonStr
    let as := aggs.map fun (fn, name, col) => s!"[{jsonStr fn.short},{jsonStr name},{jsonStr col}]"
    s!"\{\"type\":\"group\",\"keys\":[{",".intercalate ks.toList}],\"aggs\":[{",".intercalate as.toList}]}"
  | .take n => s!"\{\"type\":\"take\",\"n\":{n}}"

-- | Serialize ViewKind to JSON
private def vkindToJson : ViewKind → String
  | .tbl => s!"\{\"kind\":\"tbl\"}"
  | .freqV cols total =>
    let cs := cols.map jsonStr
    s!"\{\"kind\":\"freqV\",\"cols\":[{",".intercalate cs.toList}],\"total\":{total}}"
  | .colMeta => s!"\{\"kind\":\"colMeta\"}"
  | .fld path depth => s!"\{\"kind\":\"fld\",\"path\":{jsonStr path},\"depth\":{depth}}"

-- | Serialize a single View to JSON
private def viewToJson (v : View AdbcTable) : String :=
  let q := v.nav.tbl.query
  let ops := q.ops.map opToJson
  let grp := v.nav.grp.map jsonStr
  let hidden := v.nav.hidden.map jsonStr
  let colSels := v.nav.col.sels.map jsonStr
  let search := match v.search with
    | some (i, s) => s!"\{\"col\":{i},\"val\":{jsonStr s}}"
    | none => "null"
  s!"\{\"path\":{jsonStr v.path},\"vkind\":{vkindToJson v.vkind},\"disp\":{jsonStr v.disp},\"precAdj\":{v.precAdj},\"widthAdj\":{v.widthAdj},\"row\":{v.nav.row.cur.val},\"col\":{v.nav.col.cur.val},\"grp\":[{",".intercalate grp.toList}],\"hidden\":[{",".intercalate hidden.toList}],\"colSels\":[{",".intercalate colSels.toList}],\"search\":{search},\"query\":\{\"base\":{jsonStr q.base},\"ops\":[{",".intercalate ops.toList}]}}"

-- | Serialize full stack to JSON
private def stackToJson (s : ViewStack AdbcTable) : String :=
  let views := (s.hd :: s.tl).toArray.map viewToJson
  s!"\{\"version\":1,\"views\":[{",".intercalate views.toList}]}"

/-! ## Deserialization: JSON → View state -/

-- | Minimal JSON value type for parsing session files
private inductive JVal where
  | str (v : String)
  | num (v : Int)
  | bool (v : Bool)
  | null
  | arr (vs : Array JVal)
  | obj (kvs : Array (String × JVal))
  deriving Inhabited

-- | Get char at byte position
private def charAt (s : String) (i : Nat) : Char :=
  if i < s.length then String.Pos.Raw.get s ⟨i⟩ else '\x00'

private partial def skipWS (s : String) (i : Nat) : Nat :=
  if i < s.length then
    let c := charAt s i
    if c == ' ' || c == '\n' || c == '\r' || c == '\t' then skipWS s (i + 1) else i
  else i

-- | Parse a JSON string literal starting at position i (after opening quote)
private partial def parseStr (s : String) (i : Nat) : String × Nat := Id.run do
  let mut result : List Char := []
  let mut j := i
  while j < s.length do
    let c := charAt s j
    if c == '"' then return (String.ofList result.reverse, j + 1)
    else if c == '\\' && j + 1 < s.length then
      let nc := charAt s (j + 1)
      let escaped := if nc == 'n' then '\n' else if nc == 't' then '\t' else if nc == 'r' then '\r' else nc
      result := escaped :: result
      j := j + 2
    else
      result := c :: result
      j := j + 1
  (String.ofList result.reverse, j)

-- | Parse a JSON number starting at position i
private def parseNum (s : String) (i : Nat) : Int × Nat := Id.run do
  let neg := i < s.length && charAt s i == '-'
  let mut j := if neg then i + 1 else i
  let mut n : Nat := 0
  while j < s.length do
    let c := charAt s j
    if c.isDigit then n := n * 10 + (c.toNat - '0'.toNat); j := j + 1
    else break
  let v : Int := if neg then -n else n
  (v, j)

-- | Parse a JSON value
private partial def parseVal (s : String) (i : Nat) : JVal × Nat :=
  let i := skipWS s i
  if i >= s.length then (.null, i)
  else match charAt s i with
  | '"' => let (str, j) := parseStr s (i + 1); (.str str, j)
  | 't' => (.bool true, i + 4)
  | 'f' => (.bool false, i + 5)
  | 'n' => (.null, i + 4)
  | '[' => parseArr s (skipWS s (i + 1)) #[]
  | '{' => parseObj s (skipWS s (i + 1)) #[]
  | _ => let (n, j) := parseNum s i; (.num n, j)
where
  parseArr (s : String) (j : Nat) (vs : Array JVal) : JVal × Nat :=
    if j >= s.length then (.arr vs, j)
    else if charAt s j == ']' then (.arr vs, j + 1)
    else
      let (v, j') := parseVal s j
      let j'' := skipWS s j'
      let j''' := if charAt s j'' == ',' then skipWS s (j'' + 1) else j''
      parseArr s j''' (vs.push v)
  parseObj (s : String) (j : Nat) (kvs : Array (String × JVal)) : JVal × Nat :=
    if j >= s.length then (.obj kvs, j)
    else if charAt s j == '}' then (.obj kvs, j + 1)
    else
      let j' := skipWS s j
      if charAt s j' != '"' then (.obj kvs, j')
      else
        let (key, j'') := parseStr s (j' + 1)
        let j''' := skipWS s j''
        let j'''' := if charAt s j''' == ':' then j''' + 1 else j'''
        let (val, j5) := parseVal s j''''
        let j6 := skipWS s j5
        let j7 := if charAt s j6 == ',' then skipWS s (j6 + 1) else j6
        parseObj s j7 (kvs.push (key, val))

-- | Lookup key in JSON object
private def JVal.get? (v : JVal) (key : String) : Option JVal :=
  match v with
  | .obj kvs => kvs.findSome? fun (k, v) => if k == key then some v else none
  | _ => none

private def JVal.asStr : JVal → String | .str s => s | _ => ""
private def JVal.asInt : JVal → Int | .num n => n | _ => 0
private def JVal.asArr : JVal → Array JVal | .arr vs => vs | _ => #[]

-- | Parse Agg from short name
private def parseAgg : String → Agg
  | "count" => .count | "sum" => .sum | "avg" => .avg
  | "min" => .min | "max" => .max | "stddev" => .stddev | "dist" => .dist
  | _ => .count

-- | Parse Op from JVal
private def parseOp (v : JVal) : Option Op := do
  let typ := (← v.get? "type").asStr
  match typ with
  | "filter" => some (.filter (← v.get? "expr").asStr)
  | "sort" =>
    let cols := (← v.get? "cols").asArr.filterMap fun pair =>
      match pair with
      | .arr #[.str c, .bool asc] => some (c, asc)
      | _ => none
    some (.sort cols)
  | "sel" =>
    let cols := (← v.get? "cols").asArr.map JVal.asStr
    some (.sel cols)
  | "derive" =>
    let bs := (← v.get? "bindings").asArr.filterMap fun pair =>
      match pair with
      | .arr #[.str n, .str e] => some (n, e)
      | _ => none
    some (.derive bs)
  | "group" =>
    let keys := (← v.get? "keys").asArr.map JVal.asStr
    let aggs := (← v.get? "aggs").asArr.filterMap fun triple =>
      match triple with
      | .arr #[.str fn, .str name, .str col] => some (parseAgg fn, name, col)
      | _ => none
    some (.group keys aggs)
  | "take" => some (.take (← v.get? "n").asInt.toNat)
  | _ => none

-- | Parse ViewKind from JVal
private def parseVkind (v : JVal) : ViewKind :=
  match (v.get? "kind" |>.map JVal.asStr |>.getD "") with
  | "freqV" =>
    let cols := (v.get? "cols" |>.map JVal.asArr |>.getD #[]).map JVal.asStr
    let total := (v.get? "total" |>.map JVal.asInt |>.getD 0).toNat
    .freqV cols total
  | "colMeta" => .colMeta
  | "fld" =>
    let path := (v.get? "path" |>.map JVal.asStr |>.getD ".")
    let depth := (v.get? "depth" |>.map JVal.asInt |>.getD 1).toNat
    .fld path depth
  | _ => .tbl

-- | Restore a single view from JSON, re-executing the query pipeline.
--   Errors are caught per-view so partial restoration works.
private def restoreView (v : JVal) : IO (Option (View AdbcTable)) := do
  let path := (v.get? "path" |>.map JVal.asStr |>.getD "")
  if path.isEmpty then return none
  let vkind := parseVkind (v.get? "vkind" |>.getD .null)
  let disp := (v.get? "disp" |>.map JVal.asStr |>.getD "")
  let precAdj := (v.get? "precAdj" |>.map JVal.asInt |>.getD 0)
  let widthAdj := (v.get? "widthAdj" |>.map JVal.asInt |>.getD 0)
  let row := (v.get? "row" |>.map JVal.asInt |>.getD 0).toNat
  let col := (v.get? "col" |>.map JVal.asInt |>.getD 0).toNat
  let grp := (v.get? "grp" |>.map JVal.asArr |>.getD #[]).map JVal.asStr
  let hidden := (v.get? "hidden" |>.map JVal.asArr |>.getD #[]).map JVal.asStr
  let colSels := (v.get? "colSels" |>.map JVal.asArr |>.getD #[]).map JVal.asStr
  let search := do
    let s ← v.get? "search"
    match s with
    | .null => none
    | _ => some ((← s.get? "col").asInt.toNat, (← s.get? "val").asStr)
  -- Parse query
  let qObj := v.get? "query" |>.getD .null
  let base := (qObj.get? "base" |>.map JVal.asStr |>.getD s!"from `{path}`")
  let ops := (qObj.get? "ops" |>.map JVal.asArr |>.getD #[]).filterMap parseOp
  let query : Prql.Query := { base, ops }
  -- Re-execute: folder views use Folder.mkView, table views replay query
  let tbl? ← try
    match vkind with
    | .fld p depth => do
      match ← Folder.mkView p depth with
      | some fv => pure (some fv.nav.tbl)
      | none => pure none
    | _ => do
      let total ← AdbcTable.queryCount query
      AdbcTable.requery query total
  catch e =>
    Log.write "session" s!"skip view: {path} ({e})"
    pure none
  match tbl? with
  | none =>
    Log.write "session" s!"skip view: {path} (query failed)"
    pure none
  | some tbl =>
    let nRows := TblOps.nRows tbl
    let nCols := (TblOps.colNames tbl).size
    if nRows == 0 || nCols == 0 then return none
    let row' := min row (nRows - 1)
    let col' := min col (nCols - 1)
    match View.fromTbl tbl path col' grp row' with
    | some view => pure (some { view with
        vkind, disp, precAdj, widthAdj, search
        nav := { view.nav with hidden, col := { view.nav.col with sels := colSels } } })
    | none => pure none

/-! ## Public API -/

-- | Save current view stack to a named session file
def save (stk : ViewStack AdbcTable) (name : String) : IO Unit := do
  let path ← sessPath name
  IO.FS.writeFile path (stackToJson stk)
  Log.write "session" s!"saved {(stk.hd :: stk.tl).length} view(s) to {path}"
  statusMsg s!"session saved: {name}"

-- | Load a session by name, returns restored ViewStack
def load (name : String) : IO (Option (ViewStack AdbcTable)) := do
  let path ← sessPath name
  let content ← try IO.FS.readFile path catch _ => return none
  let (json, _) := parseVal content 0
  let views := (json.get? "views" |>.map JVal.asArr |>.getD #[])
  if views.isEmpty then return none
  -- Restore views in order (first = current, rest = parents)
  let mut restored : Array (View AdbcTable) := #[]
  for v in views do
    match ← restoreView v with
    | some view => restored := restored.push view
    | none => pure ()
  if h : restored.size > 0 then
    pure (some ⟨restored[0], (restored.extract 1 restored.size).toList⟩)
  else pure none

-- | List saved session names
def list : IO (Array String) := do
  let dir ← sessDir
  let mut names : Array String := #[]
  for entry in ← System.FilePath.readDir dir do
    let name := entry.fileName
    if name.endsWith ".json" then
      names := names.push ((name.take (name.length - 5)).toString)
  pure names

-- | Prompt user to pick a session name for saving (with fzf showing existing sessions)
def pickSaveName : IO (Option String) := do
  let existing ← list
  let input := "\n".intercalate existing.toList
  -- --print-query lets user type a new name or select existing
  Fzf.fzf #["--prompt=session name: ", "--print-query"] input |>.map (·.map fun s =>
    let lines := s.splitOn "\n" |>.filter (!·.isEmpty)
    lines.getLast?.getD (lines.headD ""))

-- | Prompt user to pick a session to load
def pickLoadName : IO (Option String) := do
  let existing ← list
  if existing.isEmpty then statusMsg "no saved sessions"; return none
  Fzf.fzf #["--prompt=load session: "] ("\n".intercalate existing.toList)
    |>.map (·.map String.trimAscii |>.map (·.toString))

end Tc.Session
