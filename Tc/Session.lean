/-
  Session: save/load view stack state to JSON files in ~/.cache/tv/sessions/.
  Serializes Prql.Query pipeline + view metadata; restores by re-executing queries.
  JSON parser assumes ASCII keys/values (session data is always ASCII).
-/
import Tc.View
import Tc.Fzf
import Tc.Data.ADBC.Ops
import Tc.Folder

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

/-! ## Serialization: View → JSON -/

-- | Escape string for JSON (quotes, backslashes, control chars)
private def jsonStr (s : String) : String :=
  let s := (s.replace "\\" "\\\\").replace "\"" "\\\""
  let s := (s.replace "\n" "\\n").replace "\r" "\\r"
  let s := s.replace "\t" "\\t"
  "\"" ++ s ++ "\""

-- | JSON key:value pair
private def kv (k : String) (v : String) : String := s!"\"{k}\":{v}"

-- | JSON array from string list
private def jsonArr (xs : List String) : String := s!"[{",".intercalate xs}]"

def opToJson : Op → String
  | .filter e => s!"\{{kv "type" (jsonStr "filter")},{kv "expr" (jsonStr e)}}"
  | .sort cols =>
    let cs := cols.map fun (c, asc) => s!"[{jsonStr c},{asc}]"
    s!"\{{kv "type" (jsonStr "sort")},{kv "cols" (jsonArr cs.toList)}}"
  | .sel cols => s!"\{{kv "type" (jsonStr "sel")},{kv "cols" (jsonArr (cols.map jsonStr).toList)}}"
  | .exclude cols => s!"\{{kv "type" (jsonStr "exclude")},{kv "cols" (jsonArr (cols.map jsonStr).toList)}}"
  | .derive bs =>
    let ds := bs.map fun (n, e) => s!"[{jsonStr n},{jsonStr e}]"
    s!"\{{kv "type" (jsonStr "derive")},{kv "bindings" (jsonArr ds.toList)}}"
  | .group keys aggs =>
    let ks := keys.map jsonStr
    let as := aggs.map fun (fn, name, col) => s!"[{jsonStr fn.short},{jsonStr name},{jsonStr col}]"
    s!"\{{kv "type" (jsonStr "group")},{kv "keys" (jsonArr ks.toList)},{kv "aggs" (jsonArr as.toList)}}"
  | .take n => s!"\{{kv "type" (jsonStr "take")},{kv "n" s!"{n}"}}"

def vkindToJson : ViewKind → String
  | .tbl => s!"\{{kv "kind" (jsonStr "tbl")}}"
  | .freqV cols total =>
    s!"\{{kv "kind" (jsonStr "freqV")},{kv "cols" (jsonArr (cols.map jsonStr).toList)},{kv "total" s!"{total}"}}"
  | .colMeta => s!"\{{kv "kind" (jsonStr "colMeta")}}"
  | .fld path depth => s!"\{{kv "kind" (jsonStr "fld")},{kv "path" (jsonStr path)},{kv "depth" s!"{depth}"}}"

private def viewToJson (v : View AdbcTable) : String :=
  let q := v.nav.tbl.query
  let search := match v.search with
    | some (i, s) => s!"\{{kv "col" s!"{i}"},{kv "val" (jsonStr s)}}"
    | none => "null"
  let fields := [
    kv "path" (jsonStr v.path), kv "vkind" (vkindToJson v.vkind),
    kv "disp" (jsonStr v.disp), kv "precAdj" s!"{v.precAdj}", kv "widthAdj" s!"{v.widthAdj}",
    kv "row" s!"{v.nav.row.cur.val}", kv "col" s!"{v.nav.col.cur.val}",
    kv "grp" (jsonArr (v.nav.grp.map jsonStr).toList),
    kv "hidden" (jsonArr (v.nav.hidden.map jsonStr).toList),
    kv "colSels" (jsonArr (v.nav.col.sels.map jsonStr).toList),
    kv "search" search,
    kv "query" s!"\{{kv "base" (jsonStr q.base)},{kv "ops" (jsonArr (q.ops.map opToJson).toList)}}"
  ]
  s!"\{{",".intercalate fields}}"

private def stackToJson (s : ViewStack AdbcTable) : String :=
  let views := (s.hd :: s.tl).map viewToJson
  s!"\{{kv "version" "1"},{kv "views" (jsonArr views)}}"

/-! ## Deserialization: JSON → View state -/

private inductive JVal where
  | str (v : String) | num (v : Int) | bool (v : Bool) | null
  | arr (vs : Array JVal) | obj (kvs : Array (String × JVal))
  deriving Inhabited

-- | Get char at byte position (ASCII-only session data)
private def charAt (s : String) (i : Nat) : Char :=
  if i < s.length then String.Pos.Raw.get s ⟨i⟩ else '\x00'

private partial def skipWS (s : String) (i : Nat) : Nat :=
  if i < s.length then
    let c := charAt s i
    if c == ' ' || c == '\n' || c == '\r' || c == '\t' then skipWS s (i + 1) else i
  else i

private partial def parseStr (s : String) (i : Nat) : String × Nat := Id.run do
  let mut result : List Char := []
  let mut j := i
  while j < s.length do
    let c := charAt s j
    if c == '"' then return (String.ofList result.reverse, j + 1)
    else if c == '\\' && j + 1 < s.length then
      let nc := charAt s (j + 1)
      let escaped := if nc == 'n' then '\n' else if nc == 't' then '\t' else if nc == 'r' then '\r' else nc
      result := escaped :: result; j := j + 2
    else result := c :: result; j := j + 1
  (String.ofList result.reverse, j)

private def parseNum (s : String) (i : Nat) : Int × Nat := Id.run do
  let neg := i < s.length && charAt s i == '-'
  let mut j := if neg then i + 1 else i
  let mut n : Nat := 0
  while j < s.length do
    let c := charAt s j
    if c.isDigit then n := n * 10 + (c.toNat - '0'.toNat); j := j + 1 else break
  (if neg then (-(n : Int), j) else (n, j))

partial def parseVal (s : String) (i : Nat) : JVal × Nat :=
  let i := skipWS s i
  if i >= s.length then (.null, i)
  else match charAt s i with
  | '"' => let (str, j) := parseStr s (i + 1); (.str str, j)
  | 't' => (.bool true, i + 4) | 'f' => (.bool false, i + 5) | 'n' => (.null, i + 4)
  | '[' => parseArr s (skipWS s (i + 1)) #[]
  | '{' => parseObj s (skipWS s (i + 1)) #[]
  | _ => let (n, j) := parseNum s i; (.num n, j)
where
  parseArr (s : String) (j : Nat) (vs : Array JVal) : JVal × Nat :=
    if j >= s.length || charAt s j == ']' then (.arr vs, j + 1)
    else
      let (v, j') := parseVal s j
      let j'' := skipWS s j'
      let j''' := if charAt s j'' == ',' then skipWS s (j'' + 1) else j''
      parseArr s j''' (vs.push v)
  parseObj (s : String) (j : Nat) (kvs : Array (String × JVal)) : JVal × Nat :=
    if j >= s.length || charAt s j == '}' then (.obj kvs, j + 1)
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

namespace JVal
def get? (v : JVal) (key : String) : Option JVal :=
  match v with | .obj kvs => kvs.findSome? fun (k, v) => if k == key then some v else none | _ => none
def asStr : JVal → String | .str s => s | _ => ""
def asInt : JVal → Int | .num n => n | _ => 0
def asArr : JVal → Array JVal | .arr vs => vs | _ => #[]
-- | Typed accessors with defaults
def strD (v : JVal) (key : String) (dflt := "") : String := (v.get? key |>.map asStr).getD dflt
def intD (v : JVal) (key : String) (dflt := 0) : Int := (v.get? key |>.map asInt).getD dflt
def arrD (v : JVal) (key : String) : Array JVal := (v.get? key |>.map asArr).getD #[]
end JVal

private def parseAgg : String → Agg
  | "count" => .count | "sum" => .sum | "avg" => .avg
  | "min" => .min | "max" => .max | "stddev" => .stddev | "dist" => .dist | _ => .count

def parseOp (v : JVal) : Option Op := do
  match (← v.get? "type").asStr with
  | "filter" => some (.filter (← v.get? "expr").asStr)
  | "sort" =>
    let cols := v.arrD "cols" |>.filterMap fun | .arr #[.str c, .bool asc] => some (c, asc) | _ => none
    some (.sort cols)
  | "sel" => some (.sel (v.arrD "cols" |>.map JVal.asStr))
  | "exclude" => some (.exclude (v.arrD "cols" |>.map JVal.asStr))
  | "derive" =>
    let bs := v.arrD "bindings" |>.filterMap fun | .arr #[.str n, .str e] => some (n, e) | _ => none
    some (.derive bs)
  | "group" =>
    let keys := v.arrD "keys" |>.map JVal.asStr
    let aggs := v.arrD "aggs" |>.filterMap fun
      | .arr #[.str fn, .str name, .str col] => some (parseAgg fn, name, col) | _ => none
    some (.group keys aggs)
  | "take" => some (.take (← v.get? "n").asInt.toNat)
  | _ => none

def parseVkind (v : JVal) : ViewKind :=
  match v.strD "kind" with
  | "freqV" => .freqV (v.arrD "cols" |>.map JVal.asStr) (v.intD "total").toNat
  | "colMeta" => .colMeta
  | "fld" => .fld (v.strD "path" ".") (v.intD "depth" 1).toNat
  | _ => .tbl

-- | Restore a single view from JSON, re-executing the query pipeline.
--   Errors are caught per-view so partial restoration works.
private def restoreView (v : JVal) : IO (Option (View AdbcTable)) := do
  let path := v.strD "path"
  if path.isEmpty then return none
  let vkind := parseVkind (v.get? "vkind" |>.getD .null)
  let disp := v.strD "disp"
  let precAdj := v.intD "precAdj"
  let widthAdj := v.intD "widthAdj"
  let row := (v.intD "row").toNat
  let col := (v.intD "col").toNat
  let grp := v.arrD "grp" |>.map JVal.asStr
  let hidden := v.arrD "hidden" |>.map JVal.asStr
  let colSels := v.arrD "colSels" |>.map JVal.asStr
  let search := do
    let s ← v.get? "search"
    match s with | .null => none | _ => some (s.intD "col" |>.toNat, s.strD "val")
  let qObj := v.get? "query" |>.getD .null
  let base := qObj.strD "base" s!"from `{path}`"
  let ops := qObj.arrD "ops" |>.filterMap parseOp
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
  let (json, _) := parseVal content 0
  let views := json.arrD "views"
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
