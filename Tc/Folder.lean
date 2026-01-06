/-
  Folder: directory browser with configurable find depth
  Commands: .fld .dup (push), .fld .inc/.dec (depth), .view .ent (enter)
-/
import Tc.Data.Mem.Text
import Tc.View
import Tc.Term

namespace Tc.Folder

-- | List directory with find command, returns tab-separated output
-- Cols: type (d/f/-), size, date, path
def listDir (path : String) (depth : Nat) : IO String := do
  let p := if path.isEmpty then "." else path
  let out ← IO.Process.output {
    cmd := "find"
    args := #[p, "-maxdepth", toString depth, "-printf", "%y\t%s\t%T+\t%p\n"]
  }
  -- add header and filter out first line (the path itself)
  let lines := out.stdout.splitOn "\n" |>.filter (·.length > 0)
  let hdr := "type\tsize\tdate\tpath"
  let body := lines.drop 1  -- skip the directory itself
  pure (hdr ++ "\n" ++ "\n".intercalate body)

-- | Parse find output to MemTable (tab-separated)
def toMemTable (output : String) : Except String MemTable := MemTable.fromTsv output

-- | View file with bat (if available) or less
def viewFile (path : String) : IO Unit := do
  Term.shutdown  -- restore terminal before spawning pager
  let hasBat ← IO.Process.output { cmd := "which", args := #["bat"] }
  if hasBat.exitCode == 0 then
    let _ ← IO.Process.spawn { cmd := "bat", args := #["--paging=always", path], stdin := .inherit, stdout := .inherit, stderr := .inherit } >>= (·.wait)
  else
    let _ ← IO.Process.spawn { cmd := "less", args := #[path], stdin := .inherit, stdout := .inherit, stderr := .inherit } >>= (·.wait)
  let _ ← Term.init  -- reinit terminal after pager

-- | Get path column value from current row
def curPath (v : View) : Option String := do
  guard (v.vkind matches .fld _ _)
  let tbl ← v.nav.tbl.asMem?
  let pathCol ← tbl.names.idxOf? "path"
  let row := v.nav.row.cur.val
  pure ((tbl.cols.getD pathCol default).get row).toRaw

-- | Get type column value from current row (d=dir, f=file, l=link)
def curType (v : View) : Option Char := do
  guard (v.vkind matches .fld _ _)
  let tbl ← v.nav.tbl.asMem?
  let typeCol ← tbl.names.idxOf? "type"
  let row := v.nav.row.cur.val
  let t := ((tbl.cols.getD typeCol default).get row).toRaw
  t.toList.head?

-- | Create folder view from path with given depth
def mkView (path : String) (depth : Nat) : IO (Option View) := do
  let output ← listDir path depth
  match toMemTable output with
  | .error _ => pure none
  | .ok tbl =>
    let disp := path.splitOn "/" |>.getLast? |>.getD path
    pure <| View.fromTbl (.mem tbl) path |>.map fun v =>
      { v with vkind := .fld path depth, disp := disp }

-- | Push new folder view onto stack (use current path or ".")
def push (s : ViewStack) : IO (Option ViewStack) := do
  let path := match curPath s.cur with
    | some p => p
    | none => match s.cur.vkind with
      | .fld p _ => p  -- already a folder view, use its path
      | _ => "."       -- default to current directory
  match ← mkView path 1 with
  | some v => pure (some (s.push v))
  | none => pure none

-- | Enter directory or view file based on current row
def enter (s : ViewStack) : IO (Option ViewStack) := do
  match curType s.cur, curPath s.cur with
  | some 'd', some p =>  -- directory: push new folder view
    let depth := match s.cur.vkind with | .fld _ d => d | _ => 1
    match ← mkView p depth with
    | some v => pure (some (s.push v))
    | none => pure none
  | some 'f', some p =>  -- regular file: view with bat/less
    viewFile p
    pure (some s)  -- return unchanged stack after viewing
  | some 'l', some p =>  -- symlink: check if dir or file
    let stat ← IO.Process.output { cmd := "test", args := #["-d", p] }
    if stat.exitCode == 0 then  -- is directory
      let depth := match s.cur.vkind with | .fld _ d => d | _ => 1
      match ← mkView p depth with
      | some v => pure (some (s.push v))
      | none => pure none
    else  -- treat as file
      viewFile p
      pure (some s)
  | _, _ => pure none

-- | Adjust find depth (+1 or -1, min 1)
def setDepth (s : ViewStack) (delta : Int) : IO (Option ViewStack) := do
  match s.cur.vkind with
  | .fld path depth =>
    let newDepth := max 1 ((depth : Int) + delta).toNat
    if newDepth == depth then pure (some s)  -- no change
    else match ← mkView path newDepth with
      | some v =>
        -- preserve row position if possible
        let row := min s.cur.nav.row.cur.val (v.nRows - 1)
        let v' := View.fromTbl v.nav.tbl path (row := row) |>.map fun x =>
          { x with vkind := .fld path newDepth, disp := s.cur.disp }
        pure (v'.map s.setCur)
      | none => pure (some s)  -- keep current if refresh fails
  | _ => pure none

-- | Execute folder commands
def exec (s : ViewStack) (cmd : Cmd) : IO (Option ViewStack) :=
  match cmd with
  | .fld .dup => push s               -- D: push folder view
  | .fld .inc => setDepth s 1         -- +d: increase depth
  | .fld .dec => setDepth s (-1)      -- -d: decrease depth
  | .view .ent =>                     -- Enter: only for fld views
    if s.cur.vkind matches .fld _ _ then enter s else pure none
  | _ => pure none

end Tc.Folder
