/-
  Command system: Verb + Obj pattern for nav/toggle, ArgCmd for argument commands.
  - Verb: movement, toggle, del, sort
  - Cmd: Obj + Verb (row, col, rowSel, colSel, grp) or .arg ArgCmd
  - ArgCmd: prefix char + payload, bypasses fzf (socket/demo/test)
-/

-- | Parse typeclass (inverse of ToString)
class Parse (α : Type) where
  parse? : String → Option α

-- | Verb: action type
inductive Verb where
  | inc | dec         -- >/< movement (colSel:[/], rowSel:/\, grp:n/N)
  | dup               -- copy/dup (info: select single-val cols)
  | del               -- delete
  | ent               -- toggle/enter (col:s, rowSel:T, grp:!)
  | up                -- go up/parent (fld: backspace → parent dir)
  | val (n : UInt8)   -- direct value selection (heat mode 0-3, etc.)
  deriving Repr, BEq, DecidableEq

namespace Verb

-- | Verb to char (<> for inc/dec, digits for val)
def toChar : Verb → Char
  | .inc => '>' | .dec => '<' | .ent => '~' | .del => 'd' | .dup => 'c' | .up => '^'
  | .val n => Char.ofNat (n.toNat + '0'.toNat)

-- | Char to verb (<> and +- both accepted for inc/dec)
def ofChar? : Char → Option Verb
  | '>' | '+' => some .inc | '<' | '-' => some .dec | '~' => some .ent
  | 'd' => some .del | 'c' => some .dup | '^' => some .up
  | c => if c.isDigit then some (.val (c.toNat - '0'.toNat).toUInt8) else none

instance : ToString Verb where toString v := v.toChar.toString
instance : Parse Verb where parse? s := s.toList.head?.bind ofChar?

end Verb

-- | Argument commands: prefix char + payload (bypass fzf, fully programmable via socket)
-- Each variant corresponds to a key that normally opens fzf for user input.
inductive ArgCmd where
  | split (arg : String)     -- :  split column by delimiter
  | derive (arg : String)    -- =  derive column (name = expr)
  | filter (arg : String)    -- \  filter rows by PRQL expression
  | search (arg : String)    -- /  search for value in current column
  | colJump (arg : String)   -- s  jump to column by name
  | export (arg : String)    -- e  export (csv/parquet/json/ndjson)
  | sessSave (arg : String)  -- W  save session with name
  | sessLoad (arg : String)  -- L  load session by name
  | join (arg : String)      -- J  join type by index (0=inner,1=left,2=right,3=union,4=diff)
  deriving Repr, BEq, DecidableEq

namespace ArgCmd

-- | Single source of truth: prefix char for each argument command
def pfx : ArgCmd → Char
  | .split _ => ':' | .derive _ => '=' | .filter _ => '\\' | .search _ => '/'
  | .colJump _ => 's' | .export _ => 'e'
  | .sessSave _ => 'W' | .sessLoad _ => 'L' | .join _ => 'J'

-- | Construct from prefix char + argument string
def ofPfx? (c : Char) (arg : String) : Option ArgCmd :=
  match c with
  | ':' => some (.split arg) | '=' => some (.derive arg)
  | '\\' => some (.filter arg) | '/' => some (.search arg)
  | 's' => some (.colJump arg) | 'e' => some (.export arg)
  | 'W' => some (.sessSave arg) | 'L' => some (.sessLoad arg)
  | 'J' => some (.join arg) | _ => none

-- | Check if char is an ArgCmd prefix (derived from ofPfx?, single source of truth)
def isPfx (c : Char) : Bool := (ofPfx? c "").isSome

instance : ToString ArgCmd where toString ac := s!"{ac.pfx}{match ac with
  | .split a | .derive a | .filter a | .search a
  | .colJump a | .export a | .sessSave a | .sessLoad a | .join a => a}"

end ArgCmd

-- | Command: Obj + Verb pattern
inductive Cmd where
  | row (v : Verb)     -- row inc/dec (single step)
  | col (v : Verb)     -- col </>/c=fzf cmd menu
  | hPage (v : Verb)   -- hPage -=prev, +=next page (column)
  | vPage (v : Verb)   -- vPage -=prev, +=next page (row)
  | hor (v : Verb)     -- hor -=home, +=end (column)
  | ver (v : Verb)     -- ver -=top, +=bottom (row)

  | rowSel (v : Verb)  -- rowSel +=search(/), -=filter(\), ~=toggle(T)
  | colSel (v : Verb)  -- colSel +=sortAsc([), -=sortDesc(]), ~=toggle(t)
  | grp (v : Verb)     -- grp +=next(n), -=prev(N), ~=toggle(!)

  | stk (v : Verb)     -- stk <pop/~swap/c=dup/d=quit/^=xpose/0=diff

  | prec (v : Verb)    -- prec -=dec, +=inc precision
  | width (v : Verb)   -- width -=dec, +=inc width
  | thm (v : Verb)     -- thm -=prev, +=next theme
  | info (v : Verb)    -- info +=show, -=hide, ~=toggle
  | metaV (v : Verb)   -- metaV c=push, 0=selNull, 1=selSingle, ~=enter
  | freq (v : Verb)    -- freq c=push, ~=filter
  | fld (v : Verb)     -- fld c=push, +/-=depth, ~=enter dir/file
  | plot (v : Verb)    -- plot: line/bar/scatter/hist/box via PlotKind
  | colShift (v : Verb) -- colShift +=right, -=left (reorder key columns)
  | heat (v : Verb)     -- heat +=more color, -=less color (mode 0-3)
  | yank (v : Verb)    -- yank ~=cell, >=row, <=col
  | prev (v : Verb)    -- prev >=scroll down, <=scroll up ({/} keys)
  | arg (ac : ArgCmd)  -- argument commands (prefix + payload, bypass fzf)
  deriving Repr, BEq, DecidableEq

namespace Cmd

-- | Obj chars
private def objs : Array (Char × (Verb → Cmd)) := #[
  ('r', .row), ('c', .col), ('R', .rowSel), ('C', .colSel), ('g', .grp), ('s', .stk),
  ('h', .hPage), ('v', .vPage), ('H', .hor), ('V', .ver), ('p', .prec), ('w', .width),
  ('T', .thm), ('i', .info), ('M', .metaV), ('F', .freq), ('D', .fld),
  ('P', .plot), ('K', .colShift), ('m', .heat),
  ('y', .yank), ('B', .prev)
]

-- | Get obj char for Cmd
private def objChar : Cmd → Char
  | .row _ => 'r' | .col _ => 'c' | .rowSel _ => 'R' | .colSel _ => 'C'
  | .grp _ => 'g' | .stk _ => 's'
  | .hPage _ => 'h' | .vPage _ => 'v' | .hor _ => 'H' | .ver _ => 'V'
  | .prec _ => 'p' | .width _ => 'w' | .thm _ => 'T' | .info _ => 'i'
  | .metaV _ => 'M' | .freq _ => 'F' | .fld _ => 'D' | .plot _ => 'P'
  | .colShift _ => 'K' | .heat _ => 'm' | .yank _ => 'y' | .prev _ => 'B'
  | .arg ac => ac.pfx

-- | Get verb from Cmd
private def verb : Cmd → Verb
  | .row v | .col v | .rowSel v | .colSel v | .grp v | .stk v => v
  | .hor v | .ver v | .hPage v | .vPage v | .prec v | .width v => v
  | .thm v | .info v | .metaV v | .freq v | .fld v | .plot v | .colShift v | .heat v | .yank v | .prev v => v
  | .arg _ => .ent

instance : ToString Cmd where toString
  | .arg ac => toString ac
  | c => s!"{c.objChar}{c.verb.toChar}"

instance : Parse Cmd where
  parse? s := do
    -- 2-char obj+verb commands first (e.g. "r+", "s~", "m~") — prevents
    -- ArgCmd prefix collision (e.g. "s~" must be stk.ent, not colJump "~")
    if let [o, vc] := s.toList then
      if let some v := Verb.ofChar? vc then
        if let some (_, mk) := objs.find? (·.1 == o) then
          return mk v
    -- Argument commands: prefix char + argument (e.g. ":-", "=double = x * 2", "ecsv")
    if s.length > 1 then
      let pfx := s.front
      let a := (s.drop 1).toString
      if let some ac := ArgCmd.ofPfx? pfx a then return .arg ac
    none


end Cmd

-- | Effect sub-types (grouped by domain)
inductive FzfEffect where | cmd | col | row | filter deriving Repr, BEq
inductive QueryEffect where
  | colMeta | freq (colNames : Array String)
  | freqFilter (cols : Array String) (row : Nat)
  | filter (expr : String)
  | sort (colIdx : Nat) (sels : Array Nat) (grp : Array Nat) (asc : Bool)
  deriving Repr, BEq
inductive FolderEffect where | push | enter | del | parent | depth (delta : Int) deriving Repr, BEq
inductive SearchEffect where | next | prev deriving Repr, BEq
inductive PlotKind where | line | bar | scatter | hist | box | area | density | step | violin deriving Repr, BEq

instance : ToString PlotKind where
  toString | .line => "line" | .bar => "bar" | .scatter => "scatter" | .hist => "hist" | .box => "box"
           | .area => "area" | .density => "density" | .step => "step" | .violin => "violin"
inductive MetaEffect where | selNull | selSingle | setKey deriving Repr, BEq
inductive ExportFmt where | csv | parquet | json | ndjson deriving Repr, BEq
inductive ClipEffect where | cell | row | col deriving Repr, BEq

-- | Effect: describes an IO operation to perform (Runner interprets)
inductive Effect where
  | none | quit
  | fzf : FzfEffect → Effect
  | query : QueryEffect → Effect
  | folder : FolderEffect → Effect
  | search : SearchEffect → Effect
  | plot : PlotKind → Effect
  | colMeta : MetaEffect → Effect
  | clip : ClipEffect → Effect
  | themeLoad (delta : Int)
  | fetchMore
  | export : ExportFmt → Effect
  | sessionSave
  | sessionLoad
  | join
  | transpose
  | diff
  deriving Repr, BEq

namespace Effect
def isNone : Effect → Bool | .none => true | _ => false
end Effect

-- | Update typeclass: pure state transition returning Effect
class Update (α : Type) where
  update : α → Cmd → Option (α × Effect)
