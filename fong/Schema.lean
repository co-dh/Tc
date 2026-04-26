import Lean

/-!
# Minimal schema-edge parser

A *very* small parser for the schema-data block at the top of
`migration.typ`.  A "schema block" here means a section opened by

```
  %% id: <NAME>
```

and closed by either the next `%% id:` marker or the closing `*/`
fence of the typst block comment that hosts it (the parser also
accepts the legacy ` ``` ` fence so the same logic still works on
mermaid-style blocks).  Inside, only edge lines of the form

```
  <src> -- <label> --> <tgt>
```

are read; everything else is ignored.

The output is a `FinGraphPres`: a list of distinct object names (in
order of first appearance) plus a list of `(src_idx, tgt_idx, label)`
edge triples.
-/

namespace Schema

/-- A parsed edge: a source node, a label, a target node. -/
structure Edge where
  src   : String
  label : String
  tgt   : String

/-- Local trim that returns a `String` (the new `String.trim` returns a
`Slice` since Lean 4.27). -/
@[inline] private def trim (s : String) : String := s.trimAscii.toString

namespace Edge

/-- Parse a single line.  Returns `none` for non-edge lines. -/
private def parse? (line : String) : Option Edge :=
  let line := trim line
  if line.isEmpty || line.startsWith "%%" then none
  else match line.splitOn "-->" with
    | [lhs, rhs] => match (trim lhs).splitOn " -- " with
        | [src, label] => some { src := trim src, label := trim label, tgt := trim rhs }
        | _ => none
    | _ => none

end Edge

/-- Walk a list of lines, collecting those between `%% id: <id>` and
the next `%% id:` marker, the closing `*/` of a typst block comment,
or a ` ``` ` fence (whichever comes first — the first case lets
multiple sections share one block, the second is the typst host, the
third is legacy mermaid).  Recursion is structural on the line list. -/
private def collectBlock (id : String) :
    List String → Bool → List String → List String
  | [],           _,    acc => acc.reverse
  | line :: rest, false, acc =>
      if trim line == s!"%% id: {id}" then collectBlock id rest true acc
      else collectBlock id rest false acc
  | line :: rest, true,  acc =>
      let t := trim line
      if t.startsWith "```" || t.startsWith "%% id:" || t == "*/"
      then acc.reverse
      else collectBlock id rest true (line :: acc)

/-- Extract all edges from the schema block named `id` in `src`. -/
def parse (src : String) (id : String) : List Edge :=
  (collectBlock id (src.splitOn "\n") false []).filterMap Edge.parse?

end Schema

/-! ## Finite-graph presentation

A schema's underlying multigraph as plain data: distinct object names
plus a list of (src-index, tgt-index, label) edge triples.  The
indices refer into `objects` (so `objects.length` is the object count). -/

structure FinGraphPres where
  objects : List String
  edges   : List (Nat × Nat × String)

namespace FinGraphPres

private def addNode (xs : List String) (x : String) : List String :=
  if xs.contains x then xs else xs ++ [x]

/-- Collect the distinct nodes from a list of edges, in order of first
appearance (sources before their targets within each edge). -/
private def collectNodes : List Schema.Edge → List String → List String
  | [],         acc => acc
  | e :: rest,  acc => collectNodes rest (addNode (addNode acc e.src) e.tgt)

/-- Convert raw schema edges into a presentation. -/
def ofEdges (edges : List Schema.Edge) : FinGraphPres :=
  let nodes := collectNodes edges []
  let idx (n : String) : Nat := nodes.findIdx? (· == n) |>.getD 0
  { objects := nodes
    edges   := edges.map (fun e => (idx e.src, idx e.tgt, e.label)) }

/-- One-shot: read schema block `id` from source text `src`. -/
def fromSrc (src : String) (id : String) : FinGraphPres :=
  ofEdges (Schema.parse src id)

end FinGraphPres

/-! ## `ToExpr` instance and `schema_pres!` term elab

The kernel can't reduce string-parsing during typechecking (most
`String` operations are `@[extern]`), so we read the file and run the
parser at **elaboration time** instead.  The result of the parse is
then injected back into the term as a literal `FinGraphPres` — which
the kernel sees as a constant and reduces freely. -/

open Lean

instance : ToExpr FinGraphPres where
  toTypeExpr := mkConst ``FinGraphPres
  toExpr p :=
    mkApp2 (mkConst ``FinGraphPres.mk) (toExpr p.objects) (toExpr p.edges)

namespace Schema

/-- `schema_pres! "<file>" "<id>"` reads the schema block named `<id>`
from the file at `<file>` (relative to the current working directory)
and elaborates to a literal `FinGraphPres`. -/
elab "schema_pres!" path:str id:str : term => do
  let content ← IO.FS.readFile path.getString
  let pres := FinGraphPres.fromSrc content id.getString
  return Lean.toExpr pres

end Schema
