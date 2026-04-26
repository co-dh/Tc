import Lean

/-!
# Minimal mermaid edge parser

A *very* small parser, just enough to extract edge data from the
mermaid blocks in `migration.md`.

A "mermaid block" here means a fenced code block whose first
non-fence line is the comment `%% id: <NAME>`.  Inside the block we
recognise only edge lines of the form

```
  <src> -- <label> --> <tgt>
```

Everything else (the `flowchart LR` header, blank lines, comments,
fancier mermaid syntax) is ignored.

The output is a `FinGraphPres`: a list of distinct object names (in
order of first appearance) plus a list of `(src_idx, tgt_idx, label)`
edge triples.
-/

namespace Mermaid

/-- A parsed mermaid edge: a source node, a label, a target node. -/
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
the next ``` mermaid fence, the next `%% id:` marker, or the closing
`*/` of a typst block comment (whichever comes first — the second
case lets multiple sections share one block, and the third lets us
host the same data inside a typst comment).  Recursion is structural
on the line list. -/
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

/-- Extract all edges from the mermaid block named `id` in `md`. -/
def parse (md : String) (id : String) : List Edge :=
  (collectBlock id (md.splitOn "\n") false []).filterMap Edge.parse?

end Mermaid

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
private def collectNodes : List Mermaid.Edge → List String → List String
  | [],         acc => acc
  | e :: rest,  acc => collectNodes rest (addNode (addNode acc e.src) e.tgt)

/-- Convert raw mermaid edges into a presentation. -/
def ofEdges (edges : List Mermaid.Edge) : FinGraphPres :=
  let nodes := collectNodes edges []
  let idx (n : String) : Nat := nodes.findIdx? (· == n) |>.getD 0
  { objects := nodes
    edges   := edges.map (fun e => (idx e.src, idx e.tgt, e.label)) }

/-- One-shot: read mermaid block `id` from markdown text `md`. -/
def fromMd (md : String) (id : String) : FinGraphPres :=
  ofEdges (Mermaid.parse md id)

end FinGraphPres

/-! ## `ToExpr` instance and `mermaid_pres!` term elab

The kernel can't reduce string-parsing during typechecking (most
`String` operations are `@[extern]`), so we read the markdown and run
the parser at **elaboration time** instead.  The result of the parse
is then injected back into the term as a literal `FinGraphPres` —
which the kernel sees as a constant and reduces freely. -/

open Lean

instance : ToExpr FinGraphPres where
  toTypeExpr := mkConst ``FinGraphPres
  toExpr p :=
    mkApp2 (mkConst ``FinGraphPres.mk) (toExpr p.objects) (toExpr p.edges)

namespace Mermaid

/-- `mermaid_pres! "<file>" "<id>"` reads the mermaid block named `<id>`
from the file at `<file>` (relative to the current working directory)
and elaborates to a literal `FinGraphPres`. -/
elab "mermaid_pres!" path:str id:str : term => do
  let content ← IO.FS.readFile path.getString
  let pres := FinGraphPres.fromMd content id.getString
  return Lean.toExpr pres

end Mermaid

