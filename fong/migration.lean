import kan_extension
import Schema

/-!
# Schema migration via Kan extensions

A formalisation of the Fong–Spivak schema-migration triple from
*Seven Sketches in Compositionality* (Chapter 3), in Lean 4.

## Setup

A **schema** is a small category `Src`; an **instance** of the schema
is a functor `Src ⇒ Set`.  (The general Kan-extension story works for
any target with enough (co)limits, but Fong & Spivak — and this file —
fix the target to `Set`, so an instance carries a *set* of rows for
each object and a *function* for each arrow.)

Given a schema functor `F : Src ⇒ Tgt`, we obtain three functors
between the instance categories `[Src, Set]` and `[Tgt, Set]`:

```
   Σ_F : [Src, Set] ⇒ [Tgt, Set]    -- left  Kan extension along F
   Δ_F : [Tgt, Set] ⇒ [Src, Set]    -- precomposition with F
   Π_F : [Src, Set] ⇒ [Tgt, Set]    -- right Kan extension along F
```

They form an adjoint triple `Σ_F ⊣ Δ_F ⊣ Π_F`.  In databases this is
the **left-pushforward / pullback / right-pushforward** of data along a
schema migration.

## Design choice: schemas as data

In an inductive-type encoding each schema would be its own Lean
`inductive` declaration, fixed at compile time.  That's fine for
textbook exposition but not for a real tool: you want to read schemas,
and the migration between them, from a config file.

So we present every schema as a **directed multigraph**
(structure `Graph`) and recover its category by the **free-category**
construction: morphisms = paths under concatenation, identities =
empty paths.  A migration `F` is a **graph homomorphism**
(structure `GraphHom`) — an object map plus a per-edge path — which
extends uniquely to a functor by the universal property of the free
category.

The whole `Graph` / `GraphHom` data is just `Fin n` indices and lookup
tables, exactly the shape one would store in JSON or a relational table.

We illustrate with Fong's running example, the migration `Gr ⇒ DDS`
(graphs to discrete dynamical systems), and run §3.11's specific
7-state DDS through `Δ_F` to obtain its trajectory graph.

## Reading guide for the Lean-curious category theorist

A few Lean-isms you'll see:

* `structure Foo where field : T` declares a labelled product (record
  type).  `def x : Foo where field := …` constructs one.
* `inductive T : … where | C₁ : … | C₂ : …` declares a sum / GADT.
  Lean's `.Cᵢ` is dot-notation: it infers `T` from context.
* `∀ {a : T}, …` is a dependent function with `a` *implicit* — Lean
  fills it in by unification at the call site.
* `match x with | pat => e` is pattern matching; Lean checks
  exhaustiveness and reduces by ι-reduction.
* `def` and `theorem` both introduce a named term; the only difference
  is that `theorem`s have type in `Prop` (proof-irrelevant), so the
  body is treated as a proof rather than a value.
* `rfl : x = x` succeeds when both sides are *definitionally* equal
  (β / ι / δ reduce to the same normal form).
* `congrArg f h` lifts `h : x = y` to `f x = f y`.
* `by tac₁; tac₂` runs tactics; `show t` restates the goal as `t`;
  `rw [eq]` rewrites along an equality.
* The infix notation `⟶`, `≫`, `⇒`, `⋙`, `⇛` for Hom, composition,
  functor, functor-composition, natural transformation comes from
  `cat.lean`.  `Cat` there is a minimal hand-rolled small-category
  structure (Mathlib has the full version).
-/

/-! ## 1. The free category on a directed multigraph

Categorically: take a directed multigraph `G` with object set `G.Obj`
and an edge family `G.Edge a b` indexed by source and target.  The
**free category** `Free(G)` has the same objects, and its morphisms
`a → b` are finite paths from `a` to `b`.  Composition is path
concatenation; identities are empty paths.  This is left-adjoint to
the underlying-graph functor `Cat → Graph`. -/

structure Graph where
  Obj  : Type
  Edge : Obj → Obj → Type

/-- A path in `G` from `a` to `b` is built one of two ways:
  · `nil a` — the empty path, which has source = target = `a` (no
    edges to traverse, so we can't go anywhere).  In `G.toCat` below
    this empty path will *become* the identity morphism on `a`.
  · `cons e p` — an edge `e : a → b'` followed by a path `p : b' → b`.

Lean note: this is an *indexed* inductive type — the source/target
appear after the colon, so each constructor can constrain them
differently (here `nil` forces source = target).  In ML/Haskell
terms, a GADT. -/
inductive Path (G : Graph) : G.Obj → G.Obj → Type where
  | nil  : (a : G.Obj) → Path G a a
  | cons : G.Edge a b → Path G b c → Path G a c

namespace Path
variable {G : Graph}

/-- Path concatenation, defined by recursion on the first path.

Lean note: the leading wildcards `_, _, _` are the three implicit
object indices `a, b, c`; the explicit pattern is on the two paths. -/
def append : ∀ {a b c}, Path G a b → Path G b c → Path G a c
  | _, _, _, .nil _,    q => q
  | _, _, _, .cons e p, q => .cons e (p.append q)

/-- Right identity of concatenation: `p · 1 = p`.  Proof by induction
on `p`; the inductive step lifts the IH through `cons e`. -/
theorem append_nil : ∀ {a b} (p : Path G a b), p.append (nil _) = p
  | _, _, .nil _    => rfl
  | _, _, .cons e p => congrArg (cons e) p.append_nil

/-- Associativity: `(p · q) · r = p · (q · r)`.  Induction on `p`. -/
theorem append_assoc : ∀ {a b c d} (p : Path G a b) (q : Path G b c) (r : Path G c d),
    (p.append q).append r = p.append (q.append r)
  | _, _, _, _, .nil _,    _, _ => rfl
  | _, _, _, _, .cons e p, q, r => congrArg (cons e) (append_assoc p q r)

/-- Number of generating edges in a path.  When `G` has a single
self-loop (as our `DDS` below does) this gives the natural-number
index of the resulting morphism in the free monoid `ℕ`. -/
def length : Path G a b → Nat
  | .nil _    => 0
  | .cons _ p => 1 + p.length

end Path

/-- Bundle the free-category data into our `Cat` interface.  The two
identity laws and associativity are discharged by `rfl` and the two
theorems just proved. -/
def Graph.toCat (G : Graph) : Cat where
  Obj         := G.Obj
  Hom         := Path G
  idH         := Path.nil
  comp        := Path.append
  id_comp _   := rfl
  comp_id     := Path.append_nil
  assoc       := Path.append_assoc

/-! ### Universal property: graph homomorphisms induce functors

A multigraph homomorphism `G → H` is an object map `o` plus, for each
edge `a → b` in `G`, a *path* `o(a) → o(b)` in `H`.  (Generators on
the source, paths on the target — this asymmetry is exactly the
Free ⊣ Underlying adjunction in disguise.)  Such data extends uniquely
to a functor `Free(G) → Free(H)`. -/

structure GraphHom (G H : Graph) where
  o : G.Obj → H.Obj
  e : ∀ {a b : G.Obj}, G.Edge a b → Path H (o a) (o b)

namespace GraphHom
variable {G H : Graph}

/-- The induced action on paths: send each generator to its assigned
path and concatenate. -/
def fmap (φ : GraphHom G H) : ∀ {a b : G.Obj}, Path G a b → Path H (φ.o a) (φ.o b)
  | _, _, .nil _    => .nil _
  | _, _, .cons e p => (φ.e e).append (φ.fmap p)

/-- Functoriality of `fmap`: `fmap (p · q) = fmap p · fmap q`.

Lean note: in the inductive step we must reassociate, hence the
`rw [Path.append_assoc]`.  The `show` line restates the goal in a form
where the IH is directly applicable. -/
theorem fmap_append (φ : GraphHom G H) :
    ∀ {a b c} (p : Path G a b) (q : Path G b c),
      φ.fmap (p.append q) = (φ.fmap p).append (φ.fmap q)
  | _, _, _, .nil _,    _ => rfl
  | _, _, _, .cons e p, q => by
      show (φ.e e).append (φ.fmap (p.append q))
        = ((φ.e e).append (φ.fmap p)).append (φ.fmap q)
      rw [fmap_append φ p q, Path.append_assoc]

/-- Bundle into a functor between the free categories. -/
def toFunc (φ : GraphHom G H) : G.toCat ⇒ H.toCat where
  o          := φ.o
  f          := φ.fmap
  f_id _     := rfl
  f_comp p q := φ.fmap_append p q

end GraphHom

/-! ## 2. The schemas, loaded from `migration.typ`

The schema data lives in `migration.typ`, inside a block comment that
follows the original mermaid edge format (`<src> -- <label> --> <tgt>`
under `%% id: <NAME>` markers).  We embed that file's contents at
compile time with `include_str` and parse out the labelled edges with
the small `Schema` library.  This way the diagram typst renders into
the PDF and the data Lean sees are *literally the same source*.

`FinGraphPres` (defined in `Schema.lean`) carries the parsed data:
a list of distinct object names plus a list of `(src, tgt, label)`
edge triples with numeric indices.  Here we add the conversion to
our `Graph` interface — for each source/target pair, the edge type
is the count of edges going from one to the other. -/

def FinGraphPres.toGraph (P : FinGraphPres) : Graph where
  Obj := Fin P.objects.length
  Edge a b := Fin (P.edges.filter (fun e => e.1 == a.val && e.2.1 == b.val) |>.length)

/-! ### `Gr` and `DDS`, parsed from the typst source

`schema_pres!` is a custom term elaborator (defined in `Schema.lean`)
that reads the typst file at *elaboration time*, runs the mermaid-edge
parser, and emits a literal `FinGraphPres`.  Doing the parsing at
elab time (rather than at term-reduction time) is essential: most of
Lean's `String` operations are `@[extern]` and don't reduce in the
kernel, so a `def` that calls the parser would block all subsequent
`rfl` tests.  As a literal, on the other hand, the parsed presentation
is just a chunk of constant data the kernel can compute on freely.

Nodes are listed in the order they first appear in edge lines, so
for `Gr` (whose first line is `E -- s --> V`) we get **`E = 0`, `V = 1`**.
For `DDS`, the only node is `S = 0`. -/

def Gr_data : FinGraphPres := schema_pres! "migration.typ" "Gr"
def DDS_data : FinGraphPres := schema_pres! "migration.typ" "DDS"

def Gr_pres : Graph := Gr_data.toGraph
def DDS_pres : Graph := DDS_data.toGraph

def Gr : Cat := Gr_pres.toCat
def DDS : Cat := DDS_pres.toCat

/-! Convenience names for objects and arrows.

Lean note: `abbrev` is `def` plus `@[reducible]`, so the elaborator
unfolds the right-hand side eagerly — necessary for the `rfl` tests
later in the file to reduce through these names.

The fully-explicit `@Path.cons …` form is needed because the edge
type only reduces once Lean knows the source and target objects:
pinning `b := grV` lets `Gr_pres.Edge grE grV` reduce to a concrete
`Fin n` so that `⟨0, by decide⟩` can be parsed against it. -/

abbrev grE : Gr.Obj := ⟨0, by decide⟩
abbrev grV : Gr.Obj := ⟨1, by decide⟩
abbrev grS : grE ⟶ grV := @Path.cons Gr_pres grE grV grV ⟨0, by decide⟩ (.nil grV)
abbrev grT : grE ⟶ grV := @Path.cons Gr_pres grE grV grV ⟨1, by decide⟩ (.nil grV)

abbrev ddsS : DDS.Obj := ⟨0, by decide⟩
abbrev ddsNext : ddsS ⟶ ddsS := .cons ⟨0, by decide⟩ (.nil _)

/-! ### `Gr → DDS` — Fong's running example

The migration `F` also lives in `migration.typ` (in the same schema
block), with each edge of the form `<gr-thing> -- F --> <dds-thing>`.
Object lines (`V→S`, `E→S`) and edge lines (`s→id`, `t→next`) share
the same section; the schema-aware Python app `migrate.py` classifies
them at runtime.

We expose the parsed data here so external tools — or a `#guard` — can
verify that the schema source hasn't drifted from the hand-written
`Gr_to_DDS_pres` below.  Constructing the `GraphHom` programmatically
from `F_data` would require lookups and termination proofs; for a
four-line table the inline form is clearer.

The pattern `⟨0, _⟩, ⟨1, _⟩, ⟨0, _⟩` reads: source object 0 (E),
target object 1 (V), edge index 0 (s).  Other source/target pairs
have `Fin 0` edge type and are vacuously handled by exhaustiveness. -/

def F_data : FinGraphPres := schema_pres! "migration.typ" "F"

def Gr_to_DDS_pres : GraphHom Gr_pres DDS_pres where
  o _ := ⟨0, by decide⟩
  e {a b} edge := match a, b, edge with
    | ⟨0, _⟩, ⟨1, _⟩, ⟨0, _⟩ => .nil _                          -- s ↦ id_S
    | ⟨0, _⟩, ⟨1, _⟩, ⟨1, _⟩ => .cons ⟨0, by decide⟩ (.nil _)   -- t ↦ next

-- Sanity check: F_data parsed from migration.typ matches the inline functor.
#guard F_data.objects = ["V", "S", "E", "s", "id", "t", "next"]
#guard F_data.edges = [(0, 1, "F"), (2, 1, "F"), (3, 4, "F"), (5, 6, "F")]

def Gr_to_DDS : Gr ⇒ DDS := Gr_to_DDS_pres.toFunc

/-! ## 3. The migration triple Σ ⊣ Δ ⊣ Π

For the rest of the file we work with **set-valued instances**, i.e.
functors `S ⇒ Set`.  We package "type-valued functor" as a small
structure (skipping the functor laws — this is exposition, not kernel
verification): -/

structure TypeFunc (S : Cat) where
  obj : S.Obj → Type
  map : ∀ {a b : S.Obj}, (a ⟶ b) → obj a → obj b

/-! ### Δ_F : the easy member, by precomposition

Given `G : Tgt ⇒ Set`, define `(Δ_F G)(s) := G(F.o s)` with morphism
action transported through `F`.

For our example, `Δ_F H` turns a DDS-instance `H : DDS ⇒ Set` into a
graph: vertices = states, edges = states (same set, since `F` collapses
both `V` and `E` to `S`), `src` = identity, `tgt` = `next_H`.  This is
the **trajectory graph** of `H`. -/

def deltaFunc {Src Tgt : Cat} (F : Src ⇒ Tgt) (G : TypeFunc Tgt) : TypeFunc Src where
  obj s := G.obj (F.o s)
  map f := G.map (F.f f)

/-! ### Σ_F : pointwise left Kan extension as a coend

The pointwise formula:

```
   (Σ_F G)(d) = ∫^c (F.o c ⟶ d) × G.obj c
```

In plain terms: triples `(c, α, x)` with `c ∈ Src.Obj`, `α : F.o c → d`
in `Tgt`, and `x ∈ G.obj c`, modulo the **dinatural** identifications

```
   (c,  F.f g ≫ α,  x)   ~   (c',  α,  G.map g x)
```

for every `g : c → c'` in `Src`.

We declare those triples as `LanPre`, the relation as an inductive
`Prop` `LanRel`, and take `Quot` to get the quotient set.

For our example, `Σ_F G(S)` is the **free DDS on the graph G**: every
G-edge `u→v` becomes the equation `next(u) = v`.  Vertices with two
parallel out-edges force their targets to be identified; vertices with
no out-edge get a free ω-orbit appended. -/

/-- Pre-quotient triples. -/
structure LanPre {Src Tgt : Cat} (F : Src ⇒ Tgt) (G : TypeFunc Src) (d : Tgt.Obj) where
  src : Src.Obj
  α   : F.o src ⟶ d
  x   : G.obj src

/-- The dinatural relation; `Quot` will close it under reflexivity,
symmetry, and transitivity. -/
inductive LanRel {Src Tgt : Cat} (F : Src ⇒ Tgt) (G : TypeFunc Src) (d : Tgt.Obj) :
    LanPre F G d → LanPre F G d → Prop where
  | step {c c' : Src.Obj} (g : c ⟶ c') (α : F.o c' ⟶ d) (x : G.obj c) :
      LanRel F G d ⟨c, F.f g ≫ α, x⟩ ⟨c', α, G.map g x⟩

/-- `(Σ_F G)(d)` — the pointwise left Kan extension as a Lean type. -/
def Lan {Src Tgt : Cat} (F : Src ⇒ Tgt) (G : TypeFunc Src) (d : Tgt.Obj) : Type :=
  Quot (LanRel F G d)

/-! ### Π_F : pointwise right Kan extension as an end

The pointwise formula:

```
   (Π_F G)(d) = ∫_c (F.o c ⟶ d) ⇒ G.obj c
```

An element is a **dinatural section**: a family `sect c : (d ⟶ F.o c)
→ G.obj c` (one component per object of `Src`) satisfying the
naturality square.

For our example, `Π_F G(S)` is the set of **infinite trajectories in
G**: sequences `(v₀, e₀, v₁, e₁, …)` with `src(eᵢ) = vᵢ` and
`tgt(eᵢ) = vᵢ₊₁`.  Dynamics on `Π_F G(S)` = shift by one step.  Empty
when `G` has only dead-ends. -/

structure Ran {Src Tgt : Cat} (F : Src ⇒ Tgt) (G : TypeFunc Src) (d : Tgt.Obj) where
  sect : ∀ c, (d ⟶ F.o c) → G.obj c
  nat  : ∀ {a b : Src.Obj} (g : a ⟶ b) (α : d ⟶ F.o a),
           G.map g (sect a α) = sect b (α ≫ F.f g)

/-! ### Bundling the triple

A `Triple` is just the three components packaged together.  We don't
yet prove the adjunctions `Σ ⊣ Δ ⊣ Π` here — see
`adjunction-mathlib/Adjunction.lean` for the Mathlib version that does. -/

structure Triple {Src Tgt : Cat} (_F : Src ⇒ Tgt) where
  lan   : TypeFunc Src → (Tgt.Obj → Type)
  delta : TypeFunc Tgt → TypeFunc Src
  ran   : TypeFunc Src → (Tgt.Obj → Type)

def schemaMigration {Src Tgt : Cat} (F : Src ⇒ Tgt) : Triple F where
  lan   G := Lan F G
  delta   := deltaFunc F
  ran   G := Ran F G

/-- Specialise to Fong's `Gr ⇒ DDS`. -/
def fongMigration : Triple Gr_to_DDS := schemaMigration Gr_to_DDS

/-! ## 4. Worked example: §3.11 through `Δ_F`

Fong's §3.11 picks a specific 7-state DDS:

```
   next  1 → 4    next  4 → 5
         2 → 4          5 → 5
         3 → 5          6 → 7
                        7 → 6
```

Lean uses 0-indexed `Fin 7`, so the table shifts down by one:

```
   next  0 → 3    next  3 → 4
         1 → 3          4 → 4
         2 → 4          5 → 6
                        6 → 5
```

We feed this DDS through `Δ_F` and read off the resulting trajectory
graph.  Since `s` is mapped to the identity in `Gr_to_DDS`, the
trajectory graph's `src` is the identity on states; since `t` is mapped
to `next`, its `tgt` is `next7`. -/

private def iter (f : Fin 7 → Fin 7) : Nat → Fin 7 → Fin 7
  | 0,     x => x
  | n + 1, x => iter f n (f x)

def next7 : Fin 7 → Fin 7
  | ⟨0, _⟩ => 3
  | ⟨1, _⟩ => 3
  | ⟨2, _⟩ => 4
  | ⟨3, _⟩ => 4
  | ⟨4, _⟩ => 4
  | ⟨5, _⟩ => 6
  | ⟨6, _⟩ => 5

/-- Fong's §3.11 DDS as a `TypeFunc DDS`.  The state set is `Fin 7`;
since DDS-morphisms are paths in a one-loop graph, `map p` reads off
the path's length (the ℕ-index of the morphism in the free monoid)
and iterates `next7` that many times. -/
def fong_311 : TypeFunc DDS where
  obj _ := Fin 7
  map p := iter next7 p.length

/-- Apply `Δ_F` to obtain the trajectory graph. -/
def trajGraph : TypeFunc Gr := fongMigration.delta fong_311

/-! ### Verification by computation

`example : T := rfl` checks that `T` holds *by reduction* — both sides
of the equation reduce to the same normal form using only the
computation rules of Lean.  These rfl-tests are our type-level "unit
tests": each one runs the data through the migration and confirms the
answer matches. -/

-- Both V and E get the state set `Fin 7`:
example : trajGraph.obj grV = Fin 7 := rfl
example : trajGraph.obj grE = Fin 7 := rfl

-- `src` is the identity:
example (i : Fin 7) : trajGraph.map grS i = i := rfl

-- `tgt` is `next7`:
example : trajGraph.map grT (0 : Fin 7) = (3 : Fin 7) := rfl
example : trajGraph.map grT (1 : Fin 7) = (3 : Fin 7) := rfl
example : trajGraph.map grT (2 : Fin 7) = (4 : Fin 7) := rfl
example : trajGraph.map grT (3 : Fin 7) = (4 : Fin 7) := rfl
example : trajGraph.map grT (4 : Fin 7) = (4 : Fin 7) := rfl
example : trajGraph.map grT (5 : Fin 7) = (6 : Fin 7) := rfl
example : trajGraph.map grT (6 : Fin 7) = (5 : Fin 7) := rfl

-- End-to-end: a literal list of inputs, mapped through `Δ_F`, equals
-- the literal list of expected `(edge, src, tgt)` triples — verified
-- by reduction alone, no tactics required.
example :
    ([0, 1, 2, 3, 4, 5, 6] : List (Fin 7)).map
      (fun e => (e, trajGraph.map grS e, trajGraph.map grT e))
    =
    [((0 : Fin 7), (0 : Fin 7), (3 : Fin 7)),
     ((1 : Fin 7), (1 : Fin 7), (3 : Fin 7)),
     ((2 : Fin 7), (2 : Fin 7), (4 : Fin 7)),
     ((3 : Fin 7), (3 : Fin 7), (4 : Fin 7)),
     ((4 : Fin 7), (4 : Fin 7), (4 : Fin 7)),
     ((5 : Fin 7), (5 : Fin 7), (6 : Fin 7)),
     ((6 : Fin 7), (6 : Fin 7), (5 : Fin 7))] := rfl

-- Σ_F and Π_F are real types now (quotient and naturality-subtype
-- respectively), so they typecheck applied to any `G : TypeFunc Gr`.
example (G : TypeFunc Gr) (d : DDS.Obj) : Type := fongMigration.lan G d
example (G : TypeFunc Gr) (d : DDS.Obj) : Type := fongMigration.ran G d
