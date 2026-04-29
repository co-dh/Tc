// migration.typ — three worked examples of Fong & Spivak's
// schema-migration triple Σ_F ⊣ Δ_F ⊣ Π_F:
//
//   Part I.   F : Gr → DDS              (§3.11 — running example)
//   Part II.  ! : DDS → 1                (§3.4.4 — terminal functor)
//   Part III. ! : Air → 1                 (§3.2  — airline reservations)
//
// For each part the typst file embeds *three* artefacts produced from
// a single specification text file at fong/examples/<name>.txt:
//
//   1. the spec itself (raw `.txt`);
//   2. the q triple    (read from triple-<name>.q);
//   3. the PRQL triple (read from triple-<name>.prql).
//
// Each triple is produced by running
//
//   LEAN_PATH=. lean --run migration.lean examples/<name>.txt q
//   LEAN_PATH=. lean --run migration.lean examples/<name>.txt prql
//
// from the fong/ directory.  The Lean source is the single source of
// truth: there is no Python codegen any more.

/*  schema-data block — read by `schema_pres!` in migration.lean at
    elaboration time.  Keep this block syntactically isomorphic to
    what files in `examples/` produce so the inline lean schemas and
    the text-file specs can't drift.  (NB: avoid the literal slash-
    star sequence in this comment — typst block comments nest, and
    an unbalanced inner `/` `*` would silently swallow the rest of
    the document.)

%% id: Gr
E -- s --> V
E -- t --> V

%% id: DDS
S -- next --> S

%% id: F
V -- F --> S
E -- F --> S
s -- F --> id
t -- F --> next

%% id: One
star -- ! --> star

%% id: Bang
S    -- ! --> star
next -- ! --> id

%% id: Air
F -- src    --> C
F -- tgt    --> C
B -- flight --> F
B -- who    --> P
P -- home   --> C
*/

#import "@preview/cetz:0.3.4": canvas, draw
#import "@preview/arkheion:0.1.0": arkheion

#show: arkheion.with(
  title: "Schema migration: a worked tour",
  authors: (
    (name: "Fong & Spivak — Seven Sketches Ch. 3",
     email: "", affiliation: ""),
  ),
  abstract: [
    Three concrete examples of categorical schema migration, each
    described by a tiny edge-list spec under `fong/examples/`.
    `migration.lean` parses the spec, instantiates `FinGraphPres` for
    `C` and `D`, and emits a complete migration triple
    `(Δ_F, Σ_F, Π_F)` in either q or PRQL.  All three pieces of code
    you read below — the parsed schemas, the round-trip examples, and
    the generated triples — come from that one Lean source.],
)

#show raw.where(block: true): set block(
  stroke: 0.5pt + gray, inset: 6pt, radius: 3pt, width: 100%)
#set raw(syntaxes: ("q.sublime-syntax", "prql.sublime-syntax"))

// ════════════════════════════════════════════════════════════════
= Pipeline
// ════════════════════════════════════════════════════════════════

Each example file under `fong/examples/` is a plain text edge-list:

```
%% id: <category-name>
<src> -- <arrow-label> --> <tgt>
...

%% id: <functor-name>
<C-object> -- F --> <D-object>
<C-arrow>  -- F --> <D-path>
```

The runner `lean --run migration.lean <spec>.txt {q|prql}` consumes
one of these files and prints a complete triple to stdout:

```sh
cd fong/
LEAN_PATH=. lean --run migration.lean examples/bang.txt q     > triple-bang.q
LEAN_PATH=. lean --run migration.lean examples/bang.txt prql  > triple-bang.prql
```

Three reasoning regimes are recognised automatically:

+ *F is the identity functor* — Σ_F and Π_F are the identity.
+ *D is terminal* (the "bang" functor `!: C → 1`) — Σ_F is the
  union-find quotient (orbits / connected components) and Π_F is the
  set of compatible families across all C-arrows.
+ *Otherwise* — the q/PRQL emit a TODO stub for Σ and Π; only Δ_F
  (precomposition / pullback) is produced in full generality.

The three sections that follow exhibit one example for each regime:
Part I uses the third (Δ only), Parts II and III use the second
(full triple).


// ════════════════════════════════════════════════════════════════
= Part I.  `F : Gr → DDS`  (§3.11)
// ════════════════════════════════════════════════════════════════

== The spec

`Gr` is a directed graph (objects $E$, $V$ with `s, t : E → V`); `DDS`
is a discrete dynamical system (one state object $S$ with a self-loop
`next`).  The functor sends both `Gr`-objects to $S$, the source map
to the identity, and the target map to `next` — i.e. an edge $e$ of
`Gr` represents the trajectory step $s(e) ↦ "next"(s(e))$ in `DDS`.

#raw(read("examples/gr_to_dds.txt"), block: true)

== The diagram

`Gr` on top (blue), `DDS` at the bottom (orange); `F` is dashed grey.
The *object map* `V↦S`, `E↦S` runs along the panel edges, and the
*edge map* `s↦id`, `t↦next` runs from each `Gr`-arrow midpoint to its
`DDS` image.

#align(center, canvas(length: 1.0cm, {
  import draw: *

  let gr-fill    = rgb("#eaf2fb")
  let gr-stroke  = rgb("#6c8ebf")
  let dds-fill   = rgb("#fdf1e3")
  let dds-stroke = rgb("#d79b00")
  let arr-text(b)  = text(weight: "bold", fill: rgb("#996600"), size: 13pt, b)
  let arr-text2(b) = text(weight: "bold", fill: rgb("#b00000"), size: 13pt, b)
  let f-stroke = (paint: gray, dash: "dashed", thickness: 0.8pt)
  let f-text(b) = text(fill: gray.darken(20%), weight: "bold", size: 10pt, b)

  rect((-1.4, -1.4), (5.4, 1.8),
       fill: gr-fill, stroke: gr-stroke + 1pt, radius: 0.18)
  content((-0.6, 1.45), text(weight: "bold", fill: gr-stroke, size: 13pt)[Gr])

  let E = (0.0, 0.2)
  let V = (4.0, 0.2)
  circle(E, radius: 0.34, fill: rgb("#dae8fc"), stroke: gr-stroke + 1pt)
  content(E, text(size: 13pt)[$E$])
  circle(V, radius: 0.34, fill: rgb("#dae8fc"), stroke: gr-stroke + 1pt)
  content(V, text(size: 13pt)[$V$])

  let s-apex = (2.0, 0.85)
  let t-apex = (2.0, -0.45)
  let s-label = (s-apex.at(0), s-apex.at(1) + 0.40)
  let t-label = (t-apex.at(0), t-apex.at(1) - 0.40)
  bezier((E.at(0)+0.34, E.at(1)+0.10), (V.at(0)-0.34, V.at(1)+0.10),
         (1.2, s-apex.at(1)+0.05), (2.8, s-apex.at(1)+0.05),
         mark: (end: ">"), stroke: 1pt)
  content(s-label, arr-text[$s$])
  bezier((E.at(0)+0.34, E.at(1)-0.10), (V.at(0)-0.34, V.at(1)-0.10),
         (1.2, t-apex.at(1)-0.05), (2.8, t-apex.at(1)-0.05),
         mark: (end: ">"), stroke: 1pt)
  content(t-label, arr-text[$t$])

  rect((-1.4, -5.6), (5.4, -2.6),
       fill: dds-fill, stroke: dds-stroke + 1pt, radius: 0.18)
  content((-0.4, -2.95), text(weight: "bold", fill: dds-stroke, size: 13pt)[DDS])

  let S = (2.0, -4.1)
  circle(S, radius: 0.40, fill: rgb("#ffe6cc"), stroke: dds-stroke + 1pt)
  content(S, text(size: 13pt)[$S$])

  let id-apex = (0.85, -4.1)
  let nx-apex = (3.15, -4.1)
  let id-label = (id-apex.at(0) - 0.45, id-apex.at(1))
  let nx-label = (nx-apex.at(0) + 0.55, nx-apex.at(1))
  bezier((S.at(0)-0.34, S.at(1)+0.22), (S.at(0)-0.34, S.at(1)-0.22),
         (id-apex.at(0)+0.10, S.at(1)+0.55), (id-apex.at(0)+0.10, S.at(1)-0.55),
         mark: (end: ">"), stroke: 1pt)
  content(id-label, arr-text2[$"id"$])
  bezier((S.at(0)+0.34, S.at(1)-0.22), (S.at(0)+0.34, S.at(1)+0.22),
         (nx-apex.at(0)-0.10, S.at(1)-0.55), (nx-apex.at(0)-0.10, S.at(1)+0.55),
         mark: (end: ">"), stroke: 1pt)
  content(nx-label, arr-text2[$"next"$])

  line((E.at(0), E.at(1)-0.34), (S.at(0)-0.40, S.at(1)+0.30),
       mark: (end: ">"), stroke: f-stroke)
  content((-0.10, -2.10), f-text[$F$])
  line((V.at(0), V.at(1)-0.34), (S.at(0)+0.40, S.at(1)+0.30),
       mark: (end: ">"), stroke: f-stroke)
  content((4.10, -2.10), f-text[$F$])

  line((s-label.at(0) - 0.10, s-label.at(1) - 0.18),
       (id-label.at(0) + 0.30, id-label.at(1) + 0.18),
       mark: (end: ">"), stroke: f-stroke)
  content((1.20, -1.10), f-text[$F$])
  line((t-label.at(0) + 0.10, t-label.at(1) - 0.18),
       (nx-label.at(0) - 0.30, nx-label.at(1) + 0.18),
       mark: (end: ">"), stroke: f-stroke)
  content((2.85, -2.50), f-text[$F$])
}))

== Generated triple

`D = DDS` is *not* terminal (it has the non-identity arrow `next`), so
this falls into the third regime: only Δ_F is produced in full; Σ_F
and Π_F are emitted as TODO stubs awaiting the pointwise Kan-extension
construction.

=== q

#raw(read("triple-gr_to_dds.q"), lang: "q", block: true)

=== PRQL

#raw(read("triple-gr_to_dds.prql"), lang: "prql", block: true)

== Round-trip on Δ_F

`delta` of a DDS-instance produces the trajectory graph: every state
becomes a vertex, and every $(s, "next"(s))$ pair becomes an edge of
$"Gr"$.  Concretely (q):

```sh
$ q
q)\l triple-gr_to_dds.q
q)D:enlist[`S]!enlist ([] id:0 1 2 3 4; next_:1 2 3 3 0)
q)delta D
E| +`id`s`t!(0 1 2 3 4;0 1 2 3 4;1 2 3 3 0)
V| +(,`id)!,`s#0 1 2 3 4
```

Edge $e_i$ has source $i$ and target `next`$(i)$.  Vertex set is
$"asc distinct"$ over all states — exactly the trajectory graph that
`migration.lean`'s rfl tests verify on the same data.


// ════════════════════════════════════════════════════════════════
= Part II.  `! : DDS → 1`  (§3.4.4)
// ════════════════════════════════════════════════════════════════

== The terminal category and `!`

The terminal category `1` has one object — call it `*` — and only its
identity arrow.  For any category `C` there is a unique functor
`!: C → 1` collapsing every object to `*` and every arrow to `id`.

== The spec

#raw(read("examples/bang.txt"), block: true)

== The diagram

`DDS` (top, orange) has its state object $S$ and the arrow `next`;
`1` (bottom, purple) has only `*` and `id`#sub[`*`].  `!` is dashed:
$S ↦ ast$, $"next" ↦ "id"_ast$.

#align(center, canvas(length: 1.0cm, {
  import draw: *

  let dds-fill   = rgb("#fdf1e3")
  let dds-stroke = rgb("#d79b00")
  let one-fill   = rgb("#e1d5e7")
  let one-stroke = rgb("#9673a6")
  let arr-text(b)  = text(weight: "bold", fill: rgb("#b00000"), size: 13pt, b)
  let arr-text2(b) = text(weight: "bold", fill: rgb("#5a3d7a"), size: 13pt, b)
  let f-stroke = (paint: gray, dash: "dashed", thickness: 0.8pt)
  let f-text(b) = text(fill: gray.darken(20%), weight: "bold", size: 10pt, b)

  rect((-1.4, -1.3), (5.4, 1.6),
       fill: dds-fill, stroke: dds-stroke + 1pt, radius: 0.18)
  content((-0.4, 1.30), text(weight: "bold", fill: dds-stroke, size: 13pt)[DDS])

  let S = (2.0, 0.2)
  circle(S, radius: 0.40, fill: rgb("#ffe6cc"), stroke: dds-stroke + 1pt)
  content(S, text(size: 13pt)[$S$])

  let id-apex = (0.85, 0.2)
  let nx-apex = (3.15, 0.2)
  let id-label = (id-apex.at(0) - 0.45, id-apex.at(1))
  let nx-label = (nx-apex.at(0) + 0.55, nx-apex.at(1))
  bezier((S.at(0)-0.34, S.at(1)+0.22), (S.at(0)-0.34, S.at(1)-0.22),
         (id-apex.at(0)+0.10, S.at(1)+0.55), (id-apex.at(0)+0.10, S.at(1)-0.55),
         mark: (end: ">"), stroke: 1pt)
  content(id-label, arr-text[$"id"$])
  bezier((S.at(0)+0.34, S.at(1)-0.22), (S.at(0)+0.34, S.at(1)+0.22),
         (nx-apex.at(0)-0.10, S.at(1)-0.55), (nx-apex.at(0)-0.10, S.at(1)+0.55),
         mark: (end: ">"), stroke: 1pt)
  content(nx-label, arr-text[$"next"$])

  rect((-1.4, -4.6), (5.4, -1.8),
       fill: one-fill, stroke: one-stroke + 1pt, radius: 0.18)
  content((-0.5, -2.10), text(weight: "bold", fill: one-stroke, size: 13pt)[1])

  let star = (2.0, -3.2)
  circle(star, radius: 0.40, fill: rgb("#d5c2e8"), stroke: one-stroke + 1pt)
  content(star, text(size: 14pt, weight: "bold")[$ast$])

  let one-id-apex = (3.15, -3.2)
  let one-id-label = (one-id-apex.at(0) + 0.55, one-id-apex.at(1))
  bezier((star.at(0)+0.34, star.at(1)-0.22), (star.at(0)+0.34, star.at(1)+0.22),
         (one-id-apex.at(0)-0.10, star.at(1)-0.55), (one-id-apex.at(0)-0.10, star.at(1)+0.55),
         mark: (end: ">"), stroke: 1pt)
  content(one-id-label, arr-text2[$"id"_ast$])

  line((S.at(0), S.at(1)-0.40), (star.at(0), star.at(1)+0.40),
       mark: (end: ">"), stroke: f-stroke)
  content((S.at(0) + 0.20, (S.at(1)+star.at(1))/2), f-text[$!$])

  line((nx-apex.at(0), nx-apex.at(1) - 0.55),
       (one-id-apex.at(0), one-id-apex.at(1) + 0.55),
       mark: (end: ">"), stroke: f-stroke)
  content((nx-apex.at(0) + 0.20, (nx-apex.at(1)+one-id-apex.at(1))/2), f-text[$!$])
}))

== Generated triple

Because `D = 1` is terminal, the codegen emits all three legs in full.
`Σ_!` is the orbit set (union-find quotient by `next`); `Π_!` is the
fixed-point set; `Δ_!` is the flat instance where every state is its
own successor.

=== q

#raw(read("triple-bang.q"), lang: "q", block: true)

=== PRQL

#raw(read("triple-bang.prql"), lang: "prql", block: true)

== Round-trip with Fong's §3.11 instance

```sh
$ q
q)\l triple-bang.q
q)I:enlist[`S]!enlist ([] id:0 1 2 3 4 5 6; next_:1 2 3 3 5 6 4)
q)sigma I                       / connected components
orbit
-----
"S_3"
"S_6"
q)pi I                          / fixed points
x_S
---
3
q)delta enlist[`star]!enlist ([] id:`a`b`c)  / flat DDS on {a,b,c}
S| +`id`next_!(`a`b`c;`a`b`c)
```

Two orbits (the chain $0→1→2→3$ ending at the fixed point 3, and the
3-cycle $4→5→6→4$ whose canonical representative is 6); one fixed
point (3); and `Δ_!` of the three-element set `{a,b,c}` is flat:
each state's `next` equals itself.


// ════════════════════════════════════════════════════════════════
= Part III.  `! : Air → 1`  (§3.2)
// ════════════════════════════════════════════════════════════════

== The schema

`Air` has four objects:

#table(columns: (auto, 1fr), stroke: none, inset: 4pt,
  [`C`], [#strong[City]    — endpoints of flights, also where people live],
  [`F`], [#strong[Flight]  — a flight is a leg from one city to another],
  [`P`], [#strong[Person]  — a passenger; lives in some `City`],
  [`B`], [#strong[Booking] — a booking pairs a `Person` with a `Flight`],
)

and five arrows:

#table(columns: (auto, auto), stroke: none, inset: 4pt,
  [`F -- src    --> C`], [departure city of a flight],
  [`F -- tgt    --> C`], [arrival city of a flight],
  [`B -- flight --> F`], [which flight is booked],
  [`B -- who    --> P`], [who booked it],
  [`P -- home   --> C`], [the passenger's home city],
)

== The spec

#raw(read("examples/air_to_one.txt"), block: true)

== The diagram

#align(center, canvas(length: 1.0cm, {
  import draw: *

  let air-fill   = rgb("#e8f4ea")
  let air-stroke = rgb("#67ab78")
  let arr-text(b) = text(weight: "bold", fill: rgb("#1f6f3a"), size: 12pt, b)

  rect((-2.4, -3.0), (6.4, 3.2),
       fill: air-fill, stroke: air-stroke + 1pt, radius: 0.18)
  content((-1.6, 2.85), text(weight: "bold", fill: air-stroke, size: 13pt)[Air])

  let C = (2.0, 0.0)
  let F = (-0.5, 2.2)
  let P = (4.5, 2.2)
  let B = (2.0, -2.4)

  let node(pos, lbl) = {
    circle(pos, radius: 0.36, fill: rgb("#c8e6cb"), stroke: air-stroke + 1pt)
    content(pos, text(size: 13pt)[#lbl])
  }
  node(C, $C$)
  node(F, $F$)
  node(P, $P$)
  node(B, $B$)

  bezier((F.at(0)+0.30, F.at(1)-0.20), (C.at(0)-0.30, C.at(1)+0.20),
         (0.4, 1.6), (1.0, 0.6),
         mark: (end: ">"), stroke: 1pt)
  content((0.30, 1.20), arr-text[$"src"$])
  bezier((F.at(0)+0.36, F.at(1)-0.10), (C.at(0)-0.18, C.at(1)+0.30),
         (1.0, 1.7), (1.6, 0.7),
         mark: (end: ">"), stroke: 1pt)
  content((1.30, 1.40), arr-text[$"tgt"$])

  line((P.at(0)-0.30, P.at(1)-0.20), (C.at(0)+0.30, C.at(1)+0.20),
       mark: (end: ">"), stroke: 1pt)
  content((3.55, 1.20), arr-text[$"home"$])

  bezier((B.at(0)-0.30, B.at(1)+0.20), (F.at(0)+0.20, F.at(1)-0.30),
         (-0.5, -1.2), (-1.5, 0.8),
         mark: (end: ">"), stroke: 1pt)
  content((-1.30, -0.50), arr-text[$"flight"$])

  bezier((B.at(0)+0.30, B.at(1)+0.20), (P.at(0)-0.20, P.at(1)-0.30),
         (4.5, -1.2), (5.5, 0.8),
         mark: (end: ">"), stroke: 1pt)
  content((5.30, -0.50), arr-text[$"who"$])
}))

== Generated triple

Same regime as Part II (`D = 1` is terminal), but now Σ_! and Π_!
range over the four C-objects and five C-arrows, so the union-find
edge set has five contributions and the limit's compatibility
constraint is a five-way conjunction.

=== q

#raw(read("triple-air_to_one.q"), lang: "q", block: true)

=== PRQL

#raw(read("triple-air_to_one.prql"), lang: "prql", block: true)

== Round-trip with a small instance

Two flights (SFO→LAX, SFO→JFK), three cities (0=SFO, 1=LAX, 2=JFK),
two passengers (Alice/home SFO, Bob/home LAX), two bookings
(Alice on 0, Bob on 1).

```sh
$ q
q)\l triple-air_to_one.q
q)I:`F`C`B`P!(([] id:0 1; src:0 0; tgt:1 2);
              ([] id:0 1 2);
              ([] id:0 1; flight:0 1; who:0 1);
              ([] id:0 1; home:0 1))
q)sigma I              / one connected blob  →  one orbit
orbit
-----
"P_1"
q)pi I                 / no compatible family — every flight has src≠tgt
x_F x_C x_B x_P
---------------
```

The single orbit is correct: every flight links cities, every booking
links a flight to a person, every person has a home city — so the
union-find quotient collapses all 9 entities (2F + 3C + 2P + 2B) into
one component.  `Π_!` is empty because the limit demands
$"src"(x_F) = "tgt"(x_F)$, but neither flight in our instance is a
self-loop.
