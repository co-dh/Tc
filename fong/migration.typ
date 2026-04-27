// migration.typ — single source of truth for three worked examples
// of Fong & Spivak's schema-migration pipeline:
//
//   I.    F : Gr → DDS              (§3.11 — the running example)
//   II.   ! : DDS → 1               (§3.4.4 — the terminal functor)
//   III.  Schema  Air                (§3.2  — airline reservations)
//
// The schema-data block at the top is parsed by both Schema.lean
// (`schema_pres!`) and migrate.py (`parse_schema_block`).  All
// downstream artefacts — diagrams, prose, code blocks — live inside
// this one typst file.
//
// Compile:   typst compile fong/migration.typ fong/migration.pdf

/*  schema-data — `%% id: <NAME>` blocks, edge lines of the form
        <src> -- <label> --> <tgt>

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
    Three concrete examples of categorical schema migration, sharing
    a single schema-data block at the top of this typst source so
    `Schema.lean` and `migrate.py` can both read them without leaving
    typst.  Part I works through Fong's running example
    `F : Gr → DDS` (§3.11), with the migration triple compiled to
    PRQL and q.  Part II specialises to the terminal functor
    `! : DDS → 1` (§3.4.4), where the triple becomes orbit set / flat
    DDS / fixed-point set.  Part III draws the airline-reservation
    schema `Air` (§3.2), with two natural functors out of it.],
)

#show raw.where(block: true): set block(
  stroke: 0.5pt + gray, inset: 6pt, radius: 3pt, width: 100%)
#set raw(syntaxes: ("q.sublime-syntax", "prql.sublime-syntax"))

// ════════════════════════════════════════════════════════════════
= Part I.  `F : Gr → DDS`  (§3.11)
// ════════════════════════════════════════════════════════════════

== Source-of-truth file

This `.typ` file is the single source of truth for the schemas (`Gr`,
`DDS`, plus `Air` and `1` below) and the migration functor
`F : Gr → DDS`.  The schema data sits in a typst comment at the top
of this source so two non-typst consumers can parse it directly:

+ `migration.lean` — at elaboration time, via `schema_pres!` in
  `Schema.lean` — to build the `Gr` and `DDS` categories and check
  the inline functor against the parsed `F`.
+ `migrate.py` — to emit PRQL (for DuckDB) or q (for kdb+) implementing
  the migration triple Σ ⊣ Δ ⊣ Π.

The two `migrate.py` workflows:

```sh
# PRQL/DuckDB:
./migrate.py migration.typ mydb.duckdb > triple.prql
prqlc compile -t sql.duckdb triple.prql | duckdb mydb.duckdb

# q/kdb+:
./migrate.py --target=q migration.typ > triple.q
q triple.q                                 # interactive
q)DDS:([] s:0 1 2 3 4 5 6; next_:3 3 4 4 4 6 5)
q)show sigma delta DDS                     # round-trip
```

For PRQL, `mydb.duckdb` should already contain a table `DDS(s, next)`
filled with a DDS instance.  See `sample.duckdb` (created on demand)
for Fong's §3.11 example.  For q, the table is created in-process —
the column for the DDS arrow `next` is renamed `next_` because `next`
is a built-in q identifier.

== The diagram

`Gr` is on top (blue panel), `DDS` on the bottom (orange panel), and
the functor `F` is drawn as *dashed grey arrows* from Gr to DDS — the
*object map* `V↦S`, `E↦S` runs along the panel edges, and the *edge
map* `s↦id`, `t↦next` runs from the midpoint of each Gr arrow to the
midpoint of its DDS image, so the F arrows literally point from one
arrow to another, the way a functor's edge component should.

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

  // ── Gr panel (top) ──
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

  // ── DDS panel (bottom) ──
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

  // ── F: object map (E↦S, V↦S) ──
  line((E.at(0), E.at(1)-0.34), (S.at(0)-0.40, S.at(1)+0.30),
       mark: (end: ">"), stroke: f-stroke)
  content((-0.10, -2.10), f-text[$F$])
  line((V.at(0), V.at(1)-0.34), (S.at(0)+0.40, S.at(1)+0.30),
       mark: (end: ">"), stroke: f-stroke)
  content((4.10, -2.10), f-text[$F$])

  // ── F: edge map (s↦id, t↦next) ──
  line((s-label.at(0) - 0.10, s-label.at(1) - 0.18),
       (id-label.at(0) + 0.30, id-label.at(1) + 0.18),
       mark: (end: ">"), stroke: f-stroke)
  content((1.20, -1.10), f-text[$F$])
  line((t-label.at(0) + 0.10, t-label.at(1) - 0.18),
       (nx-label.at(0) - 0.30, nx-label.at(1) + 0.18),
       mark: (end: ">"), stroke: f-stroke)
  content((2.85, -2.50), f-text[$F$])
}))

Reading the F section off: $V ↦ S$, $E ↦ S$, $s ↦ "identity"$, $t ↦ "next"$.

== Generated PRQL and q

The blocks below are *not* hand-written: each is the verbatim output
of `migrate.py` reading the schema-data block above and emitting code
for its target.  Re-run before compiling typst:

```sh
./migrate.py            migration.typ /dev/null > triple.prql
./migrate.py --target=q migration.typ            > triple.q
typst compile fong/migration.typ fong/migration.pdf
```

=== PRQL  (`triple.prql`)

The PRQL workflow assumes the input DuckDB has a table `DDS(s, next)`
for the DDS direction, or tables `V(id)` / `E(id, s, t)` for the Gr
direction.  The Π_F leg is emitted as a commented raw-SQL recursive
CTE template — pure PRQL has no recursion.

#raw(read("triple.prql"), lang: "prql", block: true)

==== Round-trip check (PRQL)

$Σ_F ∘ Δ_F$ should recover the original DDS instance.  On Fong's §3.11
example:

```sh
duckdb sample.duckdb -c "CREATE TABLE DDS(s INT, next INT);
  INSERT INTO DDS VALUES (0,3),(1,3),(2,4),(3,4),(4,4),(5,6),(6,5);"

./migrate.py migration.typ sample.duckdb \
  | grep '^let' \
  | (cat; echo 'from DDS_sigma | sort s') \
  | prqlc compile -t sql.duckdb \
  | duckdb sample.duckdb
```

returns the original 7 rows — and the `from E` query returns exactly
the trajectory triples `(id, src, tgt)` that the rfl tests in
`migration.lean` verify.

=== q  (`triple.q`)

The DDS arrow `next` becomes column `next_` in q (the bare name is a
built-in in `.q`).  Type comments on each function spell out the
expected shapes of its inputs; the `pi` definition uses q's scan
iterator (`\`) to apply `step` $N$ times starting from each vertex.

#raw(read("triple.q"), lang: "q", block: true)

==== Round-trip check (q)

```sh
./migrate.py --target=q migration.typ > triple.q
q triple.q
q)DDS:([] s:0 1 2 3 4 5 6; next_:3 3 4 4 4 6 5)
q)show sigma delta DDS         / recovers DDS
q)DDS~`s xasc sigma delta DDS  / 1b — round-trip succeeds
q)show pi[delta DDS;4]         / depth-4 trajectories from each vertex
```


// ════════════════════════════════════════════════════════════════
= Part II.  `! : DDS → 1`  (§3.4.4)
// ════════════════════════════════════════════════════════════════

== The terminal category and `!`

The *terminal category* `1` has exactly one object — call it `*` — and
exactly one arrow on it (the identity).  For any category `C` there is
a unique functor `!: C → 1` collapsing every object of `C` to `*` and
every arrow to `id`#sub[`*`].

Specialising `C := DDS` gives the picture below.  `DDS` (top, orange)
has its state object `S` and its arrow `next : S → S`; `1` (bottom,
purple) has only `*` and its identity.  The functor `!` is drawn as
dashed grey arrows: the *object map* `S ↦ *` runs along the centre,
and the *edge map* `next ↦ id`#sub[`*`] runs from the apex of the
`next`-loop in DDS to the apex of the identity loop in `1`.

== The diagram

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

  // !: object map  S ↦ *
  line((S.at(0), S.at(1)-0.40), (star.at(0), star.at(1)+0.40),
       mark: (end: ">"), stroke: f-stroke)
  content((S.at(0) + 0.20, (S.at(1)+star.at(1))/2), f-text[$!$])

  // !: edge map  next ↦ id_*
  line((nx-apex.at(0), nx-apex.at(1) - 0.55),
       (one-id-apex.at(0), one-id-apex.at(1) + 0.55),
       mark: (end: ">"), stroke: f-stroke)
  content((nx-apex.at(0) + 0.20, (nx-apex.at(1)+one-id-apex.at(1))/2), f-text[$!$])
}))

The picture says: every DDS-thing collapses.  `S` lands on `*`; the
non-trivial arrow `next` lands on the only arrow `id`#sub[`*`] there
is.  The implicit identity on `S` also lands on `id`#sub[`*`] (that's
forced by functoriality and not drawn).

== The migration triple

For `F = !` the three legs of `Σ_! ⊣ Δ_! ⊣ Π_!` collapse to familiar
constructions on a DDS.

=== Δ#sub[`!`] : Set → DDS-Set   (a set, with `next = id`)

Pre-composition by `!` turns a set $X = G(ast)$ into a DDS-instance
with state set $X$ and `next $= id_X$`: every state is its own
successor — a flat set, dynamically.

```q
delta:{[X] ([] s:X; next_:X)}
```

```prql
let DDS_delta = (from X | select { s = x, next = x })
```

=== Σ#sub[`!`] : DDS-Set → Set   (orbits of `next`)

The left Kan extension along `!` is the colimit, and the colimit of a
DDS is the *quotient* of the state set by the smallest equivalence
relation identifying `s` with `next(s)` for every state.  In English:
the set of *connected components* (orbits) of the state graph.

For Fong's §3.11 example the orbits are
$ {0,1,2,3,4} quad "and" quad {5,6} $
— two orbits: a sink rooted at the fixed point 4, and a 2-cycle.

```q
/ orbits via union-find on the (s, next_) edge list
sigma:{[D]
  / build forest: parent[s] := next_
  parent:exec next_!s from D;
  / repeatedly compress to a fixed point (works for finite state)
  root:{[p;v] $[v=p[v]; v; .z.s[p; p[v]]]};
  comps:asc distinct root[parent;] each exec s from D;
  ([] orbit:comps)}
```

```prql
# Pure PRQL has no recursion; the orbit set needs a recursive CTE:
# WITH RECURSIVE reach(a, b) AS (
#   SELECT s, next FROM DDS
#   UNION
#   SELECT r.a, d.next FROM reach r JOIN DDS d ON d.s = r.b
# )
# SELECT DISTINCT MIN(b) AS orbit FROM reach GROUP BY a
```

=== Π#sub[`!`] : DDS-Set → Set   (fixed points of `next`)

The right Kan extension along `!` is the limit, and the limit of a
DDS is its set of *fixed points* `${s : next(s) = s}$` — the states
that the dynamics never moves.  For §3.11 there is a single fixed
point, `4`.

```q
pi:{[D] exec s from D where s=next_}
```

```prql
let DDS_pi = (from DDS | filter s == next | select { s })
```

== Round-trip with the §3.11 example

```sh
q -q
q)DDS:([] s:0 1 2 3 4 5 6; next_:3 3 4 4 4 6 5)
q)pi DDS                     / fixed points    => 4
q)sigma DDS                  / orbits          => (0; 5)   (representatives)
q)show count sigma DDS       / number of orbits => 2
q)delta exec s from DDS      / flat DDS        => state=s, next_=s
```

Two orbits and one fixed point — exactly Fong's §3.11 picture, now
read off from `Σ_!` and `Π_!` directly.


// ════════════════════════════════════════════════════════════════
= Part III.  Schema  `Air`  (§3.2)
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

An *instance* of `Air` is a database: one table per object, one
foreign-key column per arrow.  In Lean / categorical terms it's a
functor `Air ⇒ Set`.

== The diagram

`C` sits at the centre; `F` (top-left), `P` (top-right), `B`
(bottom-centre) all point at it directly or indirectly.  The two
arrows `F → C` are the familiar `src` / `tgt` pair (same shape as
`Gr`'s `s, t : E → V`), which suggests the obvious sub-schema: forget
`P` and `B` and you're left with `Gr`.

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

== A sample instance

The §3.11-shaped concrete instance: three cities, three flights
forming a triangle, two passengers, three bookings.

#table(columns: 4, stroke: 0.4pt + gray, inset: 6pt, align: left,
  [*C*],   [],    [*F*], [`(src, tgt)`],
  [`ATL`], [],    [`f1`], [`(ATL, JFK)`],
  [`JFK`], [],    [`f2`], [`(JFK, LAX)`],
  [`LAX`], [],    [`f3`], [`(LAX, ATL)`],
)

#table(columns: 4, stroke: 0.4pt + gray, inset: 6pt, align: left,
  [*P*],     [`home`], [*B*],   [`(flight, who)`],
  [`alice`], [`ATL`],  [`r1`],  [`(f1, alice)`],
  [`bob`],   [`JFK`],  [`r2`],  [`(f2, bob)`],
  [],        [],       [`r3`],  [`(f3, alice)`],
)

== Two natural functors out of `Air`

=== `π : Air → Gr` — flight network

The sub-schema `(F, C)` with the two arrows `src, tgt : F → C` is
exactly `Gr`, with `Flight` playing `E` and `City` playing `V`.  The
inclusion `Gr ↪ Air` has a left adjoint *forgetful* functor
`π : Air → Gr` that drops `Person` and `Booking`.

For our sample instance, `Δ_π` of any `Gr`-instance pulls back to an
`Air`-instance with `P = B = ∅`; `Σ_π` of an `Air`-instance forgets
the `P, B` tables and keeps `(C, F)` — i.e. just the directed-graph
"flight network" of who-flies-where.

=== `! : Air → 1` — counts

As in Part II, collapsing everything to the terminal category `1`
turns `Σ_!` into the orbit/connected-component set, `Π_!` into the
limit (compatible families across all five arrows), and `Δ_!` into
the trivial flat instance.  For the sample data:

- `Σ_!`(`Air`) — connected components of the bipartite-ish graph on
  `C ⊔ F ⊔ P ⊔ B` glued by the five arrows.  Here everything is one
  connected blob, so the orbit set has size 1.
- `Π_!`(`Air`) — sections / fixed points.  None: every arrow goes
  between different objects, so there's nothing to "fix".

== Where the data comes from

The edge lines in the schema-data block at the top of this typst
source are the source of truth.  `Schema.lean`'s `schema_pres!` and
`migrate.py`'s `parse_schema_block` both consume them; the diagrams
above are hand-laid-out CeTZ.  Adding a new arrow means: add one line
to the block, redraw, recompile.
