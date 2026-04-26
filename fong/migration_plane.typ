// migration_plane.typ — Fong & Spivak's airline-reservation schema.
//
// Same shape as migration.typ / migration_terminal.typ: schema data
// at the top in a typst block comment, prose + CeTZ diagram below.
// The schema `Air` has four objects (City, Flight, Person, Booking)
// and five arrows.  Two natural functors out of `Air` are:
//
//   π : Air → Gr   — forget Person, Booking; keep (Flight, City) as
//                    the directed multigraph of legs between cities.
//   ! : Air → 1    — collapse everything (companion to migration_terminal).
//
// Compile:   typst compile fong/migration_plane.typ fong/migration_plane.pdf

/*  schema-data — same parser as the other examples.

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
  title: "Schema Air: airline reservations",
  authors: (
    (name: "Fong & Spivak — Seven Sketches §3.2",
     email: "", affiliation: ""),
  ),
  abstract: [
    The airline-reservation schema from Fong & Spivak.  Four objects
    — `City`, `Flight`, `Person`, `Booking` — and five arrows tie a
    booking to its flight, the flight to its endpoint cities, and the
    passenger to a home city.  Companion to `migration.typ`
    (`F : Gr → DDS`) and `migration_terminal.typ` (`! : DDS → 1`):
    same source-of-truth + parser pipeline, larger schema.],
)

#show raw.where(block: true): set block(
  stroke: 0.5pt + gray, inset: 6pt, radius: 3pt, width: 100%)
#set raw(syntaxes: ("q.sublime-syntax", "prql.sublime-syntax"))

= The schema

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

= The diagram

`C` sits at the centre; `F` (top), `P` (right), `B` (bottom-left) all
point at it directly or indirectly.  The two arrows `F → C` are the
familiar `src` / `tgt` pair (same shape as `Gr`'s `s, t : E → V`),
which suggests the obvious sub-schema: forget `P` and `B` and you're
left with the directed-multigraph schema `Gr`.

#align(center, canvas(length: 1.0cm, {
  import draw: *

  let air-fill   = rgb("#e8f4ea")
  let air-stroke = rgb("#67ab78")
  let arr-text(b) = text(weight: "bold", fill: rgb("#1f6f3a"), size: 12pt, b)

  // Panel
  rect((-2.4, -3.0), (6.4, 3.2),
       fill: air-fill, stroke: air-stroke + 1pt, radius: 0.18)
  content((-1.6, 2.85), text(weight: "bold", fill: air-stroke, size: 13pt)[Air])

  // ── Object positions ── 4-corner-ish layout, cities (C) in the middle.
  let C = (2.0, 0.0)        // centre — the "hub" all flights touch
  let F = (-0.5, 2.2)       // top-left: flights
  let P = (4.5, 2.2)        // top-right: people
  let B = (2.0, -2.4)       // bottom-centre: bookings

  let node(pos, lbl) = {
    circle(pos, radius: 0.36, fill: rgb("#c8e6cb"), stroke: air-stroke + 1pt)
    content(pos, text(size: 13pt)[#lbl])
  }
  node(C, $C$)
  node(F, $F$)
  node(P, $P$)
  node(B, $B$)

  // F → C : src and tgt — two parallel arrows from F (top-left) to C (centre).
  bezier((F.at(0)+0.30, F.at(1)-0.20), (C.at(0)-0.30, C.at(1)+0.20),
         (0.4, 1.6), (1.0, 0.6),
         mark: (end: ">"), stroke: 1pt)
  content((0.30, 1.20), arr-text[$"src"$])
  bezier((F.at(0)+0.36, F.at(1)-0.10), (C.at(0)-0.18, C.at(1)+0.30),
         (1.0, 1.7), (1.6, 0.7),
         mark: (end: ">"), stroke: 1pt)
  content((1.30, 1.40), arr-text[$"tgt"$])

  // P → C : home  (top-right down to centre)
  line((P.at(0)-0.30, P.at(1)-0.20), (C.at(0)+0.30, C.at(1)+0.20),
       mark: (end: ">"), stroke: 1pt)
  content((3.55, 1.20), arr-text[$"home"$])

  // B → F : flight  (bottom-centre up-left to top-left, curving around C)
  bezier((B.at(0)-0.30, B.at(1)+0.20), (F.at(0)+0.20, F.at(1)-0.30),
         (-0.5, -1.2), (-1.5, 0.8),
         mark: (end: ">"), stroke: 1pt)
  content((-1.30, -0.50), arr-text[$"flight"$])

  // B → P : who  (bottom-centre up-right to top-right, curving around C)
  bezier((B.at(0)+0.30, B.at(1)+0.20), (P.at(0)-0.20, P.at(1)-0.30),
         (4.5, -1.2), (5.5, 0.8),
         mark: (end: ">"), stroke: 1pt)
  content((5.30, -0.50), arr-text[$"who"$])
}))

= A sample instance

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

= Two natural functors out of `Air`

== `π : Air → Gr` — flight network

The sub-schema `(F, C)` with the two arrows `src, tgt : F → C` is
exactly `Gr`, with `Flight` playing `E` and `City` playing `V`.  The
inclusion `Gr ↪ Air` has a left adjoint *forgetful* functor
`π : Air → Gr` that drops `Person` and `Booking`.

For our sample instance, `Δ_π` of any `Gr`-instance pulls back to an
`Air`-instance with `P = B = ∅`; `Σ_π` of an `Air`-instance forgets
the `P, B` tables and keeps `(C, F)` — i.e. just the directed-graph
"flight network" of who-flies-where.

== `! : Air → 1` — counts

As in `migration_terminal`, collapsing everything to the terminal
category `1` turns `Σ_!` into the orbit/connected-component set,
`Π_!` into the limit (compatible families across all five arrows),
and `Δ_!` into the trivial flat instance.  For the sample data:

- `Σ_!`(`Air`) — connected components of the bipartite-ish graph on
  `C ⊔ F ⊔ P ⊔ B` glued by the five arrows.  Here everything is one
  connected blob, so the orbit set has size 1.
- `Π_!`(`Air`) — sections / fixed points.  None: every arrow goes
  between different objects, so there's nothing to "fix".

= Where the data comes from

The five edge lines in the schema-data block at the top of this typst
source are the source of truth.  `Schema.lean`'s `schema_pres!` and
`migrate.py`'s `parse_schema_block` both consume them; the diagram
above is hand-laid-out CeTZ.  Adding a new arrow means: add one line
to the block, redraw, recompile.
