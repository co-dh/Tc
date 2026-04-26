// migration.typ — single source of truth for the schema-migration
// example.  Carries the schema data, the prose, and the rendering all
// in one place.
//
// Compile:   typst compile fong/migration.typ fong/migration.pdf
//
// Three other consumers read this file:
//   • migration.lean       — at elab time via mermaid_pres! in Mermaid.lean
//   • migrate.py           — emits PRQL or q for the migration triple
//   • the human reader     — via the rendered PDF

/*  schema-data — parsed by both migrate.py and Mermaid.lean.
    Format mirrors the original mermaid block: a section is opened by
    a `%% id: <NAME>` comment and runs until the next such marker or
    the closing block-comment fence.  Inside, only edge lines of the
    form
        <src> -- <label> --> <tgt>
    are read; everything else is ignored.

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
*/

#import "@preview/cetz:0.3.4": canvas, draw
#import "@preview/ilm:1.4.1": *

#show: ilm.with(
  title: [Schema migration: \ #text(size: 0.8em)[*Gr* → *DDS*]],
  author: "Fong & Spivak — Seven Sketches §3.11",
  date: datetime(year: 2026, month: 4, day: 26),
  abstract: [
    A worked example of categorical schema migration: the schema
    *Gr* (a directed multigraph) and *DDS* (a discrete dynamical
    system), the migration functor `F : Gr → DDS`, and the three legs
    of the migration triple Σ#sub[`F`] ⊣ Δ#sub[`F`] ⊣ Π#sub[`F`].
    The Lean file `migration.lean` and the Python script `migrate.py`
    both consume the schema data from a comment block at the top of
    this typst source.],
  bibliography: none,
  figure-index: (enabled: false),
  table-index: (enabled: false),
  listing-index: (enabled: false),
)

// Code-block style and our custom q/PRQL syntaxes.
#show raw.where(block: true): set block(
  stroke: 0.5pt + gray, inset: 8pt, radius: 3pt, width: 100%)
#set raw(syntaxes: ("q.sublime-syntax", "prql.sublime-syntax"))

= Source-of-truth file

This `.typ` file is the single source of truth for the *schemas*
(`Gr`, `DDS`) and the *migration functor* `F : Gr → DDS`.  The schema
data sits in a typst comment at the top of this source so two
non-typst consumers can parse it directly:

+ `migration.lean` — at elaboration time, via `mermaid_pres!` in
  `Mermaid.lean` — to build the `Gr` and `DDS` categories and check
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

= The diagram

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

  // s curves up, t curves down.  Each label sits at the arc's apex
  // (just above the s-arc, just below the t-arc) so it visibly
  // labels the arrow it names.  The F edge-map arrows below are
  // drawn as diagonals from these apexes, not as vertical lines.
  let s-apex = (2.0, 0.85)
  let t-apex = (2.0, -0.45)
  let s-label = (s-apex.at(0), s-apex.at(1) + 0.40)   // above s-arc apex
  let t-label = (t-apex.at(0), t-apex.at(1) - 0.40)   // below t-arc apex
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
  let id-label = (id-apex.at(0) - 0.45, id-apex.at(1))   // left of id loop
  let nx-label = (nx-apex.at(0) + 0.55, nx-apex.at(1))   // right of next loop
  bezier((S.at(0)-0.34, S.at(1)+0.22), (S.at(0)-0.34, S.at(1)-0.22),
         (id-apex.at(0)+0.10, S.at(1)+0.55), (id-apex.at(0)+0.10, S.at(1)-0.55),
         mark: (end: ">"), stroke: 1pt)
  content(id-label, arr-text2[$"id"$])
  bezier((S.at(0)+0.34, S.at(1)-0.22), (S.at(0)+0.34, S.at(1)+0.22),
         (nx-apex.at(0)-0.10, S.at(1)-0.55), (nx-apex.at(0)-0.10, S.at(1)+0.55),
         mark: (end: ">"), stroke: 1pt)
  content(nx-label, arr-text2[$"next"$])

  // ── F: object map (E↦S, V↦S) — drawn at the panel-corner ends, so
  // the middle stays clear for the edge-map arrows.  Arrowheads aim
  // at the LEFT and RIGHT edges of `S` (not its centre) so they
  // don't visually merge with the s→id / t→next arrows below.
  line((E.at(0), E.at(1)-0.34), (S.at(0)-0.40, S.at(1)+0.30),
       mark: (end: ">"), stroke: f-stroke)
  content((-0.10, -2.10), f-text[$F$])
  line((V.at(0), V.at(1)-0.34), (S.at(0)+0.40, S.at(1)+0.30),
       mark: (end: ">"), stroke: f-stroke)
  content((4.10, -2.10), f-text[$F$])

  // ── F: edge map (s↦id, t↦next) — diagonals from the s/t labels
  // (sitting on their arcs) down to the id/next labels (sitting at
  // the loop apexes).  The arrow tips touch the destination label.
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

= Generated PRQL and q

The blocks below are *not* hand-written: each is the verbatim output
of `migrate.py` reading the schema-data block at the top of this file
and emitting code for its target.  Re-run before compiling typst:

```sh
./migrate.py            migration.typ /dev/null > triple.prql
./migrate.py --target=q migration.typ            > triple.q
typst compile fong/migration.typ fong/migration.pdf
```

== PRQL  (`triple.prql`)

The PRQL workflow assumes the input DuckDB has a table `DDS(s, next)`
for the DDS direction, or tables `V(id)` / `E(id, s, t)` for the Gr
direction.  The Π_F leg is emitted as a commented raw-SQL recursive
CTE template — pure PRQL has no recursion.

#raw(read("triple.prql"), lang: "prql", block: true)

=== Round-trip check (PRQL)

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

== q  (`triple.q`)

The DDS arrow `next` becomes column `next_` in q (the bare name is a
built-in in `.q`).  Type comments on each function spell out the
expected shapes of its inputs; the `pi` definition uses q's scan
iterator (`\`) to apply `step` $N$ times starting from each vertex.

#raw(read("triple.q"), lang: "q", block: true)

=== Round-trip check (q)

```sh
./migrate.py --target=q migration.typ > triple.q
q triple.q
q)DDS:([] s:0 1 2 3 4 5 6; next_:3 3 4 4 4 6 5)
q)show sigma delta DDS         / recovers DDS
q)DDS~`s xasc sigma delta DDS  / 1b — round-trip succeeds
q)show pi[delta DDS;4]         / depth-4 trajectories from each vertex
```
