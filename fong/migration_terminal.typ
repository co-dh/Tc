// migration_terminal.typ — companion to migration.typ.
//
// Where migration.typ does Fong's running example F : Gr → DDS (§3.11),
// this file does the §3.4.4 special case: the unique functor
// !: C → 1 from any category C to the terminal category 1.
//
// We pick C := DDS, so:
//   - 1   has a single object * and only the identity on it.
//   - !   sends the DDS-object S to *, and the DDS-arrow `next` to id_*
//         (and the implicit identity on S to id_*).
//
// The migration triple Σ_! ⊣ Δ_! ⊣ Π_! becomes:
//   Δ_! : Set → DDS-Set       a set X turns into a DDS with state X
//                              and next = id_X (every state is fixed)
//   Σ_! : DDS-Set → Set       connected components / orbits of next
//   Π_! : DDS-Set → Set       fixed points of next (states with next s = s)
//
// Compile:   typst compile fong/migration_terminal.typ fong/migration_terminal.pdf

/*  schema-data — same parser as migration.typ; reuse the mermaid edge
    format inside this typst block-comment.

%% id: DDS
S -- next --> S

%% id: One
star -- ! --> star

%% id: Bang
S    -- ! --> star
next -- ! --> id
*/

#import "@preview/cetz:0.3.4": canvas, draw

#set page(width: 17cm, height: auto, margin: 1.5cm)
#set text(font: "DejaVu Sans", size: 11pt)
#set heading(numbering: "1.")
#show raw.where(block: true): set block(
  stroke: 0.5pt + gray, inset: 8pt, radius: 3pt, width: 100%)
#show link: set text(fill: blue.darken(20%))

#set raw(syntaxes: ("q.sublime-syntax", "prql.sublime-syntax"))

= Schema migration: DDS → 1   (Fong §3.4.4)

The *terminal category* `1` has exactly one object — call it `*` — and
exactly one arrow on it (the identity).  For any category `C` there is
a unique functor `!: C → 1` collapsing every object of `C` to `*` and
every arrow to `id`#sub[`*`].

Specialising `C := DDS` gives the picture below.  `DDS` (top, orange)
has its state object `S` and its arrow `next : S → S`; `1` (bottom,
purple) has only `*` and its identity.  The functor `!` is drawn as
dashed grey arrows: the *object map* `S ↦ *` runs along the left edge,
and the *edge map* `next ↦ id`#sub[`*`] runs from the apex of the
`next`-loop in DDS to the apex of the identity loop in `1`.

= The diagram

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

  // ── DDS panel (top) ──
  rect((-1.4, -1.3), (5.4, 1.6),
       fill: dds-fill, stroke: dds-stroke + 1pt, radius: 0.18)
  content((-0.4, 1.30), text(weight: "bold", fill: dds-stroke, size: 13pt)[DDS])

  let S = (2.0, 0.2)
  circle(S, radius: 0.40, fill: rgb("#ffe6cc"), stroke: dds-stroke + 1pt)
  content(S, text(size: 13pt)[$S$])

  // id (left) and next (right) self-loops on S
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

  // ── 1 panel (bottom) ──
  rect((-1.4, -4.6), (5.4, -1.8),
       fill: one-fill, stroke: one-stroke + 1pt, radius: 0.18)
  content((-0.5, -2.10), text(weight: "bold", fill: one-stroke, size: 13pt)[1])

  let star = (2.0, -3.2)
  circle(star, radius: 0.40, fill: rgb("#d5c2e8"), stroke: one-stroke + 1pt)
  content(star, text(size: 14pt, weight: "bold")[$ast$])

  // identity loop on * — placed on the RIGHT, aligned under `next` above,
  // so the `! : next ↦ id_*` arrow is nearly vertical.
  let one-id-apex = (3.15, -3.2)
  let one-id-label = (one-id-apex.at(0) + 0.55, one-id-apex.at(1))
  bezier((star.at(0)+0.34, star.at(1)-0.22), (star.at(0)+0.34, star.at(1)+0.22),
         (one-id-apex.at(0)-0.10, star.at(1)-0.55), (one-id-apex.at(0)-0.10, star.at(1)+0.55),
         mark: (end: ">"), stroke: 1pt)
  content(one-id-label, arr-text2[$"id"_ast$])

  // ── !: object map  S ↦ *  (centre, vertical) ──
  line((S.at(0), S.at(1)-0.40), (star.at(0), star.at(1)+0.40),
       mark: (end: ">"), stroke: f-stroke)
  content((S.at(0) + 0.20, (S.at(1)+star.at(1))/2), f-text[$!$])

  // ── !: edge map  next ↦ id_*  (right, vertical from next-apex
  //     down to id_*-apex) ──
  line((nx-apex.at(0), nx-apex.at(1) - 0.55),
       (one-id-apex.at(0), one-id-apex.at(1) + 0.55),
       mark: (end: ">"), stroke: f-stroke)
  content((nx-apex.at(0) + 0.20, (nx-apex.at(1)+one-id-apex.at(1))/2), f-text[$!$])
}))

The picture says: every DDS-thing collapses.  `S` lands on `*`; the
non-trivial arrow `next` lands on the only arrow `id`#sub[`*`] there
is.  The implicit identity on `S` also lands on `id`#sub[`*`] (that's
forced by functoriality and not drawn).

= The migration triple

For `F = !` the three legs of `Σ_! ⊣ Δ_! ⊣ Π_!` collapse to familiar
constructions on a DDS.

== Δ#sub[`!`] : Set → DDS-Set   (a set, with `next = id`)

Pre-composition by `!` turns a set $X = G(ast)$ into a DDS-instance
with state set $X$ and `next $= id_X$`: every state is its own
successor — a flat set, dynamically.

```q
delta:{[X] ([] s:X; next_:X)}
```

```prql
let DDS_delta = (from X | select { s = x, next = x })
```

== Σ#sub[`!`] : DDS-Set → Set   (orbits of `next`)

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

== Π#sub[`!`] : DDS-Set → Set   (fixed points of `next`)

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

= Round-trip with the §3.11 example

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
