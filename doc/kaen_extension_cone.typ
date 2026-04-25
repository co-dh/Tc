// Compile: typst compile doc/kaen_extension_cone.typ
//
// Cone diagram for `ConeData` from kaen_extension_cone.lean.
// The diagram triangle (X, Y, Z and arrows f, g, h) is drawn in red to
// emphasise it is the *image* of the indexing category I in the target C.
// Cone legs lx, ly, lz are blue. Apex c is black.

#import "@preview/cetz:0.3.4": canvas, draw

#set page(width: 16cm, height: auto, margin: 1.2cm)
#set text(font: "DejaVu Sans", size: 11pt)
#set heading(numbering: "1.")

= Cone over $D$ with apex $c$

#align(center, canvas(length: 1.1cm, {
  import draw: *

  // Positions
  let apex = (4, 5.2)
  let xpt  = (1, 0)
  let ypt  = (4, 2.2)
  let zpt  = (7, 0)

  let red-stroke = red + 1pt
  let red-text(body) = text(fill: red, weight: "bold", size: 12pt, body)
  let blue-stroke = blue + 1pt
  let blue-text(body) = text(fill: blue, weight: "bold", body)

  // -- apex c --
  content(apex, [#text(weight: "bold", size: 13pt)[$c$]])

  // -- diagram objects (red) --
  content(xpt, red-text[$X$])
  content(ypt, red-text[$Y$])
  content(zpt, red-text[$Z$])

  // -- diagram arrows f, g, h (red) --
  // f : X → Y  (up-right)
  line((xpt.at(0) + 0.4, xpt.at(1) + 0.2),
       (ypt.at(0) - 0.4, ypt.at(1) - 0.2),
       mark: (end: ">"), stroke: red-stroke)
  content((2.0, 1.4), red-text[$f$])
  // g : Y → Z  (down-right)
  line((ypt.at(0) + 0.4, ypt.at(1) - 0.2),
       (zpt.at(0) - 0.4, zpt.at(1) + 0.2),
       mark: (end: ">"), stroke: red-stroke)
  content((6.0, 1.4), red-text[$g$])
  // h : X → Z  (horizontal at bottom)
  line((xpt.at(0) + 0.4, xpt.at(1)),
       (zpt.at(0) - 0.4, zpt.at(1)),
       mark: (end: ">"), stroke: red-stroke)
  content((4, -0.4), red-text[$h$])

  // -- cone legs lx, ly, lz (blue) --
  // lx : c → X
  line((apex.at(0) - 0.15, apex.at(1) - 0.3),
       (xpt.at(0) + 0.2, xpt.at(1) + 0.4),
       mark: (end: ">"), stroke: blue-stroke)
  content((1.7, 3.0), blue-text[$l_x$])
  // ly : c → Y
  line((apex.at(0), apex.at(1) - 0.3),
       (ypt.at(0), ypt.at(1) + 0.3),
       mark: (end: ">"), stroke: blue-stroke)
  content((4.25, 3.7), blue-text[$l_y$])
  // lz : c → Z
  line((apex.at(0) + 0.15, apex.at(1) - 0.3),
       (zpt.at(0) - 0.2, zpt.at(1) + 0.4),
       mark: (end: ">"), stroke: blue-stroke)
  content((6.3, 3.0), blue-text[$l_z$])
}))

#text(fill: red)[Red] = the diagram $D : "Icat" => C$ (its objects $X, Y, Z$ and arrows $f, g, h$, the image of the indexing category $I$ in the target).
#text(fill: blue)[Blue] = the cone, three legs from the apex $c$ to each diagram object.

The compatibility equations (the structure's `hf, hg, hh`) say the
two paths from $c$ to each diagram object agree:

- $l_x compose D(f) = l_y$ — going $c arrow X arrow Y$ matches direct $l_y$.
- $l_y compose D(g) = l_z$ — going $c arrow Y arrow Z$ matches direct $l_z$.
- $l_x compose D(h) = l_z$ — redundant (follows from the above + functoriality).

= `ConeData` (Lean source)

#raw("structure ConeData (D : Icat ⇒ C) (c : C.Obj) where
  lx : c ⟶ D.o .X
  ly : c ⟶ D.o .Y
  lz : c ⟶ D.o .Z
  hf : lx ≫ D.f .f = ly
  hg : ly ≫ D.f .g = lz
  hh : lx ≫ D.f .h = lz", lang: "lean", block: true)
