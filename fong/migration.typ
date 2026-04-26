// Schema-migration diagram for fong/migration.md.
//
// Shows:
//   • Gr  — schema with V, E, arrows s,t : E→V (top, blue panel)
//   • DDS — schema with S, arrows id, next : S→S (bottom, orange panel)
//   • F   — functor Gr → DDS, drawn as dashed grey arrows.
//           Object map: V↦S, E↦S.
//           Edge map:   s↦id, t↦next — drawn from each Gr arrow's
//           midpoint to the corresponding DDS arrow's midpoint.

#import "@preview/cetz:0.3.4": canvas, draw

#set page(width: auto, height: auto, margin: 0.4cm)
#set text(font: "DejaVu Sans", size: 12pt)

#canvas(length: 1.0cm, {
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

  // s curves up, t curves down — both from E to V
  let s-apex = (2.0, 0.85)
  let t-apex = (2.0, -0.45)
  bezier((E.at(0)+0.34, E.at(1)+0.10), (V.at(0)-0.34, V.at(1)+0.10),
         (1.2, s-apex.at(1)+0.05), (2.8, s-apex.at(1)+0.05),
         mark: (end: ">"), stroke: 1pt)
  content((s-apex.at(0), s-apex.at(1)+0.40), arr-text[$s$])
  bezier((E.at(0)+0.34, E.at(1)-0.10), (V.at(0)-0.34, V.at(1)-0.10),
         (1.2, t-apex.at(1)-0.05), (2.8, t-apex.at(1)-0.05),
         mark: (end: ">"), stroke: 1pt)
  content((t-apex.at(0), t-apex.at(1)-0.40), arr-text[$t$])

  // ── DDS panel (bottom) ──
  rect((-1.4, -5.6), (5.4, -2.6),
       fill: dds-fill, stroke: dds-stroke + 1pt, radius: 0.18)
  content((-0.4, -2.95), text(weight: "bold", fill: dds-stroke, size: 13pt)[DDS])

  let S = (2.0, -4.1)
  circle(S, radius: 0.40, fill: rgb("#ffe6cc"), stroke: dds-stroke + 1pt)
  content(S, text(size: 13pt)[$S$])

  let id-apex = (0.85, -4.1)
  let nx-apex = (3.15, -4.1)
  bezier((S.at(0)-0.34, S.at(1)+0.22), (S.at(0)-0.34, S.at(1)-0.22),
         (id-apex.at(0)+0.10, S.at(1)+0.65), (id-apex.at(0)+0.10, S.at(1)-0.65),
         mark: (end: ">"), stroke: 1pt)
  content((id-apex.at(0)-0.20, id-apex.at(1)), arr-text2[$"id"$])
  bezier((S.at(0)+0.34, S.at(1)-0.22), (S.at(0)+0.34, S.at(1)+0.22),
         (nx-apex.at(0)-0.10, S.at(1)-0.65), (nx-apex.at(0)-0.10, S.at(1)+0.65),
         mark: (end: ">"), stroke: 1pt)
  content((nx-apex.at(0)+0.30, nx-apex.at(1)), arr-text2[$"next"$])

  // ── F: object map (E↦S, V↦S) ──
  line((E.at(0)-0.05, E.at(1)-0.34), (S.at(0)-1.10, S.at(1)+0.32),
       mark: (end: ">"), stroke: f-stroke)
  content((-0.10, -1.85), f-text[$F$])
  line((V.at(0)+0.05, V.at(1)-0.34), (S.at(0)+1.10, S.at(1)+0.32),
       mark: (end: ">"), stroke: f-stroke)
  content((4.10, -1.85), f-text[$F$])

  // ── F: edge map (s↦id, t↦next) ──
  // From above the s label down to above the id loop apex.
  line((s-apex.at(0)-0.30, s-apex.at(1)+0.20), (id-apex.at(0)+0.20, id-apex.at(1)+0.55),
       mark: (end: ">"), stroke: f-stroke)
  content((1.05, 0.20), f-text[$F$])
  line((t-apex.at(0)+0.30, t-apex.at(1)-0.20), (nx-apex.at(0)-0.20, nx-apex.at(1)+0.55),
       mark: (end: ">"), stroke: f-stroke)
  content((2.95, -1.10), f-text[$F$])
})
