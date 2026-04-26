/ migration.typ → q
/ ────────────────────────
/ Gr  objects = ['E', 'V']
/ Gr  arrows  = {'s': ('E', 'V'), 't': ('E', 'V')}
/ DDS objects = ['S']
/ DDS arrows  = {'next': ('S', 'S')}
/ F object map = {'V': 'S', 'E': 'S'}
/ F edge   map = {'s': [], 't': ['next']}

/ ────────────────────────────────────────────
/ Δ_F : DDS → Gr   (precomposition / pullback)
/
/   D :: table DDS(s:long, next_:long)
/   returns dict of Gr-tables: `E: ([] id:long, s:long, t:long) | `V: ([] id:long)
/
/ Each Gr-arrow `a out of object O reads from the column F(a)
/ of `DDS`: an arrow F maps to identity copies the state column,
/ an arrow F maps to a step copies the step's DDS column.
/ ────────────────────────────────────────────
delta:{[D]
  / Gr E ↦ DDS S: edge set (`s↦id, `t↦`next)
  E:([] id:D`s; s:D`s; t:D`next_);
  / Gr V ↦ DDS S: vertex set (no out-arrows)
  V:([] id:asc distinct D`s);
  `E`V!(E;V)}

/ ────────────────────────────────────────────
/ Σ_F : Gr → DDS   (free DDS on G; left Kan extension)
/
/   G :: dict of Gr-tables (as returned by delta)
/   returns table DDS(s:long, next_:long)
/
/ Each Gr-edge u→v in `E reads as `next`(u) = v.  Sound only when
/ each Gr-vertex has ≤1 out-edge in G — otherwise targets must be
/ identified, which would need a quotient (not done here).
/ ────────────────────────────────────────────
sigma:{[G]
  E:G`E;
  / `s↦id  (state column),  `t↦`next  (step column)
  ([] s:E`s; next_:E`t)}

/ ────────────────────────────────────────────
/ Π_F : Gr → DDS   (trajectories; right Kan extension)
/
/   G :: dict of Gr-tables;  N :: depth (long)
/   returns table (start:long; traj:list of long)
/
/ For each Gr-vertex v, traj is the length-(N+1) sequence
/   v, step v, step (step v), ..., step^N v
/ where `step :: src→tgt` is the next-vertex map read off `E.
/ Assumes function-like G (≤1 out-edge per vertex); a multi-edge
/ G would yield a tree of paths, not implemented here.
/ ────────────────────────────────────────────
pi:{[G;N]
  E:G`E;
  / step :: src-vertex → tgt-vertex  (q dict, indexable as step[v])
  step:exec first t by s from E;
  / all vertices appearing as either src or tgt of some Gr-edge
  verts:asc distinct (E`s),E`t;
  / scan: starting from each v, apply `step` N times
  ([] start:verts; traj:{[step;N;v] N {x[y]}[step]\v}[step;N] each verts)}

