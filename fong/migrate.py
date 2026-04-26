#!/usr/bin/env python3
"""migrate.py — generate PRQL or q for Fong's schema migration triple.

Reads a single source-of-truth Typst file (`migration.typ`) whose
top-of-file block comment carries the schemas (Gr, DDS) and the
migration functor (F) in the legacy mermaid edge format, and emits
Δ_F, Σ_F, Π_F as either PRQL (for DuckDB) or q (for kdb+).

Usage: migrate.py [--target=prql|q] <migration.typ> [<database.duckdb>]

The DuckDB path is only consulted when --target=prql, to introspect
the DDS table's columns.  For q the columns come from the schema's
arrow names (with q reserved words like `next` suffixed with `_`).
"""

from __future__ import annotations

import re
import shutil
import subprocess
import sys
from pathlib import Path


def parse_schema_block(md: str, block_id: str) -> list[tuple[str, str, str]]:
    """Extract `(src, label, tgt)` edges from the section tagged `%% id: <block_id>`.

    A section runs from its `%% id:` marker until the next `%% id:` marker, the
    closing ``` mermaid fence, or the closing `*/` block-comment fence — whichever
    comes first.  This lets us host the same mermaid-style schema data inside a
    typst `/* ... */` comment with no parser changes worth speaking of."""
    start_re = re.compile(rf'^\s*%% id: {re.escape(block_id)}\s*$', re.MULTILINE)
    m = start_re.search(md)
    if not m:
        return []
    rest = md[m.end():]
    end_re = re.compile(r'^\s*```|^\s*%% id: |\*/', re.MULTILINE)
    em = end_re.search(rest)
    section = rest[:em.start()] if em else rest
    edges = []
    for line in section.split('\n'):
        line = line.strip()
        if not line or line.startswith('%%') or '-->' not in line:
            continue
        e = re.match(r'(\S+)\s+--\s+(.+?)\s+-->\s+(\S+)', line)
        if e:
            edges.append((e.group(1), e.group(2).strip(), e.group(3)))
    return edges


def duckdb_describe(db_path: str, table: str) -> list[str] | None:
    """Run `DESCRIBE <table>` via the `duckdb` CLI and return the column names."""
    if shutil.which('duckdb') is None:
        return None
    try:
        out = subprocess.run(
            ['duckdb', db_path, '-csv', '-c', f'SELECT column_name FROM (DESCRIBE {table})'],
            capture_output=True, text=True, check=True, timeout=5)
        rows = [l.strip() for l in out.stdout.split('\n') if l.strip()]
        return rows[1:] if rows and rows[0] == 'column_name' else rows
    except subprocess.CalledProcessError:
        return None


# q has a small set of identifiers that can't be used as column or
# variable names (built-ins in the .q namespace).  Anything used as a
# Gr-arrow or DDS-arrow name in migration.typ gets `_`-suffixed when
# emitted for q so the script parses.
Q_RESERVED = {
    'next', 'first', 'last', 'count', 'each', 'over', 'scan',
    'do', 'while', 'if', 'select', 'from', 'where', 'by',
    'exec', 'update', 'delete', 'in', 'within', 'like',
    'vs', 'sv', 'asc', 'desc', 'distinct', 'cross',
}


def q_id(name: str) -> str:
    """Map a schema-arrow name to a q identifier (column or variable)."""
    return name + '_' if name in Q_RESERVED else name


def emit_prql(gr_arrows, gr_objs, obj_map, edge_map, table_name: str = 'DDS') -> None:
    """Emit PRQL for the migration triple to stdout."""

    # ── Δ_F : DDS instance → Gr instance ────────────────────────────
    print(f"# ────────────────────────────────────────────")
    print(f"# Δ_F : DDS instance → Gr instance")
    print(f"# Reads from table `{table_name}`.")
    print(f"# ────────────────────────────────────────────")
    for obj in sorted(gr_objs):
        f_obj = obj_map.get(obj, '?')
        out_arrows = [(lbl, tgt) for lbl, (src, tgt) in gr_arrows.items() if src == obj]
        if not out_arrows:
            # "vertex" object — no outgoing Gr-arrows, just the state set.
            print(f"# Gr object {obj} ↦ DDS object {f_obj}: vertex set.")
            print(f"let {obj} = (from {table_name} | select {{ id = s }} | group {{id}} (take 1))")
        else:
            # "edge" object — one projection per outgoing Gr-arrow.
            cols = ['id = s']
            for arrow, _tgt_obj in out_arrows:
                path = edge_map.get(arrow, [])
                if not path:
                    cols.append(f"{arrow} = s")           # F(arrow) = identity
                elif len(path) == 1:
                    cols.append(f"{arrow} = {path[0]}")   # F(arrow) = single-step
                else:
                    cols.append(f"# TODO multi-step path {arrow} ↦ {path}")
            print(f"# Gr object {obj} ↦ DDS object {f_obj}: edge set with arrow projections.")
            print(f"let {obj} = (from {table_name} | select {{ {', '.join(cols)} }})")
        print()

    # ── Σ_F : Gr instance → DDS instance ────────────────────────────
    print(f"# ────────────────────────────────────────────")
    print(f"# Σ_F : Gr instance → DDS instance  (left Kan extension)")
    print(f"# Free DDS on G: states = vertices, next absorbs out-edges.")
    print(f"# Conditional on each vertex having ≤1 out-edge in G.")
    print(f"# ────────────────────────────────────────────")
    edge_obj = next((o for o in sorted(gr_objs)
                     if any(src == o for src, _ in gr_arrows.values())), None)
    if edge_obj:
        # Find the Gr arrows mapped to identity vs to next, in the schema sense.
        id_arrow = next((a for a, p in edge_map.items() if not p), None)
        next_arrow = next((a for a, p in edge_map.items() if p), None)
        if id_arrow and next_arrow:
            print(f"# Read state from arrow `{id_arrow}` (which F sends to identity)")
            print(f"# and next from arrow `{next_arrow}` (which F sends to a step).")
            print(f"let DDS_sigma = (from {edge_obj} | select {{ s = {id_arrow}, next = {next_arrow} }})")
        else:
            print(f"# (Could not determine identity/step arrows from edge map {edge_map})")
    else:
        print(f"# (No Gr object has outgoing arrows — Σ_F is trivial.)")
    print()

    # ── Π_F : Gr instance → DDS instance ────────────────────────────
    print(f"# ────────────────────────────────────────────")
    print(f"# Π_F : Gr instance → DDS instance  (right Kan extension)")
    print(f"# Set of (infinite) trajectories in G.  Pure PRQL has no recursion;")
    print(f"# below is a finite-depth unfold as a recursive CTE in raw SQL.")
    print(f"# ────────────────────────────────────────────")
    if edge_obj:
        print(f"# WITH RECURSIVE traj(state, step) AS (")
        print(f"#   SELECT id, 0 FROM {edge_obj}")
        print(f"#   UNION ALL")
        print(f"#   SELECT e.tgt, t.step + 1 FROM traj t JOIN {edge_obj} e ON e.id = t.state WHERE t.step < N")
        print(f"# )")


def emit_q(gr_arrows, gr_objs, obj_map, edge_map, table_name: str = 'DDS') -> None:
    """Emit q (kdb+) code for the migration triple.

    Reserved-word arrows get a trailing `_` in the generated q
    identifiers — e.g. the DDS arrow `next` becomes column `next_`.
    Otherwise the q text mirrors the PRQL structure: one function per
    leg of the triple, parameterised by the schema instance."""

    nxt_arrow = next((a for a, p in edge_map.items() if p), None)
    id_arrow = next((a for a, p in edge_map.items() if not p), None)
    edge_obj = next((o for o in sorted(gr_objs)
                     if any(src == o for src, _ in gr_arrows.values())), None)

    # ── Δ_F : DDS → Gr ────────────────────────────────────────
    print(f"/ ────────────────────────────────────────────")
    print(f"/ Δ_F : DDS instance → Gr instance")
    print(f"/ Takes a `{table_name}` table, returns a dict of Gr-tables.")
    print(f"/ ────────────────────────────────────────────")
    body_lines = []
    for obj in sorted(gr_objs):
        f_obj = obj_map.get(obj, '?')
        out_arrows = [(lbl, tgt) for lbl, (src, tgt) in gr_arrows.items() if src == obj]
        if not out_arrows:
            body_lines.append(f"  / Gr {obj} ↦ DDS {f_obj}: vertex set")
            body_lines.append(f"  {obj}:([] id:asc distinct D`s);")
        else:
            cols = ['id:D`s']
            for arrow, _ in out_arrows:
                path = edge_map.get(arrow, [])
                if not path:                          # F(arrow) = identity
                    cols.append(f"{q_id(arrow)}:D`s")
                elif len(path) == 1:                  # F(arrow) = single-step
                    cols.append(f"{q_id(arrow)}:D`{q_id(path[0])}")
            body_lines.append(f"  / Gr {obj} ↦ DDS {f_obj}: edge set")
            body_lines.append(f"  {obj}:([] {'; '.join(cols)});")
    keys = ''.join(f'`{o}' for o in sorted(gr_objs))
    vals = ';'.join(sorted(gr_objs))
    print(f"delta:{{[D]")
    for ln in body_lines:
        print(ln)
    print(f"  {keys}!({vals})}}")
    print()

    # ── Σ_F : Gr → DDS ────────────────────────────────────────
    print(f"/ ────────────────────────────────────────────")
    print(f"/ Σ_F : Gr instance → DDS instance  (free DDS on G, left Kan)")
    print(f"/ Conditional on each Gr-vertex having ≤1 out-edge.")
    print(f"/ ────────────────────────────────────────────")
    if edge_obj and id_arrow and nxt_arrow:
        # DDS column names: `s` for the state, q_id(dds_step) for the
        # step arrow (e.g. q_id("next") = "next_").
        dds_step = edge_map[nxt_arrow][0]
        print(f"sigma:{{[G]")
        print(f"  E:G`{edge_obj};")
        print(f"  / Gr arrow `{id_arrow} ↦ identity, Gr arrow `{nxt_arrow} ↦ DDS step `{dds_step}")
        print(f"  ([] s:E`{id_arrow}; {q_id(dds_step)}:E`{nxt_arrow})}}")
    print()

    # ── Π_F : Gr → DDS ────────────────────────────────────────
    print(f"/ ────────────────────────────────────────────")
    print(f"/ Π_F : Gr instance → DDS instance  (trajectories, right Kan)")
    print(f"/ Length-N trajectory from each Gr-vertex.  Assumes function-like")
    print(f"/ (each vertex has ≤1 out-edge); for general G the result is a")
    print(f"/ tree of paths, not implemented here.")
    print(f"/ ────────────────────────────────────────────")
    if edge_obj and id_arrow and nxt_arrow:
        ida, nxa = q_id(id_arrow), q_id(nxt_arrow)
        print(f"pi:{{[G;N]")
        print(f"  E:G`{edge_obj};")
        print(f"  step:exec first {nxa} by {ida} from E;")
        print(f"  verts:asc distinct (E`{ida}),E`{nxa};")
        print(f"  ([] start:verts; traj:{{[s;N;v] N {{x[y]}}[s]\\v}}[step;N] each verts)}}")
    print()


def main() -> None:
    args = sys.argv[1:]
    target = 'prql'
    rest = []
    for a in args:
        if a.startswith('--target='):
            target = a.split('=', 1)[1]
        else:
            rest.append(a)
    if target not in ('prql', 'q'):
        print(f"Unknown target {target!r} (expected prql or q)", file=sys.stderr)
        sys.exit(1)
    if target == 'prql':
        if len(rest) != 2:
            print(__doc__, file=sys.stderr)
            sys.exit(1)
        md_path, db_path = rest
    else:
        if len(rest) != 1:
            print(__doc__, file=sys.stderr)
            sys.exit(1)
        md_path, db_path = rest[0], None
    md = Path(md_path).read_text()

    gr_edges = parse_schema_block(md, 'Gr')
    dds_edges = parse_schema_block(md, 'DDS')
    f_edges = parse_schema_block(md, 'F')
    if not (gr_edges and dds_edges and f_edges):
        print(f"Missing one of Gr / DDS / F schema blocks in {md_path}", file=sys.stderr)
        sys.exit(1)

    gr_objs = {n for e in gr_edges for n in (e[0], e[2])}
    gr_arrows = {e[1]: (e[0], e[2]) for e in gr_edges}
    dds_objs = {n for e in dds_edges for n in (e[0], e[2])}
    dds_arrows = {e[1]: (e[0], e[2]) for e in dds_edges}

    obj_map: dict[str, str] = {}
    edge_map: dict[str, list[str]] = {}
    for src, _, tgt in f_edges:
        if src in gr_objs:
            obj_map[src] = tgt
        elif src in gr_arrows:
            edge_map[src] = [] if tgt == 'id' else [tgt]

    com = '#' if target == 'prql' else '/'
    print(f"{com} migration.typ → {target}")
    print(f"{com} ────────────────────────")
    print(f"{com} Gr  objects = {sorted(gr_objs)}")
    print(f"{com} Gr  arrows  = {gr_arrows}")
    print(f"{com} DDS objects = {sorted(dds_objs)}")
    print(f"{com} DDS arrows  = {dds_arrows}")
    print(f"{com} F object map = {obj_map}")
    print(f"{com} F edge   map = {edge_map}")
    print()

    if target == 'prql':
        cols = duckdb_describe(db_path, 'DDS')
        if cols is None:
            print(f"# WARN: could not introspect DDS table in {db_path} (duckdb CLI missing or table absent).")
        else:
            print(f"# DDS instance table `{db_path}` columns: {cols}")
        print()
        emit_prql(gr_arrows, gr_objs, obj_map, edge_map, table_name='DDS')
    else:
        emit_q(gr_arrows, gr_objs, obj_map, edge_map, table_name='DDS')


if __name__ == '__main__':
    main()
