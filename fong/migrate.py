#!/usr/bin/env python3
"""migrate.py — generate PRQL for Fong's schema migration triple.

Reads a single source-of-truth markdown file with mermaid blocks for
the schemas (Gr, DDS) and the migration functor (F), plus a DuckDB
file holding a DDS instance (table `DDS` with columns matching the
DDS schema's state and arrows), and emits PRQL for Δ_F, Σ_F, Π_F.

Usage: migrate.py <migration.md> <database.duckdb>
"""

from __future__ import annotations

import re
import shutil
import subprocess
import sys
from pathlib import Path


def parse_mermaid_block(md: str, block_id: str) -> list[tuple[str, str, str]]:
    """Extract `(src, label, tgt)` edges from the section tagged `%% id: <block_id>`.

    A section runs from its `%% id:` marker until **either** the next `%% id:`
    marker or the closing ``` fence — so multiple sections can share a single
    mermaid block in the source markdown."""
    start_re = re.compile(rf'^\s*%% id: {re.escape(block_id)}\s*$', re.MULTILINE)
    m = start_re.search(md)
    if not m:
        return []
    rest = md[m.end():]
    end_re = re.compile(r'^\s*```|^\s*%% id: ', re.MULTILINE)
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


def main() -> None:
    if len(sys.argv) != 3:
        print(__doc__, file=sys.stderr)
        sys.exit(1)
    md_path, db_path = sys.argv[1], sys.argv[2]
    md = Path(md_path).read_text()

    gr_edges = parse_mermaid_block(md, 'Gr')
    dds_edges = parse_mermaid_block(md, 'DDS')
    f_edges = parse_mermaid_block(md, 'F')
    if not (gr_edges and dds_edges and f_edges):
        print(f"Missing one of Gr / DDS / F mermaid blocks in {md_path}", file=sys.stderr)
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

    print(f"# migration.md → PRQL")
    print(f"# ────────────────────────")
    print(f"# Gr  objects = {sorted(gr_objs)}")
    print(f"# Gr  arrows  = {gr_arrows}")
    print(f"# DDS objects = {sorted(dds_objs)}")
    print(f"# DDS arrows  = {dds_arrows}")
    print(f"# F object map = {obj_map}")
    print(f"# F edge   map = {edge_map}")
    print()

    cols = duckdb_describe(db_path, 'DDS')
    if cols is None:
        print(f"# WARN: could not introspect DDS table in {db_path} (duckdb CLI missing or table absent).")
    else:
        print(f"# DDS instance table `{db_path}` columns: {cols}")
    print()

    emit_prql(gr_arrows, gr_objs, obj_map, edge_map, table_name='DDS')


if __name__ == '__main__':
    main()
