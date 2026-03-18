#!/usr/bin/env python3
"""
Populate ~/.cache/tv/osquery.duckdb with osquery table metadata.

Creates:
  - listing table (name, safety, rows, description) — for tv folder view
  - one view per osquery table in osq schema with COMMENT ON COLUMN for descriptions

Column descriptions are queryable via: duckdb_columns() WHERE schema_name='osq'

Dependencies: python3 (stdlib only), duckdb CLI, osqueryi, curl
"""

import json
import os
import subprocess
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

DANGEROUS_TABLES = [
    "hash", "file", "augeas", "yara", "curl", "curl_certificate",
    "magic", "device_file", "carves", "suid_bin",
    "file_events", "process_events", "process_file_events", "socket_events",
    "hardware_events", "selinux_events", "seccomp_events", "syslog_events",
    "user_events", "apparmor_events",
]

SCHEMA_URL_BASE = (
    "https://raw.githubusercontent.com/osquery/osquery-site/"
    "source/src/data/osquery_schema_versions/"
)

CACHE_DIR = Path.home() / ".cache" / "tv"
SCHEMA_CACHE = CACHE_DIR / "osquery_schema.json"
DUCKDB_PATH = CACHE_DIR / "osquery.duckdb"
CACHE_MAX_AGE = 24 * 3600  # 24 hours for row counts
SCHEMA_MAX_AGE = 30 * 24 * 3600  # 30 days for schema

# osquery type → DuckDB type
TYPE_MAP = {
    "TEXT": "VARCHAR", "INTEGER": "BIGINT", "BIGINT": "BIGINT",
    "DOUBLE": "DOUBLE", "BLOB": "BLOB", "UNSIGNED_BIGINT": "UBIGINT",
}


def esc(s):
    """Escape single quotes for SQL."""
    return s.replace("'", "''")


def duckdb_exec(sql):
    """Run SQL against the persistent DuckDB via CLI."""
    r = subprocess.run(
        ["duckdb", str(DUCKDB_PATH), "-c", sql],
        capture_output=True, text=True, timeout=60,
    )
    if r.returncode != 0:
        print(f"duckdb error: {r.stderr.strip()}", file=sys.stderr)
    return r


def duckdb_json(sql):
    """Run SQL and return parsed JSON output, or None on failure."""
    try:
        r = subprocess.run(
            ["duckdb", str(DUCKDB_PATH), "-json", "-c", sql],
            capture_output=True, text=True, timeout=10,
        )
        if r.returncode != 0:
            return None
        return json.loads(r.stdout)
    except Exception:
        return None


def osqueryi_json(sql, timeout_sec=5):
    """Run osqueryi --json and return parsed JSON, or None on failure."""
    try:
        r = subprocess.run(
            ["osqueryi", "--json", sql],
            capture_output=True, text=True, timeout=timeout_sec,
        )
        if r.returncode != 0:
            return None
        return json.loads(r.stdout)
    except (subprocess.TimeoutExpired, json.JSONDecodeError, FileNotFoundError):
        return None


def get_table_names():
    """Get all osquery table names."""
    rows = osqueryi_json(
        "SELECT name FROM osquery_registry WHERE registry='table' ORDER BY name"
    )
    if not rows:
        return []
    return [r["name"] for r in rows if "name" in r]


def get_osquery_version():
    """Get osquery version string (e.g. '5.21.0')."""
    try:
        r = subprocess.run(
            ["osqueryi", "--version"], capture_output=True, text=True, timeout=5
        )
        return r.stdout.strip().split()[-1]
    except Exception:
        return "5.21.0"


def download_schema(version):
    """Download osquery schema JSON from GitHub, cache locally."""
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    if SCHEMA_CACHE.exists():
        age = time.time() - SCHEMA_CACHE.stat().st_mtime
        if age < SCHEMA_MAX_AGE:
            try:
                return json.loads(SCHEMA_CACHE.read_text())
            except json.JSONDecodeError:
                pass
    url = f"{SCHEMA_URL_BASE}{version}.json"
    try:
        r = subprocess.run(
            ["curl", "-sf", "-o", str(SCHEMA_CACHE), url],
            capture_output=True, timeout=15,
        )
        if r.returncode != 0:
            return None
        return json.loads(SCHEMA_CACHE.read_text())
    except Exception:
        return None


def count_table(name):
    """Count rows for a single table with timeout."""
    try:
        r = subprocess.run(
            ["timeout", "-k", "1", "2", "osqueryi", "--json",
             f"SELECT count(*) as n FROM {name}"],
            capture_output=True, text=True, timeout=5,
        )
        if r.returncode != 0:
            return (name, None)
        rows = json.loads(r.stdout)
        if rows and "n" in rows[0]:
            return (name, int(rows[0]["n"]))
        return (name, None)
    except Exception:
        return (name, None)


def count_all_tables(names):
    """Count rows for all safe tables in parallel."""
    safe = [n for n in names if n not in DANGEROUS_TABLES]
    counts = {}
    with ThreadPoolExecutor(max_workers=min(32, len(safe) or 1)) as pool:
        futures = {pool.submit(count_table, name): name for name in safe}
        for future in as_completed(futures):
            name, n = future.result()
            if n is not None:
                counts[name] = n
    return counts


def populate_views(schema):
    """Create a typed stub view per osquery table with COMMENT ON COLUMN for descriptions.
    Stub views define column types (used by runEnter for TRY_CAST) and store comments
    (used by status bar and meta enrich via duckdb_columns())."""
    stmts = ["CREATE SCHEMA IF NOT EXISTS osq"]
    # Drop existing views first
    existing = duckdb_json(
        "SELECT table_name FROM duckdb_views() WHERE schema_name='osq'"
    )
    if existing:
        for row in existing:
            stmts.append(f"DROP VIEW IF EXISTS osq.\"{row['table_name']}\"")

    comments = []
    for entry in schema:
        tname = entry.get("name", "")
        cols = entry.get("columns", [])
        if not tname or not cols:
            continue
        # Build: CREATE VIEW osq."tname" AS SELECT NULL::TYPE AS "col1", ...
        col_defs = []
        for col in cols:
            cn = col.get("name", "")
            ct = TYPE_MAP.get(col.get("type", "TEXT").upper(), "VARCHAR")
            col_defs.append(f'NULL::{ct} AS "{cn}"')
        stmts.append(f'CREATE VIEW osq."{tname}" AS SELECT {", ".join(col_defs)}')
        # Collect COMMENT statements
        for col in cols:
            cn = col.get("name", "")
            cd = col.get("description", "")
            if cd:
                comments.append(f"COMMENT ON COLUMN osq.\"{tname}\".\"{cn}\" IS '{esc(cd)}'")

    # Execute in chunks (views first, then comments)
    chunk_size = 100
    for i in range(0, len(stmts), chunk_size):
        duckdb_exec("; ".join(stmts[i:i + chunk_size]))
    for i in range(0, len(comments), chunk_size):
        duckdb_exec("; ".join(comments[i:i + chunk_size]))


def check_cached_counts():
    """Check if existing listing row counts are fresh enough. Return dict or None."""
    rows = duckdb_json(
        "SELECT name, rows, updated_at FROM listing WHERE rows IS NOT NULL"
    )
    if not rows:
        return None
    ts = rows[0].get("updated_at", 0)
    if (time.time() - float(ts)) > CACHE_MAX_AGE:
        return None
    return {r["name"]: int(r["rows"]) for r in rows}


def main():
    names = get_table_names()
    if not names:
        print("No osquery tables found", file=sys.stderr)
        sys.exit(1)

    CACHE_DIR.mkdir(parents=True, exist_ok=True)

    # Schema: download and create views with column comments
    version = get_osquery_version()
    schema = download_schema(version)
    desc_map = {}
    if schema:
        desc_map = {e["name"]: e.get("description", "") for e in schema if "name" in e}
        populate_views(schema)

    # Row counts: check if existing listing is fresh enough
    counts = check_cached_counts()
    if counts is None:
        counts = count_all_tables(names)

    # Build listing table via temp JSON + read_json_auto
    now = time.time()
    listing = []
    for name in names:
        listing.append({
            "name": name,
            "safety": "input-required" if name in DANGEROUS_TABLES else "safe",
            "rows": counts.get(name),
            "description": desc_map.get(name, ""),
            "updated_at": now,
        })
    tmp = CACHE_DIR / "tmp_listing.json"
    tmp.write_text(json.dumps(listing))
    duckdb_exec(
        f"DROP TABLE IF EXISTS listing;"
        f"CREATE TABLE listing AS SELECT * FROM read_json_auto('{tmp}')"
    )
    try:
        tmp.unlink()
    except OSError:
        pass

    print(f"Populated {DUCKDB_PATH}: {len(names)} tables", file=sys.stderr)


if __name__ == "__main__":
    main()
