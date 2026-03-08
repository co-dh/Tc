#!/usr/bin/env python3
"""
Populate ~/.cache/tc/osquery.duckdb with osquery table metadata.

Creates three tables:
  - listing (name, safety, rows, description) — full table list for tc folder view
  - tables  (name, description, platforms)    — from osquery-site schema JSON
  - columns (table_name, col_name, col_type, col_desc) — column-level descriptions

Dependencies: python3, duckdb (pip install duckdb)
"""

import json
import os
import subprocess
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

try:
    import duckdb
except ImportError:
    print("Error: duckdb python package required (pip install duckdb)", file=sys.stderr)
    sys.exit(1)

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

CACHE_DIR = Path.home() / ".cache" / "tc"
SCHEMA_CACHE = CACHE_DIR / "osquery_schema.json"
DUCKDB_PATH = CACHE_DIR / "osquery.duckdb"
CACHE_MAX_AGE = 24 * 3600  # 24 hours for row counts
SCHEMA_MAX_AGE = 30 * 24 * 3600  # 30 days for schema


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


def main():
    names = get_table_names()
    if not names:
        print("No osquery tables found", file=sys.stderr)
        sys.exit(1)

    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    con = duckdb.connect(str(DUCKDB_PATH))

    # Schema: download and populate tables/columns
    version = get_osquery_version()
    schema = download_schema(version)
    desc_map = {}
    if schema:
        desc_map = {e["name"]: e.get("description", "") for e in schema if "name" in e}
        con.execute("DROP TABLE IF EXISTS tables")
        con.execute("DROP TABLE IF EXISTS columns")
        con.execute("CREATE TABLE tables (name VARCHAR, description VARCHAR, platforms VARCHAR)")
        con.execute("CREATE TABLE columns (table_name VARCHAR, col_name VARCHAR, col_type VARCHAR, col_desc VARCHAR)")
        for entry in schema:
            name = entry.get("name", "")
            desc = entry.get("description", "")
            platforms = ", ".join(entry.get("platforms", []))
            con.execute("INSERT INTO tables VALUES (?, ?, ?)", [name, desc, platforms])
            for col in entry.get("columns", []):
                con.execute("INSERT INTO columns VALUES (?, ?, ?, ?)",
                            [name, col.get("name", ""), col.get("type", ""),
                             col.get("description", "")])

    # Row counts: check if existing listing is fresh enough
    counts = None
    try:
        ts = con.execute("SELECT updated_at FROM listing LIMIT 1").fetchone()
        if ts and (time.time() - ts[0]) < CACHE_MAX_AGE:
            # Existing counts are fresh, reuse them
            rows = con.execute("SELECT name, rows FROM listing WHERE rows IS NOT NULL").fetchall()
            counts = {r[0]: r[1] for r in rows}
    except Exception:
        pass

    if counts is None:
        counts = count_all_tables(names)

    # Build listing table
    now = time.time()
    con.execute("DROP TABLE IF EXISTS listing")
    con.execute("CREATE TABLE listing (name VARCHAR, safety VARCHAR, rows INTEGER, description VARCHAR, updated_at DOUBLE)")
    for name in names:
        safety = "input-required" if name in DANGEROUS_TABLES else "safe"
        row_count = counts.get(name)
        desc = desc_map.get(name, "")
        con.execute("INSERT INTO listing VALUES (?, ?, ?, ?, ?)",
                    [name, safety, row_count, desc, now])

    con.close()
    print(f"Populated {DUCKDB_PATH}: {len(names)} tables", file=sys.stderr)


if __name__ == "__main__":
    main()
