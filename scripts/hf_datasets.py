#!/usr/bin/env python3
"""
Populate ~/.cache/tv/hf_datasets.duckdb with Hugging Face dataset listing.

Creates a `listing` table with: id, author, downloads, likes, description,
created, modified, tags (semicolon-separated), license, task, language.

Fetches top datasets sorted by downloads (cursor-paginated).
Caches for 24h; re-run to refresh.

Dependencies: python3 (stdlib only), duckdb CLI, curl
"""

import json
import re
import subprocess
import sys
import time
from pathlib import Path

CACHE_DIR = Path.home() / ".cache" / "tv"
DUCKDB_PATH = CACHE_DIR / "hf_datasets.duckdb"
TMP_JSON = CACHE_DIR / "tmp_hf_listing.json"
CACHE_MAX_AGE = 24 * 3600  # 24 hours

API_BASE = "https://huggingface.co/api/datasets"
PAGE_LIMIT = 1000
MAX_DATASETS = 50000  # safety cap


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


def check_cache_fresh():
    """Check if existing listing is fresh enough."""
    if not DUCKDB_PATH.exists():
        return False
    rows = duckdb_json(
        "SELECT count(*) as n, max(updated_at) as ts FROM listing"
    )
    if not rows or not rows[0].get("ts"):
        return False
    try:
        ts = float(rows[0]["ts"])
        n = int(rows[0]["n"])
        return n > 0 and (time.time() - ts) < CACHE_MAX_AGE
    except (ValueError, TypeError):
        return False


def fetch_page(url):
    """Fetch a single API page. Returns (entries, next_url)."""
    r = subprocess.run(
        ["curl", "-sf", "-D-", url],
        capture_output=True, text=True, timeout=60,
    )
    if r.returncode != 0:
        return [], None

    # Split headers and body
    parts = r.stdout.split("\r\n\r\n", 1)
    if len(parts) < 2:
        # Try \n\n
        parts = r.stdout.split("\n\n", 1)
    if len(parts) < 2:
        return [], None

    headers, body = parts[0], parts[1]

    # Parse entries
    try:
        entries = json.loads(body)
    except json.JSONDecodeError:
        return [], None

    # Parse Link header for next page
    next_url = None
    for line in headers.split("\n"):
        if line.lower().startswith("link:"):
            m = re.search(r'<([^>]+)>;\s*rel="next"', line)
            if m:
                next_url = m.group(1)
    return entries, next_url


def extract_tag(tags, prefix):
    """Extract first tag value matching prefix (e.g. 'license:mit' → 'mit')."""
    for t in tags:
        if t.startswith(prefix):
            return t[len(prefix):]
    return ""


def clean_desc(s):
    """Strip HTML/markdown noise from description, collapse whitespace."""
    s = re.sub(r'<[^>]+>', ' ', s)       # strip HTML tags
    s = re.sub(r'\[([^\]]*)\]\([^)]*\)', r'\1', s)  # [text](url) → text
    s = re.sub(r'[#*_~`>]', '', s)       # strip markdown chars
    s = re.sub(r'\s+', ' ', s).strip()   # collapse whitespace
    return s[:200]


def fetch_all():
    """Fetch all datasets via paginated API."""
    url = f"{API_BASE}?limit={PAGE_LIMIT}&sort=downloads&direction=-1&full=true"
    all_entries = []
    page = 0

    while url and len(all_entries) < MAX_DATASETS:
        page += 1
        entries, next_url = fetch_page(url)
        if not entries:
            break
        all_entries.extend(entries)
        print(f"  page {page}: {len(entries)} datasets (total: {len(all_entries)})",
              file=sys.stderr)
        url = next_url

    return all_entries


def build_rows(entries):
    """Convert API entries to flat rows for DuckDB."""
    now = time.time()
    rows = []
    for e in entries:
        tags = e.get("tags", [])
        card = e.get("cardData", {}) or {}
        rows.append({
            "id": e.get("id", ""),
            "author": e.get("author", ""),
            "downloads": e.get("downloads", 0),
            "likes": e.get("likes", 0),
            "description": clean_desc(card.get("pretty_name") or e.get("description", "") or ""),
            "created": (e.get("createdAt", "") or "")[:19],
            "modified": (e.get("lastModified", "") or "")[:19],
            "license": extract_tag(tags, "license:") or str(card.get("license", "") or ""),
            "task": extract_tag(tags, "task_categories:"),
            "language": extract_tag(tags, "language:"),
            "gated": e.get("gated", False),
            "updated_at": now,
        })
    return rows


def main():
    CACHE_DIR.mkdir(parents=True, exist_ok=True)

    if check_cache_fresh():
        print("HF dataset listing is fresh, skipping fetch", file=sys.stderr)
        return

    print("Fetching HF dataset listing...", file=sys.stderr)
    entries = fetch_all()
    if not entries:
        print("No datasets fetched", file=sys.stderr)
        sys.exit(1)

    rows = build_rows(entries)
    TMP_JSON.write_text(json.dumps(rows))
    duckdb_exec(
        "DROP TABLE IF EXISTS listing; "
        f"CREATE TABLE listing AS SELECT * FROM read_json_auto('{TMP_JSON}')"
    )
    try:
        TMP_JSON.unlink()
    except OSError:
        pass

    print(f"Populated {DUCKDB_PATH}: {len(rows)} datasets", file=sys.stderr)


if __name__ == "__main__":
    main()
