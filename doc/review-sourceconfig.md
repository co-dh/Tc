# Code Review: Config-Driven Source Handling

Reviewed: `main` branch (8 commits from `eb90bbb` to `8143209`)

## Overview

Replaces `S3.lean`, `HF.lean`, and `Osquery.lean` (~411 lines deleted) with a single `SourceConfig.lean` (271 lines) backed by a DuckDB config table (`cfg/sources.duckdb` / `cfg/sources.sql`). Adding a new source is now a SQL `INSERT` — no Lean code changes needed. `Folder.lean` is simplified from importing 4 modules to 2 (`SourceConfig` + `Remote`).

## Good

- **Strong design**: Config-as-data in DuckDB is a natural fit for an app that already uses DuckDB everywhere. The SQL config table is readable and self-documenting.
- **QRow** (`SourceConfig.lean:84-103`): Eliminates row/col swap bugs by construction. Clean API.
- **Setup idempotency** (`SourceConfig.lean:146-168`): Try SQL first, fall back to shell cmd, mark done. Handles the cold-start case (osquery DB doesn't exist yet) correctly.
- **Folder.lean** is much cleaner — no more `Backend` struct or `backend?` dispatch. `mkView` is now a simple `findSource` → `runList` → `mkViewFromAdbc` pipeline.
- **Test coverage**: Osquery tests exercise the full `script` → `runEnter` → type-casting pipeline. HF and folder tests cover the CLI-mode path.
- **`sources.sql`** is well-structured with clear comments per source.

## Issues Found & Fixed

### 1. Shell injection via `expand` (medium severity) — FIXED

`expand` does raw string substitution into `sh -c` commands. Path components like `s3://bucket/$(whoami)` would execute. The old code (`S3.lean`) passed args as an array to `IO.Process.output`, which is safe.

**Fix**: Added `isSafePathChar` whitelist and `validateShellSafe` check in `cmdVars` and `runEnter`. Rejects paths containing shell metacharacters (`$`, backticks, `;`, `&`, etc.) before template expansion.

### 2. `s3Extra` hardcoded to S3 prefix — DOCUMENTED

`cmdVars` line 188: `if cfg.pfx == "s3://" then s3Extra else pure ""` couples the generic system to S3. Only S3 needs runtime auth flags (`--no-sign-request` via `+n` CLI arg); other sources use static URLs or tokens in their templates directly.

**Fix**: Added comment explaining the design decision.

### 3. `findSource` SQL injection (minor) — FIXED

Replaced string-interpolated SQL with parameterized queries via `Adbc.queryParam` (backed by `AdbcStatementBind`). Both `findSource` and the type-apply query in `runEnter` now use `$1` placeholders instead of string escaping.

### 4. Parent detection heuristic is fragile — FIXED

Line 219 checks `cfg.listSql.splitOn "as type"` to decide whether to UNION a `..` parent row. This fails on `AS type` or other case variations.

**Fix**: Made the check case-insensitive by also checking `"AS type"`.

### 5. `attachDb` silent failure — FIXED

When `sources.duckdb` isn't found, the app runs with no sources configured. This could confuse users trying remote browsing.

**Fix**: Improved log message to explicitly state remote source browsing is disabled.

### 6. `runDownload` calls `tmpPath` twice — FIXED

`cmdVars` already calls `Tc.tmpPath "src"` and includes it in template vars. `runDownload` called it again redundantly.

**Fix**: Reuse `tmpDir` from the vars array returned by `cmdVars`.

## Summary

| Area | Assessment |
|------|-----------|
| Architecture | Excellent — config-as-data, single dispatch point |
| Code reduction | Net -172 lines, 3 modules → 1 |
| Correctness | Good — `QRow` fixes prior bug class |
| Shell safety | Fixed — path validation before template expansion |
| Extensibility | Very good — SQL INSERT to add sources |
| Tests | Thorough — covers osquery, HF, folder, DuckDB paths |
