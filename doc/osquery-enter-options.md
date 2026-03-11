# Osquery Enter Options

Three approaches explored for querying osquery tables through DuckDB.

## Option 1: Script cmd (chosen)

The `script` column in `tc_sources` holds a shell command template (e.g. `osqueryi --json "SELECT * FROM {name}"`). Lean runs it, loads the JSON stdout into a temp table, then applies `TRY_CAST` using column types from the DuckDB stub views. Status bar and meta enrich use generic `duckdb_columns()` comment lookup.

**Pros**: Simple, no new DuckDB extensions, stub views provide types + comments, setup is instant.
**Cons**: Requires a `script` column in `tc_sources` (only used by osquery currently).

## Option 2: shellfs-backed DuckDB views

Each osquery table becomes a DuckDB view using the shellfs extension: `CREATE VIEW osq.groups AS SELECT ... FROM read_json_auto('osqueryi --json "SELECT * FROM groups" |')`. Entering a table = `SELECT * FROM osq.groups`. No script needed.

**Pros**: Pure DuckDB, no Lean script code, views are self-contained with types + comments.
**Cons**: DuckDB eagerly validates views at creation time — it runs `osqueryi` for every table during `CREATE VIEW` (~0.5s each, ~75s total for 151 tables). Platform-specific tables (Windows-only) fail. Not viable for setup.

## Option 3: shellfs macro + stub views

A single DuckDB table-returning macro `osq_enter(tbl)` does the data fetch lazily. Stub views still provide type/comment metadata. Lean calls `SELECT * FROM osq_enter('groups')` at enter time.

**Pros**: Lazy (no eager validation), single macro for all tables, metadata from stub views.
**Cons**: Macro returns all VARCHAR (osqueryi JSON), still needs TRY_CAST from stub view types. Lean code must know to call the macro — requires either a config field or hardcoded logic. More complexity for minimal gain over option 1.
