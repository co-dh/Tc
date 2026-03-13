-- Default source configs for tc remote browsing and file type readers.
-- Template placeholders: {1}..{9} = path parts, {N+} = parts N onward joined by /,
-- {path}, {name}, {tmp}, {extra}, {src} = JSON temp file (in list_sql only).
-- {home} is expanded at load time.
-- script: shell cmd template for entering a file row (stdout = JSON rows). Empty = none.
-- ext: comma-separated file extensions (e.g. '.sqlite,.sqlite3'). Matched by findByExt.
-- reader: DuckDB reader function for ext-based files. Empty = auto-detect.
-- attach: true = enter uses fromDuckDBTable (for attached databases like DuckDB/SQLite).

CREATE TABLE IF NOT EXISTS tc_sources (
  pfx VARCHAR, min_parts INTEGER, list_cmd VARCHAR,
  list_sql VARCHAR, download_cmd VARCHAR, needs_download BOOLEAN,
  dir_suffix BOOLEAN, parent_fallback VARCHAR,
  setup_cmd VARCHAR, setup_sql VARCHAR, grp VARCHAR, enter_url VARCHAR,
  script VARCHAR,
  ext VARCHAR, reader VARCHAR, attach BOOLEAN
);

INSERT INTO tc_sources VALUES
  -- S3: aws s3api JSON output, download via aws s3 cp
  ('s3://', 3,
   'aws s3api list-objects-v2 --bucket {1} --delimiter / --prefix {2+}/ {extra} --output json',
   'SELECT split_part(unnest.Key, ''/'', -1) as name, unnest.Size as size,
           unnest.LastModified as date, ''file'' as type
    FROM (SELECT unnest(Contents) FROM read_json_auto(''{src}''))
    WHERE unnest.Key IS NOT NULL
    UNION ALL
    SELECT split_part(unnest.Prefix, ''/'', -2) as name, 0 as size,
           '''' as date, ''dir'' as type
    FROM (SELECT unnest(CommonPrefixes) FROM read_json_auto(''{src}''))
    WHERE unnest.Prefix IS NOT NULL',
   'aws s3 cp {extra} {path} {tmp}/{name}',
   true, true, '',
   '', '', '', '',
   '', '', '', false),

  -- HF dataset browser: curl HF Hub API, DuckDB reads via httpfs
  ('hf://datasets/', 5,
   'curl -sf https://huggingface.co/api/datasets/{1}/{2}/tree/main/{3+}',
   'SELECT split_part(path, ''/'', -1) as name, size, type
    FROM read_json_auto(''{src}'')',
   'curl -sfL -o {tmp}/{name} https://huggingface.co/datasets/{1}/{2}/resolve/main/{3+}',
   false, true, 'hf://',
   '', '', '', '',
   '', '', '', false),

  -- HF root: dataset listing from pre-populated DuckDB
  ('hf://', 0,
   '',
   'SELECT id, downloads, likes, description, license, task, language, created, modified
    FROM hf.listing ORDER BY downloads DESC',
   '',
   false, false, '',
   'python3 scripts/hf_datasets.py',
   'ATTACH ''{home}/.cache/tc/hf_datasets.duckdb'' AS hf (READ_ONLY)',
   'id', 'hf://datasets/{name}/',
   '', '', '', false),

  -- Generic REST API: curl any JSON endpoint
  ('rest://', 1,
   'curl -sfL https://{1+}',
   'SELECT * FROM read_json_auto(''{src}'', auto_detect=true)',
   '',
   false, false, '',
   '', '', '', '',
   '', '', '', false),

  -- Osquery: stub views in osq schema provide types + column comments.
  ('osquery://', 0,
   '',
   'SELECT name, safety, rows, description FROM osq.listing ORDER BY name',
   '', false, false, '',
   'python3 scripts/osquery_tables.py',
   'ATTACH ''{home}/.cache/tc/osquery.duckdb'' AS osq (READ_ONLY)',
   'name', '',
   'osqueryi --json "SELECT * FROM {name}"', '', '', false),

  -- DuckDB databases: ATTACH and list tables
  ('', 0, '',
   'DETACH DATABASE IF EXISTS extdb;
    ATTACH ''{path}'' AS extdb (READ_ONLY);
    SELECT table_name as name, estimated_size as size, column_count as columns
    FROM duckdb_tables() WHERE database_name = ''extdb''',
   '', false, false, '',
   '', '', 'name', '', '',
   '.duckdb,.db', '', true),

  -- SQLite databases: ATTACH via DuckDB sqlite extension
  ('', 0, '',
   'DETACH DATABASE IF EXISTS extdb;
    INSTALL sqlite;
    LOAD sqlite;
    ATTACH ''{path}'' AS extdb (TYPE SQLITE, READ_ONLY);
    SELECT table_name as name FROM duckdb_tables() WHERE database_name = ''extdb''',
   '', false, false, '',
   '', '', 'name', '', '',
   '.sqlite,.sqlite3', '', true),

  -- File readers: auto-detected by DuckDB
  ('', 0, '', '', '', false, false, '', '', '', '', '', '',
   '.csv,.parquet,.json,.jsonl,.ndjson', '', false),

  -- Arrow IPC / Feather: needs explicit reader
  ('', 0, '', '', '', false, false, '',
   '', 'INSTALL arrow; LOAD arrow', '', '', '',
   '.arrow,.feather', 'read_arrow', false),

  -- Excel: needs excel extension
  ('', 0, '', '', '', false, false, '',
   '', 'INSTALL excel; LOAD excel', '', '', '',
   '.xlsx,.xls', 'read_xlsx', false),

  -- Avro: needs avro extension
  ('', 0, '', '', '', false, false, '',
   '', 'INSTALL avro; LOAD avro', '', '', '',
   '.avro', 'read_avro', false);
