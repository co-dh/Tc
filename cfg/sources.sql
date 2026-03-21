-- Default source configs for tv remote sources (S3, HF, REST, osquery, pg://).
-- File formats (csv, parquet, arrow, xlsx, etc.) are handled in code, not here.
-- Template placeholders: {1}..{9} = path parts, {N+} = parts N onward joined by /,
-- {path}, {name}, {tmp}, {extra}, {dsn} = path with prefix stripped,
-- {src} = JSON temp file (in list_sql only), {home} = $HOME (in setup_sql only).
-- duckdb_ext: auto INSTALL/LOAD before any operation. Empty = none.
-- attach: true = enter uses fromDuckDBTable. If list_sql is empty, auto-generates
--   DETACH/ATTACH/SELECT from attach_type and duckdb_ext.
-- attach_type: TYPE clause for ATTACH (e.g. 'POSTGRES'). Empty = native DuckDB.

DROP TABLE IF EXISTS tv_sources;
CREATE TABLE tv_sources (
  pfx VARCHAR, min_parts INTEGER, list_cmd VARCHAR,
  list_sql VARCHAR, download_cmd VARCHAR, needs_download BOOLEAN,
  dir_suffix BOOLEAN, parent_fallback VARCHAR,
  setup_cmd VARCHAR, setup_sql VARCHAR, grp VARCHAR, enter_url VARCHAR,
  script VARCHAR, attach BOOLEAN,
  duckdb_ext VARCHAR, attach_type VARCHAR
);

INSERT INTO tv_sources VALUES
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
   '', false, '', ''),

  -- HF dataset browser: curl HF Hub API, DuckDB reads via httpfs
  ('hf://datasets/', 5,
   'curl -sf https://huggingface.co/api/datasets/{1}/{2}/tree/main/{3+}',
   'SELECT split_part(path, ''/'', -1) as name, size, type
    FROM read_json_auto(''{src}'')',
   'curl -sfL -o {tmp}/{name} https://huggingface.co/datasets/{1}/{2}/resolve/main/{3+}',
   false, true, 'hf://',
   '', '', '', '',
   '', false, '', ''),

  -- HF root: dataset listing from pre-populated DuckDB
  ('hf://', 0,
   '',
   'SELECT id, downloads, likes, description, license, task, language, created, modified
    FROM hf.listing ORDER BY downloads DESC',
   '',
   false, false, '',
   'python3 scripts/hf_datasets.py',
   'ATTACH ''{home}/.cache/tv/hf_datasets.duckdb'' AS hf (READ_ONLY)',
   'id', 'hf://datasets/{name}/',
   '', false, '', ''),

  -- Generic REST API: curl any JSON endpoint
  ('rest://', 1,
   'curl -sfL https://{1+}',
   'SELECT * FROM read_json_auto(''{src}'', auto_detect=true)',
   '',
   false, false, '',
   '', '', '', '',
   '', false, '', ''),

  -- Osquery: stub views in osq schema provide types + column comments.
  ('osquery://', 0,
   '',
   'SELECT name, safety, rows, description FROM osq.listing ORDER BY name',
   '', false, false, '',
   'python3 scripts/osquery_tables.py',
   'ATTACH ''{home}/.cache/tv/osquery.duckdb'' AS osq (READ_ONLY)',
   'name', '',
   'osqueryi --json "SELECT * FROM {name}"', false, '', ''),

  -- PostgreSQL: attach=true + duckdb_ext auto-generates ATTACH SQL
  ('pg://', 99, '', '', '', false, false, '',
   '', '', 'name', '',
   '', true, 'postgres', 'POSTGRES');
