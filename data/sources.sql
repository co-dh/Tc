-- Default source configs for tc remote browsing.
-- Template placeholders: {1}..{9} = path parts, {N+} = parts N onward joined by /,
-- {path}, {name}, {tmp}, {extra}, {src} = JSON temp file (in list_sql only).
-- {home} is expanded at load time.

CREATE TABLE IF NOT EXISTS tc_sources (
  pfx VARCHAR, min_parts INTEGER, list_cmd VARCHAR,
  list_sql VARCHAR, download_cmd VARCHAR, needs_download BOOLEAN,
  dir_suffix BOOLEAN, parent_fallback VARCHAR,
  setup_cmd VARCHAR, setup_sql VARCHAR, grp VARCHAR, enter_url VARCHAR
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
   '', '', '', ''),

  -- HF dataset browser: curl HF Hub API, DuckDB reads via httpfs
  ('hf://datasets/', 5,
   'curl -sf https://huggingface.co/api/datasets/{1}/{2}/tree/main/{3+}',
   'SELECT split_part(path, ''/'', -1) as name, size, type
    FROM read_json_auto(''{src}'')',
   'curl -sfL -o {tmp}/{name} https://huggingface.co/datasets/{1}/{2}/resolve/main/{3+}',
   false, true, 'hf://',
   '', '', '', ''),

  -- HF root: dataset listing from pre-populated DuckDB
  ('hf://', 0,
   '',
   'SELECT id, downloads, likes, description, license, task, language, created, modified
    FROM hf.listing ORDER BY downloads DESC',
   '',
   false, false, '',
   'python3 scripts/hf_datasets.py',
   'ATTACH ''{home}/.cache/tc/hf_datasets.duckdb'' AS hf (READ_ONLY)',
   'id', 'hf://datasets/{name}/'),

  -- Generic REST API: curl any JSON endpoint
  -- Usage: tc rest://https://api.github.com/users/octocat/repos
  ('rest://', 1,
   'curl -sf {1+}',
   'SELECT * FROM read_json_auto(''{src}'', auto_detect=true)',
   '',
   false, false, '',
   '', '', '', '');
