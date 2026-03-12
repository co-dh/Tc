# DuckDB Cheat Sheet
`FROM 'data.csv' SELECT * WHERE x > 0 ORDER BY y LIMIT 10` — FROM-first, file paths as tables, SQL + extensions.
## Query
| Clause       | Syntax                                  || Clause       | Syntax                                  |
|------------- |---------                                |--|------------- |---------                                |
| `FROM`       | `FROM tbl` / `FROM 'file.csv'`          || `ORDER BY`   | `ORDER BY col [ASC|DESC] [NULLS LAST]`  |
| `SELECT`     | `SELECT col1, expr AS alias`            || `LIMIT`      | `LIMIT n [OFFSET m]`                    |
| `WHERE`      | `WHERE condition`                       || `HAVING`     | `HAVING agg_condition`                  |
| `GROUP BY`   | `GROUP BY col` / `GROUP BY ALL`         || `QUALIFY`    | `QUALIFY row_number() OVER (...) = 1`   |
| `DISTINCT`   | `SELECT DISTINCT col`                   || `SAMPLE`     | `TABLESAMPLE 10%` / `TABLESAMPLE 10 ROWS` |
| `DISTINCT ON`| `DISTINCT ON(col) ... ORDER BY ...`     || `WITH`       | `WITH t AS (...) SELECT * FROM t`       |

## DuckDB Extensions
```sql
FROM tbl;                                           -- SELECT * implied
FROM 'file.csv';                                    -- direct file query
FROM 'data/*.parquet';                              -- glob patterns
SELECT * EXCLUDE (col1, col2) FROM tbl;             -- drop columns
SELECT * REPLACE (lower(city) AS city) FROM tbl;    -- modify in-place
SELECT COLUMNS('num\d+') FROM tbl;                  -- regex column select
DESCRIBE tbl;                                       -- show schema
SUMMARIZE tbl;                                      -- column stats
COPY tbl TO 'out.csv';                              -- export
COPY tbl TO 'out.parquet' (FORMAT PARQUET);          -- export parquet
CREATE TABLE t AS SELECT * FROM 'file.csv';          -- CTAS
INSERT INTO t SELECT * FROM 'more.csv';              -- insert from file
```
## Joins
```sql
FROM a JOIN b ON a.id = b.id                         -- inner (default)
FROM a LEFT JOIN b ON a.id = b.id                    -- left outer
FROM a FULL JOIN b USING (id)                        -- full outer, shared col
FROM a CROSS JOIN b                                  -- cartesian product
FROM a SEMI JOIN b ON a.id = b.id                    -- rows in a with match in b
FROM a ANTI JOIN b ON a.id = b.id                    -- rows in a without match
FROM a POSITIONAL JOIN b                             -- by row position
FROM a ASOF JOIN b ON a.sym = b.sym AND a.ts >= b.ts -- nearest match
FROM a, LATERAL (SELECT a.x + 1) t(y)               -- correlated subquery
```
## Set Operations
```sql
SELECT * FROM a UNION ALL SELECT * FROM b            -- all rows
SELECT * FROM a UNION BY NAME SELECT * FROM b        -- match by column name
SELECT * FROM a INTERSECT SELECT * FROM b            -- common rows
SELECT * FROM a EXCEPT SELECT * FROM b               -- rows in a not in b
```
## Types
| Type          | Aliases              || Type          | Aliases              |
|-------------- |--------              |--|-------------- |--------              |
| `BOOLEAN`     | `BOOL`               || `DATE`        |                      |
| `TINYINT`     | `INT1`               || `TIME`        |                      |
| `SMALLINT`    | `INT2`, `SHORT`      || `TIMESTAMP`   | `DATETIME`           |
| `INTEGER`     | `INT`, `INT4`        || `TIMESTAMPTZ` |                      |
| `BIGINT`      | `INT8`, `LONG`       || `INTERVAL`    |                      |
| `HUGEINT`     |                      || `UUID`        |                      |
| `FLOAT`       | `FLOAT4`, `REAL`     || `VARCHAR`     | `TEXT`, `STRING`     |
| `DOUBLE`      | `FLOAT8`             || `BLOB`        | `BYTEA`, `BINARY`    |
| `DECIMAL(p,s)`| `NUMERIC(p,s)`       || `JSON`        |                      |

**Nested**: `INTEGER[]` (list), `INTEGER[3]` (array), `STRUCT(a INT, b TEXT)`, `MAP(INT, TEXT)`, `UNION(n INT, s TEXT)`
## Aggregates
| Function                   | Description          || Function                   | Description          |
|----------                  |-----------           |--|----------                  |-----------           |
| `count(*)` / `count(col)`  | row / non-null count || `median(x)`                | 50th percentile      |
| `sum(x)` / `avg(x)`        | sum / mean           || `mode(x)`                  | most frequent        |
| `min(x)` / `max(x)`        | min / max            || `quantile_cont(x, 0.95)`   | continuous quantile  |
| `first(x)` / `last(x)`     | first / last value   || `string_agg(x, ',')`       | concat with sep      |
| `arg_min(ret, x)`          | ret at min x         || `list(x)`                  | collect into list    |
| `arg_max(ret, x)`          | ret at max x         || `histogram(x)`             | value → count map    |
| `stddev_samp(x)`           | sample std dev       || `approx_count_distinct(x)` | HLL distinct count   |
| `bool_and(x)` / `bool_or`  | logical and/or       || `product(x)`               | multiply all         |

**Modifiers**: `agg(DISTINCT x)`, `agg(x ORDER BY y)`, `agg(x) FILTER (WHERE cond)`
## Window Functions
```sql
SELECT col, agg(x) OVER (
  PARTITION BY grp ORDER BY ts
  ROWS BETWEEN 2 PRECEDING AND CURRENT ROW
) FROM tbl;
```
| Function          | Description            || Function          | Description            |
|----------         |-----------             |--|----------         |-----------             |
| `row_number()`    | sequential 1..n        || `lag(x, n)`       | n rows before          |
| `rank()`          | with gaps              || `lead(x, n)`      | n rows after           |
| `dense_rank()`    | without gaps           || `first_value(x)`  | first in frame         |
| `ntile(n)`        | bucket 1..n            || `last_value(x)`   | last in frame          |
| `percent_rank()`  | relative rank          || `nth_value(x, n)` | nth in frame           |

**Frames**: `ROWS n PRECEDING`, `RANGE INTERVAL '3' DAY PRECEDING`, `GROUPS`, `UNBOUNDED PRECEDING/FOLLOWING`
## Text Functions
| Function                               || Function                                |
|----------                              |--|----------                               |
| `lower(s)` · `upper(s)` · `length(s)` || `replace(s, from, to)`                  |
| `trim(s)` · `ltrim(s)` · `rtrim(s)`   || `regexp_matches(s, pat)`                |
| `contains(s, sub)`                     || `regexp_replace(s, pat, repl)`          |
| `starts_with(s, pfx)`                  || `regexp_extract(s, pat [, grp])`        |
| `ends_with(s, sfx)`                    || `split_part(s, sep, idx)`               |
| `left(s, n)` · `right(s, n)`          || `s || t` (concat) · `concat_ws(',',…)`  |
| `lpad(s, n, c)` · `rpad(s, n, c)`     || `format('{}', x)` · `printf('%d', x)`   |
| `substring(s, start [, len])`          || `s[1:3]` (slice) · `position(sub IN s)` |
| `reverse(s)` · `repeat(s, n)`         || `strip_accents(s)` · `unicode(s)`       |

**Regex options**: `regexp_matches(s, pat, 'i')` — i=case-insensitive, s=dotall, g=global
## Date / Time Functions
| Function                            || Function                             |
|----------                           |--|----------                            |
| `today()` · `current_timestamp`     || `date_diff('day', d1, d2)`           |
| `date_part('year', d)`              || `date_add(d, INTERVAL '3' MONTH)`    |
| `date_trunc('month', d)`            || `strftime(d, '%Y-%m-%d')`            |
| `extract(dow FROM d)`               || `strptime('2024-01-15', '%Y-%m-%d')` |
| `make_date(2024, 1, 15)`            || `age(d1, d2)` — interval diff        |
| `last_day(d)` · `dayname(d)`        || `epoch(ts)` — seconds since epoch    |

**Format**: `%Y`=2024 `%m`=01 `%d`=15 `%H`=14 `%M`=30 `%S`=00 `%a`=Mon `%b`=Jan `%F`=2024-01-15 `%T`=14:30:00
## List Functions
| Function                           || Function                           |
|----------                          |--|----------                          |
| `list_transform(l, x -> x+1)`     || `list_sort(l)` · `list_distinct(l)`|
| `list_filter(l, x -> x > 0)`      || `list_contains(l, val)`            |
| `list_reduce(l, (a,b) -> a+b)`    || `list_position(l, val)`            |
| `list_aggregate(l, 'sum')`        || `list_concat(l1, l2)`              |
| `flatten(nested)` · `unnest(l)`   || `list_slice(l, 1, 3)`              |
| `generate_series(1, 10)`          || `list_zip(l1, l2)`                 |
| `range(0, 10, 2)`                 || `[1, 2, 3]` — literal              |

**Lambda**: `list_transform([1,2,3], x -> x * 2)` → `[2, 4, 6]`
## PIVOT / UNPIVOT
```sql
PIVOT tbl ON year USING sum(pop) GROUP BY country;           -- cols from values
UNPIVOT tbl ON col1, col2, col3 INTO NAME var VALUE val;     -- cols to rows
```
## File I/O
```sql
FROM 'data.csv';                                    -- auto-detect
FROM read_csv('data.csv', header=true, delim='\t'); -- explicit options
FROM read_parquet('*.parquet');                      -- glob
FROM read_parquet(['a.parquet', 'b.parquet']);       -- explicit list
FROM read_json('data.json');                        -- JSON
COPY tbl TO 'out.parquet' (FORMAT PARQUET);         -- write
COPY (SELECT ...) TO 'out.csv' (HEADER, DELIMITER '\t');
```
## Useful Patterns
```sql
-- Dedup: keep latest per group
SELECT DISTINCT ON (id) * FROM tbl ORDER BY id, ts DESC;

-- Running total
SELECT *, sum(amt) OVER (ORDER BY ts) AS running FROM tbl;

-- Top-N per group
SELECT * FROM (
  SELECT *, row_number() OVER (PARTITION BY grp ORDER BY val DESC) AS rn FROM tbl
) WHERE rn <= 3;

-- Conditional aggregation
SELECT count(*) FILTER (WHERE status = 'ok') AS ok_count FROM tbl;

-- String splitting to rows
SELECT unnest(string_split(tags, ',')) AS tag FROM tbl;

-- Struct packing / unpacking
SELECT t FROM tbl t;                                -- row as struct
SELECT t.* FROM (SELECT {a: 1, b: 2} AS t);        -- unpack struct
```
