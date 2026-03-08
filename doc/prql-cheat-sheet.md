# PRQL Cheat Sheet
`from t | filter x > 0 | select {a, b} | sort {-a} | take 10` — pipe `|` chains transforms; newlines also work.
## Transforms
| Transform | Syntax                      | Transform   | Syntax                          | Transform | Syntax                          |
|---------- |--------                     |------------ |--------                         |---------- |--------                         |
| `from`    | `from table`                | `sort`      | `sort {+col1, -col2}`           | `group`   | `group {col} (aggregate {...})` |
| `select`  | `select {col1, new = expr}` | `take`      | `take 10` / `take 5..10`        | `window`  | `window rows:-2..0 (derive ..)` |
| `filter`  | `filter condition`          | `join`      | `join side:left t (==col)`      | `append`  | `append other` (UNION ALL)      |
| `derive`  | `derive {new_col = expr}`   | `aggregate` | `aggregate {sum val, count x}`  | `remove`  | `remove other` (set diff)       |
## Operators (by precedence, tightest first)
| P | Group   | Operators              | P | Group    | Operators                    | P  | Group   | Operators |
|---|---------|-------                 |---|----------|-------                       |----|---------|-------    |
| 0 | parens  | `()`                   | 4 | pow      | `**` (right-to-left)         | 8  | coalesce| `??`      |
| 1 | dot     | `.`                    | 5 | mul      | `*` `/` `//` `%`             | 9  | and     | `&&`      |
| 2 | unary   | `-` `+` `!` `==`       | 6 | add      | `+` `-`                      | 10 | or      | `\|\|`    |
| 3 | range   | `..`                   | 7 | compare  | `==` `!=` `<` `>` `<=` `>=`  | 11 | func    |           |
Also: `~=` regex match. `/` float div, `//` integer div.
## Literals & Strings
| Example                                           | Example                                              |
|--------                                           |--------                                              |
| `42` `5_000_000` `3.14` `5e9` `0xff` `0b10` `0o7` | `@2024-01-15` `@14:30` `@2024-01-15T14:30-08:00`     |
| `true` `false` `null`                             | `3days` `2years` `1hours`                            |
| `'single'` `"double"` `"""triple "quoted" """`    | `f"Hello {name}"` interpolate cols (names only)      |
| `s"SQL({col})"` raw SQL escape hatch              | `r"\no\escaping"` raw string                         |
s-string tips: `{{`/`}}` literal braces, `s"({x}) / 365"` for precedence.
## Joins · Group · Window
| Example                                                               | Notes                                                              |
|--------                                                               |------                                                              |
| `join side:left t (==id)`                                             | shorthand                                                          |
| `join side:left p=pos (this.id == that.eid)`                          | alias                                                              |
| `join side:full other (true)`                                         | cross join                                                         |
| `group {dept} (aggregate {avg_sal = average salary, n = count this})` | group+agg                                                          |
| `group role (sort date \| take 1)`                                    | first per group                                                    |
| `window rows:-2..0 (derive {ma = average val})`                       | sliding; `rolling:3` = `rows:-2..0`, `expanding:true` = `rows:..0` |
Sides: `inner` (default), `left`, `right`, `full`. Refs: `this`/`that`.
## Case · Functions · Declarations
| Example                                                       | Notes                         |
|--------                                                       |------                         |
| `case [a > 0 => "pos", true => "other"]`                      | case (true=default)           |
| `let f = lo:0 hi x -> (x - lo) / (hi - lo)`                   | func, lo default 0            |
| `x \| f 100`                                                  | pipe into last arg            |
| `let t = (from employees \| filter active)`                   | named relation                |
| `into mid`                                                    | name mid-pipeline             |
| `col \| as int` · `status \| in ["a","b"]` · `` `col name` `` | cast · membership · quoted id |
## stdlib
| Params             | Functions                                                                                                      |
|---------           |----------                                                                                                      |
| **text** `col`     | `lower` `upper` `trim` `ltrim` `rtrim` `length`                                                                |
| **text** `s c`     | `contains s c` · `starts_with s c` · `ends_with s c` · `extract i n c` · `replace before after c`              |
| **math** `col`     | `abs` `floor` `ceil` `sqrt` `exp` `ln` `log10` `pi` `sin` `cos` `tan` `asin` `acos` `atan` `degrees` `radians` |
| **math** `n col`   | `round n c` · `pow b c` · `log b c`                                                                            |
| **agg** `col`      | `min` `max` `sum` `average` `stddev` `count` `count_distinct` `any` `all` `concat_array`                       |
| **window** `col`   | `first` `last` `rank` `rank_dense` `row_number` · `lag n c` · `lead n c`                                       |
| **file** `path`    | `read_parquet` `read_csv` `read_json` · **date**: `date.to_text fmt col`                                       |
## date format (`date.to_text fmt col`)
| Spec | Example    | Spec | Example   | Spec | Example              |
|----- |--------    |----- |---------  |----- |---------             |
| `%Y` | 2001       | `%d` | 08        | `%H` | 00                   |
| `%y` | 01         | `%-d`| 8         | `%M` | 34                   |
| `%m` | 07         | `%a` | Sun       | `%S` | 60                   |
| `%-m`| 7          | `%A` | Sunday    | `%f` | 264900               |
| `%b` | Jul        | `%F` | 2001-07-08| `%T` | 00:34:60             |
| `%B` | July       | `%D` | 07/08/01  | `%+` | 2001-07-08T00:34:60Z |
DuckDB: `prqlc compile -t sql.duckdb` (`~=` → `REGEXP_MATCHES`).
