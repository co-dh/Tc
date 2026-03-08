# PRQL Cheat Sheet
`from t | filter x > 0 | select {a, b} | sort {-a} | take 10` — pipe `|` chains transforms; newlines also work.
## Transforms
| Transform   | Syntax                            | Transform   | Syntax                           |
|------------ |--------                           |------------ |--------                          |
| `from`      | `from table`                      | `join`      | `join side:left t (==col)`       |
| `select`    | `select {col1, new = expr}`       | `group`     | `group {col} (aggregate {...})`  |
| `filter`    | `filter condition`                | `window`    | `window rows:-2..0 (derive ..)`  |
| `derive`    | `derive {new_col = expr}`         | `aggregate` | `aggregate {sum val, count x}`   |
| `sort`      | `sort {+col1, -col2}`             | `append`    | `append other` (UNION ALL)       |
| `take`      | `take 10` / `take 5..10`          | `remove`    | `remove other` (set difference)  |
## Operators (by precedence, tightest first)
| P  | Group     | Operators                   | P  | Group    | Operators                    |
|--- |---------- |-----------                  |--- |--------- |-----------                   |
| 0  | parens    | `()`                        | 6  | add      | `+` `-`                      |
| 1  | dot       | `.`                         | 7  | compare  | `==` `!=` `<` `>` `<=` `>=` |
| 2  | unary     | `-` `+` `!` `==`            | 8  | coalesce | `??`                         |
| 3  | range     | `..`                        | 9  | and      | `&&`                         |
| 4  | pow       | `**` (right-to-left)        | 10 | or       | `\|\|`                       |
| 5  | mul       | `*` `/` `//` `%`            | 11 | func call|                              |
Also: `~=` regex match. `/` float div, `//` integer div.
## Literals
```
42  5_000_000  3.14  5e9  0xff  0b1010  0o77     # numbers
true  false  null                                 # bool/null
'single'  "double"  """triple "quoted" """        # strings
f"Hello {name}"  s"SQL({col})"  r"\raw"           # f/s/r-strings
@2024-01-15  @14:30  @2024-01-15T14:30-08:00      # date/time/timestamp
3days  2years  1hours                              # duration
```
s-string tips: `{{`/`}}` for literal braces, `s"({x}) / 365"` for precedence.
## Joins
```
join side:left t (==id)                                    # shorthand
join side:left p=positions (this.id == that.emp_id)        # alias
join side:full other (true)                                # cross join
```
Sides: `inner` (default), `left`, `right`, `full`. Use `this`/`that` for table refs.
## Group, Aggregate, Window
```
group {dept} (aggregate {avg_sal = average salary, n = count this})
group role (sort date | take 1)                   # first per group
window rows:-2..0 (derive {ma = average val})     # sliding window
window rolling:3 (...)                            # = rows:-2..0
window expanding:true (derive {cum = sum val})    # = rows:..0
```
## Case, Functions, Declarations
```
derive x = case [a > 0 => "pos", true => "other"]           # case
let f = lo:0 hi x -> (x - lo) / (hi - lo)                   # func (lo default 0)
x | f 100                                                    # pipe into last arg
let t = (from employees | filter active)                     # named relation
into mid                                                     # name mid-pipeline
derive y = (col | as int)                                    # cast
filter (status | in ["a", "b"])                              # membership
select `column with spaces`                                  # quoted identifier
```
## stdlib
| Params             | Functions                                                                                                 |
|---------           |----------                                                                                                 |
| **text** `col`     | `lower` `upper` `trim` `ltrim` `rtrim` `length`                                                           |
| **text** `sub col` | `contains` `starts_with` `ends_with`                                                                      |
| **text** other     | `extract idx len col` · `replace before after col`                                                        |
| **math** `col`     | `abs` `floor` `ceil` `sqrt` `exp` `ln` `log10` `sin` `cos` `tan` `asin` `acos` `atan` `degrees` `radians` |
| **math** `n col`   | `round n col` · `pow b col` · `log b col`                                                                 |
| **math**           | `pi`                                                                                                      |
| **agg** `col`      | `min` `max` `sum` `average` `stddev` `count` `count_distinct` `any` `all` `concat_array`                  |
| **window** `col`   | `first` `last` `rank` `rank_dense` `row_number`                                                           |
| **window** `n col` | `lag` `lead`                                                                                              |
| **file** `path`    | `read_parquet` `read_csv` `read_json`                                                                     |
| **date**           | `date.to_text fmt col`                                                                                    |
## date format (`date.to_text fmt col`)
| Spec | Example    | Spec | Example   | Spec | Example              |
|----- |--------    |----- |---------  |----- |---------             |
| `%Y` | 2001       | `%d` | 08        | `%H` | 00                   |
| `%y` | 01         | `%-d`| 8         | `%M` | 34                   |
| `%m` | 07         | `%a` | Sun       | `%S` | 60                   |
| `%-m`| 7          | `%A` | Sunday    | `%f` | 264900               |
| `%b` | Jul        | `%F` | 2001-07-08| `%T` | 00:34:60             |
| `%B` | July       | `%D` | 07/08/01  | `%+` | 2001-07-08T00:34:60Z |
DuckDB target: `prqlc compile -t sql.duckdb` (makes `~=` use `REGEXP_MATCHES`).
