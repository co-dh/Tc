# PRQL Cheat Sheet

## Pipeline

```
from employees | filter age > 30 | select {name, age} | sort age | take 10
```

Pipe `|` chains transforms. Newlines work too (no `|` needed between transforms on separate lines).

## Transforms

| Transform   | Syntax                            | Notes                       |
|-----------  |--------                           |-------                      |
| `from`      | `from table`                      | Source table                |
| `select`    | `select {col1, col2, new = expr}` | Pick/rename columns         |
| `filter`    | `filter condition`                | Keep matching rows          |
| `derive`    | `derive {new_col = expr}`         | Add computed columns        |
| `sort`      | `sort {+col1, -col2}`             | `+` asc (default), `-` desc |
| `take`      | `take 10` or `take 5..10`         | Limit rows or range         |
| `join`      | `join side:left t (==col)`        | See Joins below             |
| `group`     | `group {col} (pipeline)`          | See Group below             |
| `window`    | `window rows:-2..0 (pipeline)`    | See Windows below           |
| `aggregate` | `aggregate {sum val, count col}`  | Collapse to one row         |
| `append`    | `append other_table`              | UNION ALL                   |
| `intersect` | `intersect other_table`           | Set intersection            |
| `remove`    | `remove other_table`              | Set difference              |

## Operators (by precedence, tightest first)

| Prec | Group       | Operators                    | Associativity |
|----- |------------ |----------                    |-------------- |
| 0    | parentheses | `()`                         |               |
| 1    | dot         | `.`                          |               |
| 2    | unary       | `-` `+` `!` `==`             |               |
| 3    | range       | `..`                         |               |
| 4    | pow         | `**`                         | right-to-left |
| 5    | mul         | `*` `/` `//` `%`             | left-to-right |
| 6    | add         | `+` `-`                      | left-to-right |
| 7    | compare     | `==` `!=` `<=` `>=` `<` `>`  | left-to-right |
| 8    | coalesce    | `??`                         | left-to-right |
| 9    | and         | `&&`                         | left-to-right |
| 10   | or          | `\|\|`                       | left-to-right |
| 11   | func call   |                              |               |

Also: `~=` regex match, `/` float division, `//` integer division.

## Literals

```
42                  # int
5_000_000           # int with separators
3.14    5e9         # float
0xff  0b1010  0o77  # hex, binary, octal
true  false  null   # boolean, null
'hello'  "world"   # strings (single or double quotes)
@2024-01-15         # date
@14:30:00           # time
@2024-01-15T14:30   # timestamp
@2024-01-15T14:30:00-08:00  # timestamp with tz
3days  2years  1hours       # duration
```

## Strings

```
'basic string'
"also basic"
"""triple-quoted "contains quotes" inside"""

f"Hello {first_name} {last_name}"    # f-string: interpolate columns (names only, no exprs)
s"REGEXP_MATCHES({col}, 'pat')"      # s-string: raw SQL escape hatch
r"\no\escaping\here"                 # r-string: raw, no escape processing
```

**s-string tips**: `{{` `}}` for literal braces. Wrap interpolated vars in parens for precedence: `s"({x}) / 365"`.

## Joins

```
join side:inner other (==id)                         # self-equality shorthand
join side:left p=positions (this.id == that.emp_id)  # alias + explicit condition
join side:left t (this.id == that.id && that.country == 'UK')  # compound
join side:full other (true)                          # cross join
```

Sides: `inner` (default), `left`, `right`, `full`.

## Group + Aggregate

```
from employees
group {department} (
  aggregate {
    avg_sal = average salary,
    n = count this,
  }
)
```

Any pipeline works inside group — not just aggregate:

```
from employees
group role (
  sort join_date
  take 1
)
# → first-hired per role
```

## Window Functions

```
window rows:0..2 (derive {next2 = sum val})     # current + 2 following
window rows:-2..0 (derive {prev2 = sum val})     # 2 preceding + current
window rolling:3 (derive {ma = average val})     # alias for rows:-2..0
window expanding:true (derive {cumsum = sum val}) # alias for rows:..0
```

Window functions: `lag offset col`, `lead offset col`, `first`, `last`, `rank`, `rank_dense`, `row_number`.

## Aggregate Functions

`min` `max` `sum` `average` `stddev` `count` `any` `all` `concat_array`

## Case

```
derive category = case [
  score > 90 => "A",
  score > 80 => "B",
  score > 70 => "C",
  true => "F",            # default (else)
]
```

## Functions

```
let double = x -> x * 2
let clamp = lo:0 hi:100 x -> max lo (min hi x)   # named params with defaults

from t | derive {y = double x, z = clamp hi:50 x}
from t | derive {y = (x | double)}                # pipe into last positional arg
```

## Variables & Declarations

```
let t = from employees                # named relation
let result = (                        # multi-line
  from employees
  filter department == "Engineering"
)
into mid_result                       # name intermediate result in pipeline
```

## Type Casting

```
derive x = (col | as int)
derive y = (col | as text)
```

## Membership

```
filter (status | in ["active", "pending"])
```

## Identifiers

```
select `column with spaces`          # backtick-quoted
this.col                             # current table ref
that.col                             # joined table ref
```

## stdlib: text

| Function      | Params            | Description                                      |
|-------------- |--------           |--------------                                    |
| `lower`       | `col`             | Convert to lower case                            |
| `upper`       | `col`             | Convert to upper case                            |
| `trim`        | `col`             | Remove whitespace from both sides                |
| `ltrim`       | `col`             | Remove whitespace from left                      |
| `rtrim`       | `col`             | Remove whitespace from right                     |
| `length`      | `col`             | Number of characters                             |
| `contains`    | `sub col`         | True if col contains sub                         |
| `starts_with` | `sub col`         | True if col starts with sub                      |
| `ends_with`   | `sub col`         | True if col ends with sub                        |
| `extract`     | `idx len col`     | Substring at index idx (1-based) with length len |
| `replace`     | `before after col` | Replace occurrences of before with after         |

## stdlib: math

| Function  | Params    | Description            |
|---------- |--------   |-----------             |
| `abs`     | `col`     | Absolute value         |
| `floor`   | `col`     | Round down             |
| `ceil`    | `col`     | Round up               |
| `round`   | `n col`   | Round to n decimals    |
| `sqrt`    | `col`     | Square root            |
| `pow`     | `b col`   | col to the power b     |
| `exp`     | `col`     | e^col                  |
| `ln`      | `col`     | Natural logarithm      |
| `log10`   | `col`     | Base-10 logarithm      |
| `log`     | `b col`   | Base-b logarithm       |
| `pi`      |           | The constant π         |
| `sin`     | `col`     | Sine                   |
| `cos`     | `col`     | Cosine                 |
| `tan`     | `col`     | Tangent                |
| `asin`    | `col`     | Arcsine                |
| `acos`    | `col`     | Arccosine              |
| `atan`    | `col`     | Arctangent             |
| `degrees` | `col`     | Radians to degrees     |
| `radians` | `col`     | Degrees to radians     |

## stdlib: date

`date.to_text format col` — format a date/timestamp as text.

| Spec | Example          | Description                     |
|----- |--------          |-----------                      |
| `%Y` | 2001             | Year (4 digits)                 |
| `%y` | 01               | Year (2 digits)                 |
| `%m` | 07               | Month (01–12)                   |
| `%-m`| 7                | Month (1–12)                    |
| `%b` | Jul              | Month abbreviated               |
| `%B` | July             | Month full name                 |
| `%d` | 08               | Day (01–31)                     |
| `%-d`| 8                | Day (1–31)                      |
| `%a` | Sun              | Weekday abbreviated             |
| `%A` | Sunday           | Weekday full name               |
| `%F` | 2001-07-08       | ISO date (`%Y-%m-%d`)           |
| `%D` | 07/08/01         | US date (`%m/%d/%y`)            |
| `%H` | 00               | Hour (00–23)                    |
| `%I` | 12               | Hour 12h (01–12)                |
| `%p` | AM               | AM/PM                           |
| `%M` | 34               | Minute (00–59)                  |
| `%S` | 60               | Second (00–59)                  |
| `%f` | 264900           | Microseconds                    |
| `%T` | 00:34:60         | Time (`%H:%M:%S`)              |
| `%R` | 00:34            | Time (`%H:%M`)                 |
| `%+` | 2001-07-08T00:34:60.026490Z | ISO 8601 date+time  |
| `%%` |                  | Literal `%`                     |

## stdlib: aggregation

| Function       | Params     | Description              |
|--------------- |--------    |-----------               |
| `min`          | `col`      | Minimum value            |
| `max`          | `col`      | Maximum value            |
| `sum`          | `col`      | Sum                      |
| `average`      | `col`      | Mean                     |
| `stddev`       | `col`      | Standard deviation       |
| `count`        | `col`      | Count items              |
| `count_distinct`| `col`     | Count distinct values    |
| `any`          | `col`      | True if any true         |
| `all`          | `col`      | True if all true         |
| `concat_array` | `col`      | Concatenate as string    |

## stdlib: window

| Function     | Params         | Description                  |
|------------- |--------        |-----------                   |
| `lag`        | `offset col`   | Value from previous row      |
| `lead`       | `offset col`   | Value from next row          |
| `first`      | `col`          | First value in window        |
| `last`       | `col`          | Last value in window         |
| `rank`       | `col`          | Rank (with gaps for ties)    |
| `rank_dense` | `col`          | Rank (no gaps)               |
| `row_number` | `col`          | Sequential row number        |

## stdlib: file

| Function       | Params  | Description       |
|--------------- |-------- |-----------        |
| `read_parquet` | `path`  | Read parquet file |
| `read_csv`     | `path`  | Read CSV file     |
| `read_json`    | `path`  | Read JSON file    |

## DuckDB target

Compile with: `prqlc compile -t sql.duckdb`

This ensures `~=` compiles to `REGEXP_MATCHES()` instead of generic `REGEXP()`.
