# PRQL Cheat Sheet
`from t | filter x > 0 | select {a, b} | sort {-a} | take 10` — pipe `|` chains transforms; newlines also work.
## Transforms
| Transform   | Syntax                            || Transform   | Syntax                           |
|------------ |--------                           |--|------------ |--------                          |
| `from`      | `from table`                      || `join`      | `join side:left t (==col)`       |
| `select`    | `select {col1, new = expr}`       || `group`     | `group {col} (aggregate {...})`  |
| `filter`    | `filter condition`                || `window`    | `window rows:-2..0 (derive ..)`  |
| `derive`    | `derive {new_col = expr}`         || `aggregate` | `aggregate {sum val, count x}`   |
| `sort`      | `sort {+col1, -col2}`             || `append`    | `append other` (UNION ALL)       |
| `take`      | `take 10` / `take 5..10`          || `remove`    | `remove other` (set difference)  |
## Operators (by precedence, tightest first)
| P  | Group     | Operators                        || P  | Group    | Operators                    |
|--- |---------- |-----------                       |--|--- |--------- |-----------                   |
| 0  | parens    | `()`                             || 6  | add      | `+` `-`                      |
| 1  | dot       | `.`                              || 7  | compare  | `==` `!=` `~=` `<` `>` `<=` `>=` `~=`|
| 2  | unary     | `-` `+` `!` `==`                 || 8  | coalesce | `??`                         |
| 3  | range     | `..`                             || 9  | and      | `&&`                         |
| 4  | pow       | `**` (right-to-left)             || 10 | or       | `\|\|`                       |
| 5  | mul       | `*` `/` `//` `%`                 || 11 | func call|                              |

## Literals
```
42  5_000_000  3.14  5e9  0xff  0b1010  0o77      # numbers
true  false  null                                 # bool/null
'single'  "double"  """triple "quoted" """        # strings
f"Hello {name}"  s"SQL({col})"  r"\raw"           # f/s/r-strings
@2024-01-15  @14:30  @2024-01-15T14:30-08:00      # date/time/timestamp
3days  2years  1hours                             # duration
```
s-string tips: `{{`/`}}` for literal braces, `s"({x}) / 365"` for precedence.
`from s"..."` requires the s-string to start with `SELECT` (prqlc limitation) — use `from s"SELECT * FROM func()"` not `from s"func()"`.
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
derive x = case [a > 0 => "pos", true => "other"]            # case
let f = lo:0 hi x -> (x - lo) / (hi - lo)                    # func (lo default 0)
x | f 100                                                    # pipe into last arg
let t = (from employees | filter active)                     # named relation
into mid                                                     # name mid-pipeline
derive y = (col | as int)                                    # cast
filter (status | in ["a", "b"])                              # membership
select `column with spaces`                                  # quoted identifier
```
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

## Grammar (from prqlc-parser, chumsky)
```ebnf
source        = [ query_def ] { statement } ;
query_def     = "prql" ( "version" ":" string | "target" ":" ident )+ ;

statement     = { annotation } { doc_comment } ( var_def | type_def | import_def | module_def ) ;
var_def       = "let" IDENT [ "<" type ">" ] "=" expr_call | pipeline [ "into" IDENT ] ;
type_def      = "type" IDENT "=" type ;
import_def    = "import" [ IDENT "=" ] ident ;
module_def    = "module" IDENT "{" { statement } "}" ;
annotation    = "@" IDENT [ expr ] ;

expr_call     = lambda | func_call | pipeline ;
pipeline      = expr ( "|" expr )+ | expr ;
func_call     = IDENT { expr } { IDENT ":" expr } ;    (* positional then named args *)
lambda        = param { param } "->" expr_call ;
param         = IDENT [ "<" type ">" ] [ ":" expr ] ;

expr          = unary | binary | atom ;
unary         = ( "+" | "-" | "!" | "==" ) expr ;
binary        = expr bin_op expr ;
atom          = literal | ident | tuple | array | case | interpolation | range | "(" expr ")" ;
tuple         = "{" [ aliased { "," aliased } ] "}" ;
array         = "[" [ expr { "," expr } ] "]" ;
case          = "case" "[" { expr "=>" expr "," } "]" ;
range         = [ expr ] ".." [ expr ] ;
interpolation = ("s" | "f") QUOTED_STRING ;          (* {expr} inside *)
aliased       = [ IDENT "=" ] expr ;

(* precedence high→low: **  * / // %  + -  == != < > <= >= ~=  ??  &&  || *)
bin_op        = "**" | "*" | "/" | "//" | "%" | "+" | "-"
              | "==" | "!=" | "<" | ">" | "<=" | ">=" | "~="
              | "??" | "&&" | "||" ;

ident         = IDENT { "." IDENT } ;
literal       = NUMBER | STRING | BOOL | NULL | DATE | TIME | TIMESTAMP | DURATION ;
```
