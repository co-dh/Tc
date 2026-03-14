# Typst Cheat Sheet
`= Title` `*bold*` `_italic_` `` `code` `` `$x^2$` `#let x = 1` — three modes: markup (default), math `$…$`, code `#…`.
## Markup
| Syntax              | Element          || Syntax                  | Element          |
|--------             |--------          |--|--------                 |--------          |
| `= Heading`         | heading (depth=`=` count) || `- item`         | bullet list      |
| `*bold*`            | strong emphasis  || `+ item`                | numbered list    |
| `_italic_`          | emphasis         || `/ Term: desc`          | term list        |
| `` `code` ``        | raw/monospace    || `\`                     | line break       |
| `https://…`         | auto-link        || `~`                     | non-breaking space |
| `<label>`           | label            || `---` `--` `...`        | em/en dash, ellipsis |
| `@label`            | reference        || `#expr`                 | embedded code    |
| `"quote"`           | smart quotes     || `/*…*/` `//…`           | comment          |

## Math
Inline `$x+1$` (no spaces inside `$`), block `$ x+1 $` (spaces inside `$`).
```
$x^2$  $x_1$  $x_1^2$                        # super/subscript
$a/b$  $(a+b) / (c+d)$                       # fraction (auto from /)
$sqrt(x)$  $root(3, x)$                      # roots
$sum_(i=0)^n i$  $integral_0^1 f(x) dif x$   # big operators
$vec(a, b, c)$  $mat(1, 2; 3, 4)$            # vector, matrix
$x &= 2 \ &= 3$                              # alignment (&) + newline (\)
$arrow.r$  $alpha$  $pi$  $bb(R)$             # symbols, variants
$a "is even"$                                 # text in math
$floor(x)$  $ceil(x)$  $abs(x)$  $norm(v)$   # delimiters
```
Symbols: `alpha`…`omega`, `Alpha`…`Omega`, `NN`, `ZZ`, `RR`, `CC`, `QQ`. Variants: `bb` `cal` `frak` `mono` `sans` `serif`.

## Code — Literals & Containers
```
none  auto  true  false                       # singletons
42  0xff  0b1010  0o77                        # integers
3.14  1e-5                                    # floats
"hello"  "a \"b\""                            # strings (double-quote only)
2pt  3mm  1em  1in  1cm                       # lengths
90deg  1rad                                   # angles
50%  2fr                                      # ratio, fraction
(1, 2, 3)  ("a",)                             # array (trailing , for 1-elem)
(a: 1, b: 2)                                  # dictionary
{let x = 1; x + 2}                            # code block
[*Hello* world]                               # content block
```

## Operators (by precedence, tightest first)
| P | Group     | Operators                               || P | Group   | Operators                  |
|---|--------   |---------                                |--|---|-------- |---------                   |
| 0 | unary     | `-` `+`                                 || 4 | compare | `==` `!=` `<` `>` `<=` `>=`|
| 1 | mul       | `*` `/`                                 || 5 | member  | `in` `not in`              |
| 2 | add       | `+` `-`                                 || 6 | logic   | `not` `and` `or`           |
| 3 | assign    | `=` `+=` `-=` `*=` `/=`                 ||   |         |                            |

## Functions, Bindings, Destructuring
```
let x = 1                                     # variable
let f(x, y) = x + y                           # named function
let g = (x, y) => x + y                       # anonymous function
let (a, b) = (1, 2)                            # destructuring
let (first, .., last) = arr                    # rest pattern
let _ = expr                                   # discard
```

## Control Flow
```
if x == 1 { "one" } else if x == 2 { "two" } else { "other" }
for x in (1, 2, 3) { repr(x) }                # array
for (k, v) in dict { k + ": " + repr(v) }     # dictionary
while x < 10 { x += 1 }
break  continue  return x                      # loop/function control
```

## Set & Show Rules (styling)
```
set text(14pt, font: "New Computer Modern")    # set rule (applies to scope)
set text(red) if x > 5                         # conditional set
show heading: set text(blue)                   # show-set: style element type
show "keyword": strong                         # show rule: transform matches
show heading: it => [§ #it.body]               # show with custom function
show: columns.with(2)                          # show-everything: wrap all
```

## Modules & Packages
```
include "chapter.typ"                          # include content
import "utils.typ"                             # import as module
import "utils.typ": helper, fmt                # selective import
import "utils.typ": *                          # import all
import "utils.typ": helper as h                # rename
import "@preview/cetz:0.3.4"                   # package (namespace/name/ver)
```

## Key Functions by Category
| Category        | Functions                                                                                    |
|--------         |---------                                                                                     |
| **text**        | `text` `emph` `strong` `highlight` `underline` `overline` `strike` `sub` `super` `smallcaps` `raw` `lorem` |
| **model**       | `heading` `par` `list` `enum` `terms` `table` `figure` `footnote` `link` `cite` `bibliography` `outline` `quote` |
| **layout**      | `page` `block` `box` `grid` `columns` `stack` `align` `pad` `move` `rotate` `scale` `hide` `place` `measure` `v` `h` |
| **math**        | `equation` `frac` `sqrt` `root` `vec` `mat` `cases` `accent` `cancel` `binom` `lr` `attach` `limits` `scripts` |
| **visualize**   | `image` `line` `rect` `square` `circle` `ellipse` `polygon` `path` `curve` `gradient` `color` `stroke` |
| **introspect**  | `counter` `state` `query` `locate` `here` `metadata`                                        |
| **data**        | `read` `csv` `json` `toml` `yaml` `xml` `cbor`                                              |
| **foundations** | `type` `repr` `eval` `assert` `panic` `calc` `regex` `datetime` `duration` `sys` `plugin`   |

## Page & Document Setup
```
#set page(paper: "a4", margin: 2cm, header: [My Doc], numbering: "1")
#set text(12pt, font: "Linux Libertine", lang: "en")
#set par(justify: true, leading: 0.65em, first-line-indent: 1em)
#set heading(numbering: "1.1")
```

## Tables & Figures
```
#table(
  columns: (1fr, 2fr, auto),                  // fraction/auto widths
  align: (left, center, right),
  table.header[*Name*][*Age*][*City*],
  [Alice], [30], [NYC],
  [Bob],   [25], [LA],
)
#figure(image("plot.svg", width: 80%), caption: [Results]) <fig:res>
See @fig:res.                                  // cross-reference
```

## Useful Patterns
```
#let note(body) = block(fill: luma(230), inset: 8pt, radius: 4pt, body)
#note[This is a callout.]                      # custom element
#numbering("I", 4)                             # → "IV"
#counter(heading).display()                    # current heading number
#context text.lang                             # access contextual state
#{1 + 2}  #(if true { "yes" })                 # code in markup (parens for if/for)
```
CLI: `typst compile doc.typ` → PDF.  `typst watch doc.typ` → auto-recompile.  `typst init @preview/template` → from package.
