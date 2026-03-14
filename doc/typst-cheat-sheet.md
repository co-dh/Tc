# Typst Cheat Sheet
`= Introduction` `*bold*` `_italic_` `$E = m c^2$` `#set text(size: 12pt)` — markup mode by default; `#` escapes to code mode.
## Text Formatting
| Markup         | Result                    || Markup              | Result                         |
|--------------- |---------                  |--|-------------------- |---------                       |
| `*bold*`       | **bold**                  || `@label`            | reference a `<label>`          |
| `_italic_`     | _italic_                  || `<label>`           | attach label to element        |
| `` `raw` ``    | monospace                 || `#link("url")[text]` | hyperlink                      |
| `"smart"`      | "smart quotes"            || `#footnote[text]`   | footnote                       |
| `--` / `---`   | en dash / em dash         || `#sub[x]` `#super[x]`| subscript / superscript       |
| `\`            | line break                || `#highlight[text]`  | highlight                      |
| `~`            | non-breaking space        || `#underline[text]`  | underline                      |
| `https://...`  | auto-linked URL           || `#strike[text]`     | strikethrough                  |
## Headings & Structure
```typ
= Heading 1                                     // top-level
== Heading 2                                     // sub-heading
=== Heading 3                                    // sub-sub-heading
#outline()                                       // table of contents
#pagebreak()                                     // new page
#v(1em)                                          // vertical space
#h(1em)                                          // horizontal space
#colbreak()                                      // next column
```
## Lists
```typ
- Item one                    + First             / Term: description
- Item two                    + Second             / Key: value
  - Nested                      + Sub-item
```
Unordered (`-`), ordered (`+`), term list (`/`). Indent 2 spaces to nest.
## Math Mode
```typ
Inline $x^2 + y^2 = z^2$ within text.
$ sum_(i=0)^n i = (n(n+1)) / 2 $                // display (space after $)
$ f(x) = cases(x &"if" x >= 0, -x &"if" x < 0) $
$ A = mat(1, 2; 3, 4) $                         // matrix
$ ||x|| = sqrt(x_1^2 + x_2^2) $                 // norm, subscript
$ dot(x), hat(x), arrow(x), tilde(x) $          // accents
```
| Symbol    | Typst          || Symbol     | Typst           || Symbol     | Typst          |
|---------- |-------         |--|---------- |--------         |--|---------- |-------         |
| α β γ δ   | `alpha` `beta` `gamma` `delta` || ∈ ⊂ ∀ ∃  | `in` `subset` `forall` `exists` || → ⇒ ↦ | `arrow.r` `arrow.r.double` `|->` |
| π σ θ ω   | `pi` `sigma` `theta` `omega`   || ∞ ∂ ∇    | `infinity` `partial` `nabla`     || ≤ ≥ ≠ ≈ | `<=` `>=` `!=` `approx` |
| ∑ ∏ ∫     | `sum` `product` `integral`     || ℝ ℤ ℕ    | `RR` `ZZ` `NN`                  || ⋅ × ∘   | `dot` `times` `compose`  |
## Functions & Modes
```typ
#func(arg1, arg2, named: value)                  // function call
#text(fill: red)[bold content]                   // trailing content block
#{                                               // code block
  let x = 1
  [Result: #x]
}
#set text(size: 12pt)                            // set rule (applies forward)
#set par(justify: true, leading: 0.65em)
#show heading: set text(navy)                    // show rule (transform)
#show "LaTeX": [Typst]                           // text replacement
#show heading: it => block[#it.body]             // custom transform
#[#set text(red); this is scoped]                // scoped set rule
```
## Page Setup
```typ
#set page(paper: "a4", margin: (x: 2cm, y: 2.5cm))
#set page(
  header: align(right)[My Document],
  footer: context align(center, counter(page).display("1 / 1", both: true)),
  numbering: "1",
  columns: 2,
)
#set text(font: "New Computer Modern", size: 11pt, lang: "en")
#set par(justify: true, first-line-indent: 1.5em)
```
**Paper sizes**: `"a4"`, `"us-letter"`, `"a5"`, `"presentation-16-9"`, or `(width: 15cm, height: 20cm)` custom.
## Tables & Figures
```typ
#table(
  columns: (auto, 1fr, 1fr),                    // 3 columns
  stroke: 0.5pt,                                 // border
  align: (left, center, right),                  // per-column align
  table.header[*Name*][*Value*][*Unit*],          // bold header row
  [Alpha], [1.0], [m/s],
  [Beta],  [2.5], [kg],
)
#figure(
  image("plot.png", width: 80%),                 // image with width
  caption: [Measured results],
) <fig:results>                                  // label for @fig:results
#figure(table(...), caption: [Data summary])     // table in figure
```
## Imports & Packages
```typ
#import "utils.typ": my_func, my_var             // from local file
#import "@preview/polylux:0.3.1": *              // from package registry
#include "chapter.typ"                           // inline file content
```
Packages: `@preview/name:version`. Browse at https://typst.app/universe.
## Scripting
```typ
#let x = 42                                      // variable
#let greet(name) = [Hello, #name!]               // function
#let add(a, b) = a + b                           // pure function
#greet("World")                                  // call → "Hello, World!"

#if x > 0 [positive] else [non-positive]         // conditional
#for i in range(5) [Item #i. ]                   // loop (0..4)
#for (k, v) in (a: 1, b: 2) [#k=#v ]            // dict iteration

#let nums = (1, 2, 3, 4)                         // array
#nums.len()  #nums.at(0)  #nums.rev()            // methods
#nums.filter(x => x > 2)                         // → (3, 4)
#nums.map(x => x * 2)                            // → (2, 4, 6, 8)
#nums.join(", ")                                 // → "1, 2, 3, 4"
#let d = (name: "a", val: 1)                     // dictionary
#d.at("name")  #d.keys()  #d.values()            // access
```
## Common Functions
| Function                  | Purpose            || Function                    | Purpose            |
|-------------------------- |--------            |--|---------------------------- |--------            |
| `text(fill: ..)[..]`     | styled text        || `image("f", width: ..)`     | embed image        |
| `align(center)[..]`      | alignment          || `line(length: 100%)`        | horizontal rule    |
| `box(..)[..]`             | inline container   || `rect(..)[..]`              | rectangle          |
| `block(..)[..]`           | block container    || `circle(..)[..]`            | circle             |
| `grid(columns: n, ..)`   | grid layout        || `ellipse(..)[..]`           | ellipse            |
| `stack(dir: ttb, ..)`    | stacked layout     || `rotate(10deg)[..]`         | rotate content     |
| `pad(x: 1em)[..]`        | padding            || `scale(80%)[..]`            | scale content      |
| `place(dx: .., dy: ..)` | absolute position  || `hide[..]`                  | invisible content  |
| `numbering("1.1", ..)`   | format numbers     || `counter(heading).display()`| show counter       |
| `heading(level: 2)[..]` | programmatic heading|| `enum.item(5)[..]`         | custom enum start  |
## Units & Colors
| Length | Description           || Color                   | Description          |
|------- |-----------            |--|------------------------ |-----------           |
| `pt`   | point (1/72 in)       || `rgb("#ff0000")`         | hex color            |
| `mm`   | millimeter            || `rgb(255, 0, 0)`        | RGB components       |
| `cm`   | centimeter            || `luma(80%)`             | grayscale            |
| `in`   | inch                  || `oklch(50%, 0.2, 180deg)`| perceptual color    |
| `em`   | relative to font size || `red` `blue` `green`    | named colors         |
| `%`    | relative to container || `black` `white` `navy`  | named colors         |
| `fr`   | fraction of space     || `eastern` `maroon` `teal`| named colors        |
| `1pt + 2em` | combined        || `color.mix((red, 50%), (blue, 50%))` | mix   |

**Relative**: `100%` = full container width/height; `1fr` = fraction of remaining space in `grid`/`columns`.
## Typography (`#set text(...)`)
| Option              | Example                  || Option              | Example                 |
|--------             |---------                 |--|--------             |---------                |
| `font`              | `"New Computer Modern"`  || `fill`              | `blue` / `rgb("#333")`  |
| `size`              | `11pt` / `1.2em`         || `tracking`          | `0.5pt`                 |
| `weight`            | `"bold"` / `700`         || `spacing`           | `100%`                  |
| `style`             | `"italic"` / `"normal"`  || `lang`              | `"en"` / `"de"`         |
| `number-type`       | `"lining"` / `"old-style"`|| `hyphenate`        | `true` / `false`        |
| `ligatures`         | `true` / `false`         || `top-edge` `bottom-edge` | `"ascender"` `"descender"` |
## Useful Patterns
```typ
// two-column layout
#grid(columns: (1fr, 1fr), gutter: 1em,
  [Left column content],
  [Right column content],
)

// numbered headings
#set heading(numbering: "1.1")

// custom template
#let conf(title: "", body) = {
  set page(paper: "us-letter", margin: 2cm)
  set text(font: "Linux Libertine", size: 11pt)
  align(center, text(17pt, weight: "bold", title))
  body
}
#show: conf.with(title: "My Paper")

// access page number, total pages
#context counter(page).display("1 / 1", both: true)
```
CLI: `typst compile file.typ` (PDF), `typst watch file.typ` (live reload), `typst init @preview/template` (scaffold).
