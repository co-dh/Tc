# Code Review (Arthur Whitney style)

## Tc/Data/Kdb/Table.lean

```
too much ceremony. 277 lines for simple table wrapper.

line 30-45: parseUrl - 16 lines for url parse. should be 3:
  parseUrl s = do
    let r := s.drop 6; let p := r.splitOn "/"; let h := p[0]!.splitOn ":"
    some (h[0]!, h.getD 1 "5001" |>.toNat!.toUInt16, p[1]!)

line 68-91: getCol - loop per cell. slow. should batch:
  Kdb.colData qr col  -- return entire column, not cell-by-cell

line 82-85: float parse broken. toNat? on "3.14" = none. silent bug.

line 142-145: sortCols logic unclear. comment says "isLast" but why?
  let isLast := i + 1 == n  -- magic

line 161-174: wrapTbl, extractPartFilter, extractTblName -
  3 functions doing string surgery. fragile. build AST instead.

line 185-211: queryFreq - 27 lines. too long. decompose.
```

## Tc/Render.lean

```
line 12-26: theorems good. prove bugs before fix. correct approach.

line 29-35: ViewState - 3 fields, 7 lines. too verbose:
  structure ViewState where rowOff colOff lastCol : Nat := 0

line 78-94: adjColOff - 17 lines for scroll calc. dense but ok.

line 97-128: render - 32 lines. too many locals:
  let colOff := ...
  let moveDir := ...
  let outWidths := ...
  let widths := ...
  let colName := ...
  let adj := ...
  let right := ...
  let pad := ...
  let status := ...

  inline them. 8 one-use locals = 8 lines wasted.
```

## Tc/Cmd.lean

```
good. Verb × Obj = 5 × 17 = 85 commands from 22 constructors.
isomorphism theorems: correct. parse ∘ toString = id.

line 66-70: objs array - repeated pattern:
  ('r', .row), ('c', .col), ...

could derive from Cmd constructors. reflection.

line 96-114: parse_toString proof - 17 cases × 5 subcases.
  brute force. works. native_decide handles it.
```

## c/kdb_shim.c

```
line 16: global g_handle. no. pass handle explicitly or thread-local.

line 205-218: format_timestamp broken:
  J y = 2000 + day / 365;  // computed, never used

  delete dead code.

line 221-224: format_date placeholder:
  snprintf(buf, sz, "%d", d);  // not a date, just int

  finish it or remove it.

line 228-247: format_vec_cell - switch on 15 types. ok but:
  - KP/KD/KT handlers inline. should call format_timestamp/format_date
  - duplicated null checks (nh, ni, nj). factor out.

line 272-291: lean_kdb_cell_str - cell-by-cell fetch.
  slow for large tables. add bulk column fetch.
```

## Test.lean

```
line 16-49: runKeys/runFolder - nearly identical. DRY:
  def run keys file := do
    let cmd := if file.isEmpty then "tc" else s!"tc \"{file}\""
    ...

line 55: contains via splitOn. use String.containsSubstr.

line 76-78: assert prints emoji. silent is golden:
  def assert c m := unless c do throw (IO.userError m)

test count: 80+. good coverage. keep.
```

## lakefile.lean

```
line 7-32: 4 extern_lib blocks, same pattern. factor:
  def mkLib name src := do
    let dst := pkg.dir / "c" / s!"lib{name}.a"
    buildFileAfterDep dst (←inputTextFile (pkg.dir / "c" / src)) fun _ =>
      proc { cmd := "make", args := #["-C", (pkg.dir / "c").toString, dst.fileName.get!] }

  extern_lib termbox2 pkg := mkLib "termbox2" "termbox2.h"
  extern_lib termshim pkg := mkLib "termshim" "term_shim.c"
  ...
```

## Summary

| Issue | Severity | Fix |
|-------|----------|-----|
| float parse broken (0.0 for all floats) | **critical** | use proper float parser |
| cell-by-cell fetch in kdb | **perf** | bulk column fetch |
| global g_handle | **design** | pass handle or TLS |
| 8+ one-use locals in render | **style** | inline |
| runKeys/runFolder duplication | **DRY** | factor common code |
| format_timestamp dead code | **cleanup** | delete unused `y` |
| extern_lib repetition | **DRY** | factor pattern |

## What's good

- Verb × Obj algebra: compact command space
- Theorems proving invariants: parse_toString, widthAdj_accumulates
- Effect separation: pure update returns Effect, IO runner interprets
- Test coverage: 80+ tests

## Final

```
too verbose for what it does.
kdb shim: 297 lines C for connect/query/cell.
q equivalent: 20 lines.

but architecture sound. typeclass abstraction correct.
fix the float bug. add bulk fetch. delete dead code.
```
