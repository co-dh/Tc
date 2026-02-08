# Code Review — Arthur Whitney

3500 lines of Lean for a table viewer. plus 1000 for backends. should be 500.

## good

Cmd algebra. `(obj,verb)` — 18 objects, 5 verbs. 2-char serialization with proven roundtrip. this is the right idea. compact command space, orthogonal. the `exec` match table in Nav.lean:132-147 is clean:

```
let r d := some { nav with row := { nav.row with cur := nav.row.cur.clamp d } }
| .row .inc => r 1    | .row .dec => r (-1)
| .col .inc => c 1    | .col .dec => c (-1)
```

that's how you write a command dispatch. data, not code.

ViewStack as `(hd, tl)`. push/pop/swap are one-liners. no proofs needed, the structure makes it obvious.

Column as typed arrays — `.ints`, `.floats`, `.strs` — not `Array Cell`. avoids per-cell tag. right choice for a viewer.

## too much

**35 .lean files.** a table viewer is: read, navigate, render, filter, sort. that's 5 things. you have 35 files. Dispatch.lean is 56 lines. Error.lean is 32 lines. Remote.lean is 25 lines. these aren't files, they're paragraphs. merge them.

**proofs that prove nothing useful.** `exec_row_inc_from_zero`, `exec_ver_inc_idempotent`, `exec_ver_dec_idempotent` — these are 60 lines proving that clamped arithmetic works. omega knows. you know. the reader knows. the only proof that earns its keep is `dispOrder_size` because you need it for the type checker, and `toggle_toggle_not_mem` because the property isn't obvious. the rest is decoration.
`width_inc_adds` and `width_dec_subs` are `rfl`. they're comments pretending to be theorems. `parse_toString` enumerates 18 constructors x 5 verbs = 90 cases of `native_decide`. when it breaks you won't know why. one wrong char in `objs` and the proof still passes via brute force — it doesn't localize the error.

**NavState carries two proofs that nobody reads.** `hRows : TblOps.nRows tbl = nRows` and `hCols`. every construction site passes `rfl rfl`. the proofs exist to satisfy Fin. but Fin is the wrong choice — a clamped Nat with `min` at the boundary does the same thing without lifting values to the type level. you're paying for dependent types (8 lines of `have hltc : c < nCols := ...` in `newAt`) to get what `min cursor (n-1)` gives you for free.

**RenderCtx has 14 fields.** that's a function with 14 arguments wearing a trenchcoat. half of them (`r0`, `r1`, `nGrp`, `colOff`) are derived from others. compute them at the call site.

**TblOps has 12 methods, 6 with defaults.** a typeclass with `plotExport` and `fromUrl` alongside `nRows` is not a coherent interface. it's a god object. `plotExport` takes 7 args. that's a function that doesn't know what it wants to be.

**Effect type is over-engineered.** 6 sub-enums (`FzfEffect`, `QueryEffect`, `FolderEffect`, `SearchEffect`, `PlotEffect`, `MetaEffect`) wrapped in a parent enum. `Effect.fzf (.col)` to say "open column picker". a flat enum with 20 variants would be shorter, clearer, and equally type-safe. the sub-grouping buys you nothing — Runner still matches every variant.

**`MetaTuple` is 7 parallel arrays.** classic struct-of-arrays but without the performance benefit (small data, Lean GC). should be one array of structs, or better: just a table. you already have tables. meta IS a table. you build one in `Meta.push` via `fromArrays`. skip the intermediate struct.

## missing

**no pipeline.** you have `Op` (filter, sort, sel, derive, group, take) and `ExecOp` but they're unused in the main flow. sort goes through `ModifyTable.sort`, filter through `TblOps.filter`. the Op algebra exists but doesn't compose. if you had `tbl |> filter "x>5" |> sort ["a"] |> take 100` as a first-class pipeline, half the Effect/Runner machinery disappears.

**no undo.** ViewStack gives you push/pop but sort and delete mutate in place via `setCur`. after a bad sort you can't go back. push before mutate — it's one line.

## numbers

```
35 files, 4500 lines (with backends)
 ~800 lines of proofs (toggle, dispOrder, cumWidth, nav theorems, parse roundtrip, render bounds)
 ~600 lines of types/structures/typeclasses
 ~500 lines of actual logic (nav, dispatch, filter, render offsets)
 ~400 lines of Runner/effect plumbing
 ~200 lines of imports/namespace/boilerplate
```

the ratio is wrong. 500 lines of logic wrapped in 4000 lines of scaffolding. the proofs should justify themselves in bugs caught, not lines written.

## summary

the core ideas are right — `(obj,verb)` commands, typed columns, pure update + IO effect, stack of views. but the code is 5-10x larger than it needs to be. too many files, too many abstractions, too many proofs of obvious things. dependent types for cursor bounds is a net negative — the complexity it adds exceeds the bugs it prevents. flatten the Effect hierarchy, merge small files, delete the ornamental proofs, replace `Fin` with clamped `Nat`. the 500 lines of actual logic should be what's left.

## done

- **Dispatch.lean merged** into App/Common.lean (56 → 0 lines). App.lean merged too.
- **MetaTuple eliminated.** queryMeta returns AdbcTable directly. parquet uses `parquet_metadata()` (instant, no data scan). non-parquet uses raw SQL. no intermediate struct, no column extraction in Lean.
- **Meta selection uses SQL/PRQL** instead of getCols + Lean filter. selNull/selSingle/setKey query `tc_meta` temp table via composable PRQL. all DuckDB code lives in ADBC/Meta.lean.
- **Kdb meta** still uses placeholder zeros — real q stats pending.
- **RenderCtx 14 fields**: analyzed. `r0`, `r1`, `colOff` are viewport-derived at the single call site, `nGrp` is `dispIdxs` partition size. removing them pushes recomputation into each backend's render impl or requires a viewport sub-struct. net complexity increase for no real gain — left as-is.
- **Pipeline / ExecOp**: `Op` type (filter, sort, sel, derive, group, take) is alive — used by `Query.pipe` and `Op.render` in both ADBC (PRQL) and Kdb (q) backends. But `ExecOp` typeclass was dead code — defined with instances for AdbcTable, KdbTable, Table, but `exec` never called externally. The backends already compose pipelines internally via `Query.pipe` + `requery` (sort, delCols, filter all use this directly). Deleted `ExecOp` typeclass and all instances. `Op` and `Query.pipe` remain — they're the actual pipeline.
