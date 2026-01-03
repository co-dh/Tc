# tc - Tabular Viewer in Lean

VisiData-style terminal CSV viewer with typeclass-based navigation.

## Features

- CSV and Parquet support (via ADBC/DuckDB)
- Typed column storage (int64, float, string, bool, date)
- Zero-copy rendering via C FFI
- Column separators (│ between cols, ║ after keys)
- Type indicators: `#` int, `%` float, `?` bool, `@` date
- Complex types: lists `[n] v1; v2`, structs `{n} name=val`
- Column grouping (key columns)
- Row/column selection
- Column deletion
- Multi-column sorting (asc/desc)

## Known Limitations

- Duration columns display as raw int64 (DuckDB ADBC limitation)

## Keybindings

### Navigation

| Key           | Action |
|---------------|--------|
| `j` / `↓`     | Down   |
| `k` / `↑`     | Up     |
| `h` / `←`     | Left   |
| `l` / `→`     | Right  |
| `J` / `PgDn`  | Page ↓ |
| `K` / `PgUp`  | Page ↑ |
| `H`           | Page ← |
| `L`           | Page → |
| `gj` / `g↓`   | Bottom |
| `gk` / `g↑`   | Top    |
| `gh` / `g←`   | First  |
| `gl` / `g→`   | Last   |

### Selection

| Key | Action        |
|-----|---------------|
| `t` | Toggle column |
| `T` | Toggle row    |

### Grouping

| Key | Action            |
|-----|-------------------|
| `!` | Toggle key column |

### Sorting

| Key | Action                     |
|-----|----------------------------|
| `[` | Sort ascending by column   |
| `]` | Sort descending by column  |

Sorts by key (group) columns if any, otherwise by current column.

### Editing

| Key | Action                          |
|-----|---------------------------------|
| `d` | Delete column (+ selected cols) |

### Other

| Key         | Action |
|-------------|--------|
| `q` / `Esc` | Quit   |

## Build

```bash
lake build tc
```

## Run

```bash
.lake/build/bin/tc data.csv                    # CSV file
.lake/build/bin/tc data.parquet                # Parquet file (via DuckDB)
.lake/build/bin/tc data.csv -c "rn rn cn gt"   # play commands then interactive
```

## Command String

Commands follow Obj+Verb pattern (2 chars each, space-separated):

| Obj | Verb | Meaning |
|-----|------|---------|
| `r` | `n/p/N/P/h/e` | row next/prev/pgNext/pgPrev/home/end |
| `c` | `n/p/N/P/h/e/d` | col next/prev/pgNext/pgPrev/home/end/del |
| `R` | `t` | rowSel toggle |
| `C` | `t/[/]` | colSel toggle/sortAsc/sortDesc |
| `g` | `t` | grp toggle |

Example: `rn rn cn Ct C[` = down, down, right, toggle col selection, sort ascending by selected
