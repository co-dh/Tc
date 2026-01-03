# tc - Tabular Viewer in Lean

VisiData-style terminal CSV viewer with typeclass-based navigation.

## Features

- Typed column storage (int64, float, string)
- Zero-copy rendering via C FFI
- Column grouping (key columns)
- Row/column selection
- Column deletion

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
.lake/build/bin/tc data.csv
.lake/build/bin/tc data.csv -c "rn rn cn gt"  # play commands then interactive
```

## Command String

Commands follow Obj+Verb pattern (2 chars each, space-separated):

| Obj | Verb | Meaning |
|-----|------|---------|
| `r` | `n/p/N/P/h/e` | row next/prev/pgNext/pgPrev/home/end |
| `c` | `n/p/N/P/h/e/d` | col next/prev/pgNext/pgPrev/home/end/del |
| `R` | `t` | rowSel toggle |
| `C` | `t` | colSel toggle |
| `g` | `t` | grp toggle |

Example: `rn rn cn Ct gt` = down, down, right, toggle col selection, toggle group
