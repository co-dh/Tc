# tc - Typeclass Navigation Test App

Test app for typeclass-based navigation with VisiData-style keybindings.

## Keybindings

### Cursor

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

### Row Selection

| Key | Action   |
|-----|----------|
| `s` | Select   |
| `t` | Toggle   |
| `u` | Unselect |

### Column Selection

| Key | Action   |
|-----|----------|
| `S` | Select   |
| `T` | Toggle   |
| `U` | Unselect |

### Grouping

| Key | Action            |
|-----|-------------------|
| `!` | Toggle key column |

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
```
