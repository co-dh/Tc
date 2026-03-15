# tc - Terminal Table Viewer

VisiData-style terminal table viewer written in Lean 4, with DuckDB backend.

## Features

- CSV, Parquet, JSON, DuckDB file support (via DuckDB)
- S3 bucket browsing (`s3://bucket/path`)
- HuggingFace Hub datasets (`hf://datasets/user/dataset`)
- Osquery table browser (`osquery://`)
- Folder browser with recursive depth control
- Frequency view (group by + count/pct/bar)
- Column metadata view (type, count, distinct, null%, min, max)
- Filter expressions via PRQL (`col == val && col2 > 10`)
- Fuzzy search via fzf (columns, rows, filter, commands)
- Multi-column sorting (asc/desc)
- Column grouping (key columns pinned left)
- Row/column selection, hidden columns
- Plotting via ggplot2: line, bar, scatter, histogram, boxplot — with faceting
- Status bar aggregation (sum/avg/count for current column)
- Heatmap coloring for numeric columns (`m` key)
- Sparkline distribution row for numeric columns (`Z` key)
- Regex column split (`:` key — split column by delimiter/regex into new columns)
- Theme support
- Stdin pipe mode (`cat data.csv | tc`)
- Session save/load (persist filters, sorts, derives across sessions)
- Zero-copy rendering via C FFI (termbox2)

## Build

```bash
lake build tc
```

## Run

```bash
tc data.csv                        # CSV file
tc data.parquet                    # Parquet file
tc data.duckdb                     # DuckDB file (list tables)
tc .                               # Browse current directory
tc s3://bucket/prefix              # Browse S3 bucket
tc s3://bucket/path/file.csv       # Open S3 file directly
tc s3://bucket/prefix +n           # S3 public bucket (no credentials)
tc hf://datasets/user/dataset      # HuggingFace Hub dataset
tc osquery://                      # Browse osquery tables
tc osquery://processes             # Query osquery table directly
cat data.csv | tc                  # Pipe mode (stdin)
tc -s mysession                    # Restore saved session
```

## Keybindings

### Navigation

| Key           | Action |
|---------------|--------|
| `j` / `↓`     | Down   |
| `k` / `↑`     | Up     |
| `h` / `←`     | Left   |
| `l` / `→`     | Right  |
| `J` / `PgDn`  | Page down |
| `K` / `PgUp`  | Page up |
| `H`           | Page left |
| `L`           | Page right |
| `gj`          | Bottom |
| `gk`          | Top    |
| `gh`          | First column |
| `gl`          | Last column |

### Views

| Key         | Action                                                           |
|-----        |--------                                                          |
| `F`         | Frequency view (group by key + cursor column)                    |
| `M`         | Column metadata view                                             |
| `Enter`     | Enter (open file in folder, filter from freq, set key from meta) |
| `q` / `Esc` | Pop view (quit if last)                                          |
| `X`         | Transpose (swap rows and columns)                                |
| `J`         | Join top 2 views (inner/left/right join, union, set diff)        |
| `S`         | Swap top two views                                               |
| `W`         | Save session (view stack to `~/.cache/tc/sessions/`)             |
| `L`         | Load session (restore saved view stack)                          |
| `Q`         | Quit                                                             |

### Export

| Key | Action |
|-----|--------|
| `e` | Export current view (fzf picker: csv, parquet, json) |

Exports to `~/tc_export_<name>.<fmt>`. Includes all filtered/sorted/grouped rows.

### Selection and Grouping

| Key | Action |
|-----|--------|
| `t` | Toggle column selection |
| `T` | Toggle row selection |
| `!` | Toggle key column (group) |
| `Shift+←/→` | Reorder key columns (for join ordering) |
| `H` | Toggle hide column |

### Sorting and Transforms

| Key | Action |
|-----|--------|
| `[` | Sort ascending |
| `]` | Sort descending |
| `=` | Derive column (PRQL expression) |
| `:` | Split column by delimiter/regex |

### Search

| Key | Action |
|-----|--------|
| `/` | Search (fzf) |
| `n` | Next match |
| `N` | Previous match |
| `\` | Filter expression (PRQL) |
| `s` | Column jump (fzf) |
| `Space` | Command palette (fzf) |

### Meta View (M)

| Key     | Action                                     |
|-----    |--------                                    |
| `0`     | Select all-null columns                    |
| `1`     | Select single-value columns                |
| `Enter` | Set selected as key columns, pop to parent |

### Display

| Key | Action |
|-----|--------|
| `Space p .`/`,` | Increase/decrease decimal precision |
| `Space w .`/`,` | Widen/narrow columns |
| `Space T .`/`,` | Cycle themes |
| `m` | Toggle heatmap (numeric columns) |
| `Z` | Toggle sparkline row (distribution bars for numeric columns) |
| `I` | Toggle info overlay (context-specific hints) |

### Plot

| Key | Action |
|-----|--------|
| `P.` | Line plot |
| `P,` | Bar plot |
| `Ps` | Scatter plot |
| `Ph` | Histogram |
| `Pb` | Boxplot |
| `+`/`-` | Change downsampling interval (in plot view) |
| `h`/`l` | Cycle plot type (in plot view) |

#### How it works

Plots use group columns (`!`) to define axes:

| Groups set | X-axis | Color | Facet |
|------------|--------|-------|-------|
| 0 groups | — | — | — |
| 1 group | 1st group col | — | — |
| 2 groups | 1st group col | 2nd group col | — |
| 3 groups | 1st group col | 3rd group col | 2nd group col |

Y-axis is always the column under the cursor (must be numeric).

**Histogram** (`Ph`) is special — it doesn't need any group columns. Just move the cursor to a numeric column and press `Ph`. R/ggplot2 auto-bins the values.

**All other plot types** require at least 1 group column for the x-axis. Example workflow:

1. Move to the column you want as x-axis, press `!` to group it
2. Move cursor to the numeric column you want as y-axis
3. Press `P.` for line, `P,` for bar, `Ps` for scatter, `Pb` for boxplot

**Adding color**: group a second column with `!`. Each unique value in that column gets a different color.

**Adding facets** (small multiples): group a third column. The plot splits into sub-charts, one per unique value of the 2nd group column. The 3rd group column becomes the color.

#### Interactive controls

The plot renders in-place and responds to keys immediately — no dialog, just re-renders the image:

| Key | Action |
|-----|--------|
| `+`/`=` | Coarser downsampling (fewer points, broader time buckets) |
| `-`/`_` | Finer downsampling (more points, narrower time buckets) |
| `l` | Next plot type (line → scatter → bar → box) |
| `h` | Previous plot type |
| `q`/any other | Exit back to the table |

For time-series data, intervals cycle through `1s → 1m → 1h → 1d`. For non-time data, the step multiplier increases (`1x → 2x → 4x → 8x → 16x`).

Switching plot type with `h`/`l` re-renders instantly with the same data — no need to exit and re-enter.

#### Display

Plot images are displayed using the best available method:

1. **Kitty graphics** (`kitten icat`) — pixel-perfect, works in kitty/WezTerm/ghostty
2. **viu** — half-block ANSI rendering, works in most terminals
3. **xdg-open** — opens in system image viewer

#### Requirements

Install R with ggplot2: `Rscript -e 'install.packages("ggplot2")'`

## Dependencies

Required:

| Tool    | Purpose                                    |
|------   |---------                                   |
| `prqlc` | PRQL → SQL query compilation               |
| `find`  | Folder browsing (GNU findutils `-printf`)  |
| `fzf`   | Fuzzy search, column jump, command palette |

Optional (feature-specific):

| Tool        | Feature                                | Fallback         |
|------       |---------                               |----------        |
| `Rscript`   | ggplot2 plot rendering                 | plot disabled    |
| `kitten`    | Kitty graphics protocol display        | `viu`            |
| `viu`       | Display plot PNG in terminal           | `xdg-open`       |
| `xdg-open`  | Open plot PNG in GUI viewer            | none             |
| `aws`       | S3 bucket browsing & download          | S3 disabled      |
| `curl`      | Hugging Face Hub file access           | HF disabled      |
| `jq`        | Parse HF API JSON responses            | HF disabled      |
| `bat`       | Syntax-highlighted file preview        | `less`           |
| `less`      | File preview (pager)                   | none             |
| `trash-put` | Move files to trash (folder view)      | `gio trash`      |
| `gio`       | Move files to trash (GNOME)            | none             |
| `stty`      | Terminal raw mode for plot interaction | —                |
| `osqueryi`  | Osquery table browsing & queries       | osquery disabled |
| `python3`   | Osquery table metadata setup           | osquery disabled |
| `realpath`  | Resolve folder paths                   | —                |
| `tmux`      | fzf popup mode (`--tmux`)              | fullscreen fzf   |

## Known Limitations

- Duration columns display as raw int64 (DuckDB ADBC limitation)
