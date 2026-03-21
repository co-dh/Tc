# tv - Terminal Table Viewer

VisiData-style terminal table viewer written in Lean 4, with DuckDB backend.

![tv demo](doc/demo.gif)

## Features

- CSV, Parquet, JSON, DuckDB file support (via DuckDB)
- S3 bucket browsing (`s3://bucket/path`) with auto-caching for slow listings
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
- Heatmap coloring for all column types: numeric gradient, string categorical (`space m </>` cycles mode: 0=off, 1=numeric, 2=categorical, 3=both; default=1 numeric)
- Unix socket command channel (`$TV_SOCK`) — external tools send 2-char commands for live control
- Sparkline distribution row for numeric columns (on by default, `Z` to toggle)
- Regex column split (`:` key — split column by delimiter/regex into new columns)
- Theme support
- Stdin pipe mode (`cat data.csv | tv`)
- Table diff (compare top 2 views, auto-key categorical columns, hide same-value columns)
- Session save/load (persist filters, sorts, derives across sessions)
- Replay ops on tab line (shows PRQL pipeline; replay with `tv file -p "ops"`)
- Zero-copy rendering via C FFI (termbox2)

## Build

```bash
lake build tv
```

## Run

```bash
tv data.csv                        # CSV file
tv data.parquet                    # Parquet file
tv data.duckdb                     # DuckDB file (list tables)
tv .                               # Browse current directory
tv s3://bucket/prefix              # Browse S3 bucket
tv s3://bucket/path/file.csv       # Open S3 file directly
tv s3://bucket/prefix +n           # S3 public bucket (no credentials)
tv hf://datasets/user/dataset      # HuggingFace Hub dataset
tv osquery://                      # Browse osquery tables
tv osquery://processes             # Query osquery table directly
cat data.csv | tv                  # Pipe mode (stdin)
tv -s mysession                    # Restore saved session
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
| `Backspace`  | Go to parent directory (folder view)                             |
| `q` / `Esc` | Pop view (quit if last)                                          |
| `X`         | Transpose (swap rows and columns)                                |
| `J`         | Join top 2 views (inner/left/right join, union, set diff)        |
| `V`         | Diff top 2 views (auto-key, hide same columns, Δ prefix diffs)  |
| `S`         | Swap top two views                                               |
| `W`         | Save session (view stack to `~/.cache/tv/sessions/`)             |
| `L`         | Load session (restore saved view stack)                          |
| `Q`         | Quit                                                             |

### Clipboard

| Key | Action |
|-----|--------|
| `y` | Yank cell to clipboard |
| `Y` | Yank row to clipboard (tab-separated) |
| `Space y ~` | Yank cell |
| `Space y >` | Yank row |
| `Space y <` | Yank column (newline-separated) |

Auto-detects pbcopy (macOS), wl-copy (Wayland), xclip, or xsel.

### Export

| Key | Action |
|-----|--------|
| `e` | Export current view (fzf picker: csv, parquet, json) |

Exports to `~/tv_export_<name>.<fmt>`. Includes all filtered/sorted/grouped rows.

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
| `Space` | Command palette (flat fzf menu, bottom-anchored) |

### Meta View (M)

| Key     | Action                                     |
|-----    |--------                                    |
| `0`     | Select all-null columns                    |
| `1`     | Select single-value columns                |
| `Enter` | Set selected as key columns, pop to parent |

### Display

| Key | Action |
|-----|--------|
| `Space p >`/`<` | Increase/decrease decimal precision |
| `Space w >`/`<` | Widen/narrow columns |
| `Space T >`/`<` | Cycle themes |
| `I` | Toggle info overlay (context-specific hints) |

### Plot

Renders charts via R/ggplot2. Data is exported from DuckDB, downsampled if large, and rendered to PNG.

#### Setup

| Column | How to set | Type |
|--------|-----------|------|
| **X-axis** | Group a column with `!` | any (numeric, time, string) |
| **Y-axis** | Move cursor to it | numeric (int, float, decimal) |
| **Color** (optional) | Group a 2nd column with `!` | categorical (string) |
| **Facet** (optional) | Group a 3rd column with `!` | categorical (string) |

**Histogram** is the exception — no group columns needed, just cursor on a numeric column.

#### Keybindings

| Key | Action | Columns needed |
|-----|--------|----------------|
| `P.` | Line plot | `!` x-axis + cursor on numeric y |
| `P,` | Bar plot | `!` x-axis + cursor on numeric y |
| `Ps` | Scatter plot | `!` x-axis + cursor on numeric y |
| `Pb` | Boxplot | `!` x-axis + cursor on numeric y |
| `Ph` | Histogram | cursor on numeric column (no `!` needed) |

#### Example workflow

1. Press `!` on the column you want as x-axis (e.g. `Time`)
2. Move cursor to a numeric column for y-axis (e.g. `Price`)
3. Press `Ps` for scatter plot

To add color by category: also press `!` on a string column (e.g. `Symbol`).

To add facets (small multiples): group a 3rd column — it becomes the facet, and the 2nd group becomes color.

| Groups | X-axis | Color | Facet |
|--------|--------|-------|-------|
| 1 (`!`) | 1st group | — | — |
| 2 (`!!`) | 1st group | 2nd group | — |
| 3 (`!!!`) | 1st group | 3rd group | 2nd group |

#### Interactive controls

Once in plot view, keys control the chart in-place (no dialog, instant re-render):

| Key | Action |
|-----|--------|
| `h`/`l` | Cycle plot type (line → scatter → bar → box) |
| `.`/`,` | Coarser/finer downsampling (shown only for large data) |
| `q` | Exit back to the table |

For time-series x-axis, intervals cycle `1s → 1m → 1h → 1d`. For numeric x-axis, step multiplier increases `1x → 2x → 4x → 8x → 16x`.

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
| `socat`     | Socket preview in command palette       | preview disabled |

## Socket Command Channel

tv starts a Unix domain socket at `$TV_SOCK` (e.g. `/tmp/tv-12345.sock`).
External tools can send 2-char commands to control tv:

```bash
echo "m+" | socat - UNIX-CONNECT:$TV_SOCK   # heatmap: more color
echo "T+" | socat - UNIX-CONNECT:$TV_SOCK   # theme: next
echo "C+" | socat - UNIX-CONNECT:$TV_SOCK   # sort ascending
```

Command format: `{obj}{verb}` where obj is a single char (e.g. `m`=heat, `T`=theme, `C`=colSel)
and verb is `+`=inc, `-`=dec, `~`=toggle, `c`=dup, `d`=del.

The socket is per-process and cleaned up on exit.

## Known Limitations

- Duration columns display as raw int64 (DuckDB ADBC limitation)
