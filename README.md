# tv - Terminal Table Viewer

VisiData-style terminal table viewer written in Lean 4, with DuckDB backend.

![folder](doc/folder.gif)

## Features

tv opens CSV, Parquet, JSON, Arrow, DuckDB, SQLite, and Excel files.
It can also browse S3 buckets, HuggingFace datasets, and osquery tables.

### Folder browser

Point tv at a directory to browse files. Enter opens a file or subfolder,
Backspace goes to the parent, `[`/`]` sorts columns.

![folder](doc/folder.gif)

### Sparklines

Every column header shows a tiny sparkline of the value distribution.
You can see at a glance which columns are skewed, uniform, or sparse.

![sparkline](doc/sparkline.gif)

### Frequency view

Press `F` to see how many times each value appears in the current column.
Select a value and press Enter to filter the table to only those rows.

![freq](doc/freq.gif)

### Heatmap

Toggle heatmap coloring through the Space menu (`hea`).
Mode 1 colors numeric columns by value, mode 2 colors categorical columns,
mode 3 colors both.

![heatmap](doc/heatmap.gif)

### Plotting

Move the cursor to a numeric column, open the Space menu, and pick a plot type.
Charts are rendered with R/ggplot2 and displayed in the terminal.

![plot](doc/plot.gif)

![plot example](doc/plot-example.png)

### Command menu

Press Space to open a fuzzy-search command menu. Type to filter,
Enter to run. This is how you access most features.

![fzf](doc/fzf.gif)

<!-- ### Themes

Cycle through color themes from the Space menu (`th`).

![theme](doc/theme.gif) -->

### Column metadata

Press `M` to see every column's type, null count, and distinct values.
Press `0` to select columns with nulls, `1` for single-value columns,
then Enter to hide them from the main table.

![meta](doc/meta.gif)

### Sorting

Press `[` to sort the current column ascending, `]` for descending.

![sort](doc/sort.gif)

### Column split

Press `:` to split a column by a delimiter. Type the delimiter
(e.g. `-`) and press Enter. New columns appear for each part.

![split](doc/split.gif)

### Filter

Press `\` to open the PRQL filter prompt. Type an expression like
`Bid_Price > 100` and press Enter. Only matching rows remain.

![filter](doc/filter.gif)

### Derive column

Press `=` to create a computed column. Type an expression like
`Bid_Price * 2` and press Enter. The new column appears on the right.

![derive](doc/derive.gif)

### Table diff

To compare two tables, open the first file, press `S` to swap back to the
folder, open the second file, then press `V`. tv joins them on matching
columns and shows what changed. Identical columns are hidden, changed columns
get a `Δ` prefix.

![diff](doc/diff.gif)

### Remote sources

Browse S3 buckets (`tv s3://bucket/ +n`) and HuggingFace datasets
(`tv hf://datasets/user/dataset`) the same way you browse local folders.

### Also

- Column grouping with `!` (key columns pinned left, used as x-axis for plots)
- Row/column selection and hidden columns
- Status bar shows sum/avg/count for the current column
- Pipe mode: `cat data.csv | tv`
- Session save (`W`) and load (`L`)
- Tab line shows the PRQL pipeline; replay with `tv file -p "ops"`
- Socket control channel (`$TV_SOCK`) for scripting

## Install

Download the latest release from [GitHub Releases](https://github.com/co-dh/Tc/releases):

```bash
curl -fsSL https://github.com/co-dh/Tc/releases/latest/download/tv-linux-amd64.tar.gz | tar xz
sudo cp libduckdb.so /usr/local/lib/ && sudo ldconfig
cp tv ~/.local/bin/   # or anywhere on PATH
```

Requires glibc 2.25+ (Ubuntu 18.04+, Debian 10+, RHEL 8+, any distro from ~2018).

## Build from source

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
| `Space V >`   | Bottom |
| `Space V <`   | Top    |
| `Space H <`   | First column |
| `Space H >`   | Last column |

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
| `Space y ~` | Yank cell to clipboard |
| `Space y >` | Yank row (tab-separated) |
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
