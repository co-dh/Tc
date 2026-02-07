# tc - Terminal Table Viewer

VisiData-style terminal table viewer written in Lean 4, with DuckDB backend.

## Features

- CSV, Parquet, JSON support (via DuckDB)
- S3 bucket browsing (`s3://bucket/path`)
- Kdb+ table access (`kdb://host:port/table`)
- Folder browser with recursive depth control
- Frequency view (group by + count/pct/bar)
- Column metadata view (type, count, distinct, null%, min, max)
- Filter expressions (`col == val && col2 > 10`)
- Fuzzy search via fzf (columns, rows, filter, commands)
- Multi-column sorting (asc/desc)
- Column grouping (key columns pinned left)
- Row/column selection
- Line/bar plot export (via gnuplot)
- Theme support
- Stdin pipe mode (`cat data.csv | tc`)
- Zero-copy rendering via C FFI (termbox2)

## Build

```bash
lake build tc
```

## Run

```bash
tc data.csv                   # CSV file
tc data.parquet               # Parquet file
tc .                          # Browse current directory
tc s3://bucket/prefix         # Browse S3 bucket
tc s3://bucket/path/file.csv  # Open S3 file directly
tc s3://bucket/prefix +n      # S3 public bucket (no credentials)
tc kdb://localhost:5001/trade  # Kdb+ table
cat data.csv | tc             # Pipe mode (stdin)
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

| Key | Action |
|-----|--------|
| `F` | Frequency view (group by key + cursor column) |
| `M` | Column metadata view |
| `Enter` | Enter (open file in folder, filter from freq, set key from meta) |
| `q` / `Esc` | Pop view (quit if last) |
| `S` | Swap top two views |
| `Q` | Quit |

### Selection and Grouping

| Key | Action |
|-----|--------|
| `t` | Toggle column selection |
| `T` | Toggle row selection |
| `!` | Toggle key column (group) |

### Sorting and Editing

| Key | Action |
|-----|--------|
| `[` | Sort ascending |
| `]` | Sort descending |
| `d` | Delete column (+ selected cols) |

### Search

| Key | Action |
|-----|--------|
| `/` | Search (fzf) |
| `n` | Next match |
| `N` | Previous match |
| `\` | Filter expression |
| `s` | Column jump (fzf) |
| `Space` | Command palette (fzf) |

### Meta View (M)

| Key | Action |
|-----|--------|
| `0` | Select all-null columns |
| `1` | Select single-value columns |
| `Enter` | Set selected as key columns, pop to parent |

### Display

| Key | Action |
|-----|--------|
| `.` / `,` | Increase/decrease decimal precision |
| `>` / `<` | Widen/narrow columns |
| `I` | Toggle info overlay |

### Plot

| Key | Action |
|-----|--------|
| `p` | Line plot (cursor col vs first col) |
| `P` | Bar plot |

## Known Limitations

- Duration columns display as raw int64 (DuckDB ADBC limitation)
- Kdb+ requires `c.so` (kdb C library) at link time
