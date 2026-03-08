# Pure C Plugin Architecture (Not Implemented)

Design for .so plugins in pure C. Saved for reference.

## Overview

```
tc-core (main binary, ~25MB)
├── MemTable (CSV/stdin)
├── Plugin loader (dlopen)
└── Typeclass interface

Plugins (pure C, ~5MB each):
├── duckdb.so   - DuckDB embedded (parquet, local SQL)
├── adbc.so     - Generic ADBC (Snowflake/Postgres drivers)
└── kdb.so      - kdb+ IPC (q expressions)
```

## C Plugin Interface

```c
// tc_plugin.h
#define TC_PLUGIN_VERSION 1

typedef void* tc_table;

typedef struct {
    int version;
    const char* name;           // "duckdb", "adbc", "kdb"
    const char* schemes;        // "file:parquet" or "adbc://" or "kdb://"

    // Lifecycle
    int  (*init)(void);
    void (*shutdown)(void);

    // Open/close
    tc_table (*open)(const char* uri);
    void (*close)(tc_table t);

    // ReadTable
    int64_t (*nrows)(tc_table t);
    int64_t (*ncols)(tc_table t);
    const char* (*col_name)(tc_table t, int64_t col);
    char (*col_type)(tc_table t, int64_t col);

    // Cell access
    int64_t (*cell_int)(tc_table t, int64_t row, int64_t col);
    double (*cell_float)(tc_table t, int64_t row, int64_t col);
    const char* (*cell_str)(tc_table t, int64_t row, int64_t col);

    // QueryTable
    const char* (*query_meta)(tc_table t);
    const char* (*query_freq)(tc_table t, const char* sql);
    tc_table (*filter)(tc_table t, const char* sql);
    const char* (*distinct)(tc_table t, int64_t col);
    int64_t (*find_row)(tc_table t, int64_t col, const char* val, int64_t start, int fwd);

    // ModifyTable
    tc_table (*del_cols)(tc_table t, const int64_t* cols, int ncols);
    tc_table (*sort_by)(tc_table t, const int64_t* cols, int ncols, int asc);
} tc_plugin_t;

extern tc_plugin_t tc_plugin;
```

## Lean Integration

```lean
-- Tc/Plugin.lean
opaque PluginTable : Type

@[extern "tc_plugin_nrows"]
opaque pluginNrows : PluginTable → IO Nat

-- Table = mem | plugin
inductive Table where
  | mem : MemTable → Table
  | plugin : PluginTable → Table
```

## PRQL Flow

Core compiles PRQL → SQL, sends SQL to plugins (except kdb uses q).

## File Structure

```
c/
├── tc_plugin.h
├── plugin_loader.c

plugins/
├── duckdb/duckdb_plugin.c
├── adbc/adbc_plugin.c
└── kdb/kdb_plugin.c
```

## Usage

```bash
tc data.csv                      # MemTable
tc data.parquet                  # duckdb.so
tc "adbc://snowflake/db/table"   # adbc.so
tc "kdb://localhost:5000/trades" # kdb.so
```
