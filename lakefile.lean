import Lake
open Lake DSL

package tc

-- | Build lib{name}.a from c/{src}, via make
def mkCLib (pkg : Package) (name src : String) := do
  let srcF := pkg.dir / "c" / src
  let dst := pkg.dir / "c" / s!"lib{name}.a"
  buildFileAfterDep dst (←inputTextFile srcF) fun _ =>
    proc { cmd := "make", args := #["-C", (pkg.dir / "c").toString, s!"lib{name}.a"] }

-- | C libraries
extern_lib termbox2 pkg := mkCLib pkg "termbox2" "termbox2.h"
extern_lib termshim pkg := mkCLib pkg "termshim" "term_shim.c"
-- ADBC: adbc_core.c (generic) + duckdb_driver.c (DuckDB init)
extern_lib adbcshim pkg := mkCLib pkg "adbcshim" "adbc_core.c"
extern_lib kdbshim pkg := mkCLib pkg "kdbshim" "kdb_shim.c"

lean_lib Tc where
  roots := #[`Tc.Offset, `Tc.Cmd, `Tc.Effect, `Tc.Nav, `Tc.Render, `Tc.Key, `Tc.App,
             `Tc.Term, `Tc.Types, `Tc.Error, `Tc.Op, `Tc.View, `Tc.ViewStack, `Tc.Dispatch,
             `Tc.Meta, `Tc.Freq, `Tc.Fzf, `Tc.Table, `Tc.Search, `Tc.Filter, `Tc.Folder,
             `Tc.Theme, `Tc.UI.Info, `Tc.Runner, `Tc.Data.CSV,
             `Tc.Data.Mem.Table, `Tc.Data.Mem.Text, `Tc.Data.Mem.Meta, `Tc.Data.Mem.Freq,
             `Tc.Data.ADBC.FFI, `Tc.Data.ADBC.Prql,
             `Tc.Data.ADBC.Table, `Tc.Data.ADBC.Meta,
             `Tc.Data.Kdb.FFI, `Tc.Data.Kdb.Q, `Tc.Data.Kdb.Table,
             `Tc.Backend, `Tc.Backend.Full, `Tc.Backend.Core,
             `Tc.Table.Mem, `Tc.Table.DuckDB, `Tc.Table.Full,
             `Tc.App.Core, `Tc.App.DuckDB]

-- | Full build: all backends (MemTable + ADBC + Kdb)
@[default_target]
lean_exe tc where
  root := `Tc.App

-- | Core build: MemTable only (CSV, no parquet/kdb)
lean_exe «tc-core» where
  root := `Tc.App.Core

-- | DuckDB build: MemTable + ADBC (CSV + parquet, no kdb)
lean_exe «tc-duckdb» where
  root := `Tc.App.DuckDB

-- | Test library (shared utilities and core tests)
lean_lib TestLib where
  roots := #[`test.TestLib, `test.CoreTest, `test.CoreTestMain]

-- | Test executable (spawns tc subprocess, parquet tests)
lean_exe test where
  root := `test.Test

-- | Core test executable (CSV-only tests, spawns tc-core)
lean_exe «core-test» where
  root := `test.CoreTestMain

-- | Kdb backend tests (requires localhost:8888/nbbo)
lean_exe «kdb-test» where
  root := `test.KdbTest

-- | Kdb key tests (UI tests, requires localhost:8888/nbbo)
lean_exe «kdb-key-test» where
  root := `test.KdbKeyTest

-- | Pure tests (compile-time checks via #guard)
lean_lib PureTest where
  roots := #[`test.PureTest]

