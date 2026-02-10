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
-- ADBC: adbc_core.c (generic ADBC + DuckDB driver)
extern_lib adbcshim pkg := mkCLib pkg "adbcshim" "adbc_core.c"
extern_lib kdbshim pkg := mkCLib pkg "kdbshim" "kdb_shim.c"

lean_lib Tc where
  roots := #[`Tc.Cmd, `Tc.Nav, `Tc.Render, `Tc.Key, `Tc.App.Common,
             `Tc.Term, `Tc.Types, `Tc.Error, `Tc.TmpDir, `Tc.View,
             `Tc.Meta, `Tc.Freq, `Tc.Fzf, `Tc.Table, `Tc.Search, `Tc.Filter, `Tc.Folder,
             `Tc.Theme, `Tc.Plot, `Tc.UI.Info, `Tc.Runner, `Tc.Remote, `Tc.S3, `Tc.HF,
             `Tc.Data.Text, `Tc.Data.ADBC.FFI, `Tc.Data.ADBC.Prql,
             `Tc.Data.ADBC.Table, `Tc.Data.ADBC.Meta, `Tc.Data.ADBC.Ops,
             `Tc.Data.Kdb.FFI, `Tc.Data.Kdb.Q, `Tc.Data.Kdb.Table, `Tc.Data.Kdb.Ops]

-- | Full build: all backends (ADBC + Kdb)
@[default_target]
lean_exe tc where
  root := `Tc.App.Common

-- | Test executable (spawns tc subprocess; pass --kdb for kdb tests)
lean_exe test where
  root := `test.Test

