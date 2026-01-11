import Lake
open Lake DSL

package tc

-- | Build lib{name}.a from c/{src}, via make
def mkCLib (pkg : Package) (name src : String) := do
  let srcF := pkg.dir / "c" / src
  let dst := pkg.dir / "c" / s!"lib{name}.a"
  buildFileAfterDep dst (←inputTextFile srcF) fun _ =>
    proc { cmd := "make", args := #["-C", (pkg.dir / "c").toString, s!"lib{name}.a"] }

-- | C libraries (shared by all targets)
extern_lib termbox2 pkg := mkCLib pkg "termbox2" "termbox2.h"
extern_lib termshim pkg := mkCLib pkg "termshim" "term_shim.c"
extern_lib kdbshim pkg := mkCLib pkg "kdbshim" "kdb_shim.c"

-- | ADBC libraries (target-specific, built via make)
-- Full build links libadbcshim.a, core build links libadbcstub.a
target adbcshim pkg : System.FilePath := do
  let dst := pkg.dir / "c" / "libadbcshim.a"
  let src := pkg.dir / "c" / "adbc_core.c"
  buildFileAfterDep dst (← inputTextFile src) fun _ =>
    proc { cmd := "make", args := #["-C", (pkg.dir / "c").toString, "libadbcshim.a"] }

target adbcstub pkg : System.FilePath := do
  let dst := pkg.dir / "c" / "libadbcstub.a"
  let src := pkg.dir / "c" / "adbc_stub.c"
  buildFileAfterDep dst (← inputTextFile src) fun _ =>
    proc { cmd := "make", args := #["-C", (pkg.dir / "c").toString, "libadbcstub.a"] }

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
             `Tc.Table.Mem, `Tc.Table.Full]

@[default_target]
lean_exe tc where
  root := `Tc.App
  moreLinkArgs := #["-L./c", "-ladbcshim", "-ldl"]

-- | Core build: CSV/stdin only (uses ADBC stub)
lean_exe «tc-core» where
  root := `Tc.App
  moreLinkArgs := #["-L./c", "-ladbcstub"]

-- | Core tests (CSV only, uses tc-core)
lean_exe «test-core» where
  root := `test.TestCore
  moreLinkArgs := #["-L./c", "-ladbcstub"]

-- | ADBC tests (parquet/folder, uses full tc)
lean_exe «test-adbc» where
  root := `test.TestAdbc
  moreLinkArgs := #["-L./c", "-ladbcshim", "-ldl"]

-- | Kdb backend tests (requires localhost:8888/nbbo)
lean_exe «kdb-test» where
  root := `test.KdbTest
  moreLinkArgs := #["-L./c", "-ladbcshim", "-ldl"]

-- | Kdb key tests (UI tests, requires localhost:8888/nbbo)
lean_exe «kdb-key-test» where
  root := `test.KdbKeyTest
  moreLinkArgs := #["-L./c", "-ladbcshim", "-ldl"]

-- | Pure tests (compile-time checks via #guard)
lean_lib PureTest where
  roots := #[`test.PureTest]

-- | Test utilities (shared between test-core and test-adbc)
lean_lib TestUtil where
  roots := #[`test.TestUtil]

