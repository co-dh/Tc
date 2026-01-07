import Lake
open Lake DSL

package tc

-- | Build libtermbox2.a from vendored termbox2.h
extern_lib termbox2 pkg := do
  let src := pkg.dir / "c" / "termbox2.h"
  let dst := pkg.dir / "c" / "libtermbox2.a"
  buildFileAfterDep dst (←inputTextFile src) fun _ => do
    proc { cmd := "make", args := #["-C", (pkg.dir / "c").toString, "libtermbox2.a"] }

-- | Build libtermshim.a tracking term_shim.c
extern_lib termshim pkg := do
  let src := pkg.dir / "c" / "term_shim.c"
  let dst := pkg.dir / "c" / "libtermshim.a"
  buildFileAfterDep dst (←inputTextFile src) fun _ => do
    proc { cmd := "make", args := #["-C", (pkg.dir / "c").toString, "libtermshim.a"] }

-- | Build libadbcshim.a tracking adbc_shim.c
extern_lib adbcshim pkg := do
  let src := pkg.dir / "c" / "adbc_shim.c"
  let dst := pkg.dir / "c" / "libadbcshim.a"
  buildFileAfterDep dst (←inputTextFile src) fun _ => do
    proc { cmd := "make", args := #["-C", (pkg.dir / "c").toString, "libadbcshim.a"] }

lean_lib Tc where
  roots := #[`Tc.Offset, `Tc.Cmd, `Tc.Effect, `Tc.Nav, `Tc.Render, `Tc.Key, `Tc.App,
             `Tc.Term, `Tc.Types, `Tc.Error, `Tc.Op, `Tc.View, `Tc.ViewStack, `Tc.Dispatch,
             `Tc.Meta, `Tc.Freq, `Tc.Fzf, `Tc.Table, `Tc.Search, `Tc.Filter, `Tc.Folder,
             `Tc.Theme, `Tc.UI.Info, `Tc.Runner, `Tc.Data.CSV,
             `Tc.Data.Mem.Table, `Tc.Data.Mem.Text, `Tc.Data.Mem.Meta, `Tc.Data.Mem.Freq,
             `Tc.Data.ADBC.FFI, `Tc.Data.ADBC.Prql,
             `Tc.Data.ADBC.Table, `Tc.Data.ADBC.Meta]

@[default_target]
lean_exe tc where
  root := `Tc.App

-- | Test executable (spawns tc subprocess)
lean_exe test where
  root := `Test

