import Lake
open Lake DSL

package tc

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
  roots := #[`Tc.Offset, `Tc.Nav, `Tc.Render, `Tc.Key, `Tc.App,
             `Tc.Term, `Tc.Types, `Tc.Error,
             `Tc.Data.Table, `Tc.Data.CSV, `Tc.Data.MemTable,
             `Tc.Data.ADBC.FFI, `Tc.Data.ADBC.Prql, `Tc.Data.ADBC.Backend, `Tc.Data.ADBC.Table]

@[default_target]
lean_exe tc where
  root := `Tc.App
  moreLinkArgs := #["-L/usr/local/lib", "-l:libtermbox2.a"]
