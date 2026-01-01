import Lake
open Lake DSL

package tc where
  moreLinkArgs := #["-L/usr/local/lib", "/usr/local/lib/libtermbox2.a"]

lean_lib Tc where
  roots := #[`Tc.Offset, `Tc.Nav, `Tc.Render, `Tc.Key, `Tc.App,
             `Tc.Term, `Tc.Types, `Tc.Error,
             `Tc.Data.Table, `Tc.Data.CSV, `Tc.Data.MemTable,
             `Tc.Data.ADBC.FFI, `Tc.Data.ADBC.Prql, `Tc.Data.ADBC.Backend, `Tc.Data.ADBC.Table]

@[default_target]
lean_exe tc where
  root := `Tc.App
  moreLinkArgs := #["c/libtermshim.a", "c/libadbcshim.a", "/usr/local/lib/libtermbox2.a"]
