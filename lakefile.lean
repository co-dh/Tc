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
lean_lib Tc where
  roots := #[`Tc.Cmd, `Tc.Nav, `Tc.Render, `Tc.Key, `Tc.App.Common,
             `Tc.Term, `Tc.Types, `Tc.Error, `Tc.TmpDir, `Tc.View,
             `Tc.Meta, `Tc.Fzf, `Tc.Filter, `Tc.Folder,
             `Tc.Theme, `Tc.Plot, `Tc.Export, `Tc.Transpose, `Tc.Join, `Tc.Derive, `Tc.Sparkline, `Tc.StatusAgg, `Tc.UI.Info, `Tc.UI.Preview, `Tc.Runner, `Tc.Remote, `Tc.SourceConfig,
             `Tc.Data.Text, `Tc.Data.ADBC.Prql,
             `Tc.Data.ADBC.Table, `Tc.Data.ADBC.Ops]

-- | Main executable
@[default_target]
lean_exe tc where
  root := `Tc.App.Common

-- | Test library (pure + runtime + screen backup tests)
lean_lib TcTest where
  roots := #[`test.TestUtil, `test.TestPure, `test.Test, `test.TestScreen, `test.TestLargeData]

-- | Screen backup tests (logic covered by pure theorems, kept for rendering validation)
lean_exe testscreen where
  root := `test.TestScreen

-- | Test executable (DuckDB backend tests)
lean_exe test where
  root := `test.Test

-- | Large-data tests (gitignored files: sample.parquet, nyse, pac.csv)
lean_exe testlarge where
  root := `test.TestLargeData

-- | Copy funcs.prql next to tc binary (runtime dependency)
def copyFuncsPrql : IO Unit := do
  IO.FS.writeFile ".lake/build/bin/funcs.prql" (← IO.FS.readFile "Tc/Data/ADBC/funcs.prql")

-- | Build test + tc, then run tests (test shells out to tc binary)
script runscreen args do
  let build ← IO.Process.spawn {
    cmd := "lake", args := #["build", "testscreen", "tc"]
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  if (← build.wait) != 0 then return 1
  copyFuncsPrql
  let child ← IO.Process.spawn {
    cmd := ".lake/build/bin/testscreen"
    args := args.toArray
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  return ← child.wait

script runlarge args do
  let build ← IO.Process.spawn {
    cmd := "lake", args := #["build", "testlarge", "tc"]
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  if (← build.wait) != 0 then return 1
  copyFuncsPrql
  let child ← IO.Process.spawn {
    cmd := ".lake/build/bin/testlarge"
    args := args.toArray
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  return ← child.wait

script runtest args do
  let build ← IO.Process.spawn {
    cmd := "lake", args := #["build", "test", "tc"]
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  if (← build.wait) != 0 then return 1
  copyFuncsPrql
  let child ← IO.Process.spawn {
    cmd := ".lake/build/bin/test"
    args := args.toArray
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  return ← child.wait

