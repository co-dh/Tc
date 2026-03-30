import Lake
open Lake DSL

package tc

-- | Build lib{name}.a via make, tracking ALL c/*.c and c/*.h as deps.
def mkCLib (pkg : Package) (name : String) := do
  let cDir := pkg.dir / "c"
  let dst := cDir / s!"lib{name}.a"
  let isCH (p : System.FilePath) := p.extension == some "c" || p.extension == some "h"
  buildFileAfterDep dst (← inputDir cDir true isCH) fun _ =>
    proc { cmd := "make", args := #["-C", cDir.toString, s!"lib{name}.a"] }

-- | C libraries
extern_lib termbox2 pkg := mkCLib pkg "termbox2"
extern_lib termshim pkg := mkCLib pkg "termshim"
extern_lib adbcshim pkg := mkCLib pkg "adbcshim"
-- | include_str deps: Lake doesn't track these automatically
input_file funcsPrql where path := "Tc" / "Data" / "ADBC" / "funcs.prql"; text := true
lean_lib Tc where
  needs := #[funcsPrql]
  roots := #[`Tc.Nav, `Tc.Render, `Tc.Key, `Tc.App.Common, `Tc.App.Main,
             `Tc.Term, `Tc.Types, `Tc.Util, `Tc.View,
             `Tc.Meta, `Tc.Fzf, `Tc.Filter, `Tc.FileFormat, `Tc.Folder,
             `Tc.Theme, `Tc.Plot, `Tc.Export, `Tc.Transpose, `Tc.Join, `Tc.Diff, `Tc.Derive, `Tc.Split, `Tc.Sparkline, `Tc.Session, `Tc.StatusAgg, `Tc.Replay, `Tc.UI.Info, `Tc.UI.Preview, `Tc.Runner, `Tc.Ftp, `Tc.SourceConfig,
             `Tc.CmdConfig, `Tc.Data.Text, `Tc.Data.ADBC.Prql,
             `Tc.Data.ADBC.Table, `Tc.Data.ADBC.Ops]

-- | Main executable
@[default_target]
lean_exe tv where
  root := `Tc.App.Main

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

-- | Build test + tv, then run tests (test shells out to tv binary)
script runscreen args do
  let build ← IO.Process.spawn {
    cmd := "lake", args := #["build", "testscreen", "tv"]
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  if (← build.wait) != 0 then return 1
  let child ← IO.Process.spawn {
    cmd := ".lake/build/bin/testscreen"
    args := args.toArray
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  return ← child.wait

script runlarge args do
  let build ← IO.Process.spawn {
    cmd := "lake", args := #["build", "testlarge", "tv"]
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  if (← build.wait) != 0 then return 1
  let child ← IO.Process.spawn {
    cmd := ".lake/build/bin/testlarge"
    args := args.toArray
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  return ← child.wait

script runtest args do
  let build ← IO.Process.spawn {
    cmd := "lake", args := #["build", "test", "tv"]
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  if (← build.wait) != 0 then return 1
  let child ← IO.Process.spawn {
    cmd := ".lake/build/bin/test"
    args := args.toArray
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  return ← child.wait

