/-
  Pure tests for state machine logic.
  No IO - all tests are compile-time checks via #guard.
-/
import Tc.Nav
import Tc.View
import Tc.UI.Info
import Tc.Types
import Tc.Validity
import Tc.S3
import Tc.Data.Mem.Text

namespace PureTest

open Tc

/-! ## Mock Table for Testing -/

-- | Mock table with fixed dimensions (phantom types)
structure MockTable (nRows nCols : Nat) where
  names : Array String

-- | TblOps instance for MockTable (minimal: just nRows/colNames + dummy render)
instance : TblOps (MockTable nRows nCols) where
  nRows _ := nRows
  colNames t := t.names
  queryMeta _ := pure (#[], #[], #[], #[], #[], #[], #[])
  queryFreq _ _ := pure ⟨#[], #[], #[], #[], #[], 0, rfl, ⟨rfl, rfl⟩⟩
  filter _ _ := pure none
  distinct _ _ := pure #[]
  findRow _ _ _ _ _ := pure none
  render _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ := pure #[]

-- | Create mock 5x3 table for testing
def mock53 : MockTable 5 3 := ⟨#["c0", "c1", "c2"]⟩

/-! ## NavState Tests -/

section NavTests

-- 5 rows, 3 cols
def testNav : NavState 5 3 (MockTable 5 3) :=
  NavState.new mock53 rfl rfl (by decide) (by decide)

-- | j (row.inc) moves cursor from 0 to 1
#guard (NavState.exec (.row .inc) testNav 1 1).map (·.row.cur.val) == some 1

-- | k (row.dec) at row 0 stays at 0 (clamped)
#guard (NavState.exec (.row .dec) testNav 1 1).map (·.row.cur.val) == some 0

-- | l (col.inc) moves cursor from 0 to 1
#guard (NavState.exec (.col .inc) testNav 1 1).map (·.col.cur.val) == some 1

-- | h (col.dec) at col 0 stays at 0 (clamped)
#guard (NavState.exec (.col .dec) testNav 1 1).map (·.col.cur.val) == some 0

-- | G (ver.inc) goes to last row (4)
#guard (NavState.exec (.ver .inc) testNav 1 1).map (·.row.cur.val) == some 4

-- | g (ver.dec) goes to first row (0)
def navAtRow4 : NavState 5 3 (MockTable 5 3) :=
  match NavState.exec (.ver .inc) testNav 1 1 with
  | some n => n
  | none => testNav
#guard (NavState.exec (.ver .dec) navAtRow4 1 1).map (·.row.cur.val) == some 0

-- | $ (hor.inc) goes to last col (2)
#guard (NavState.exec (.hor .inc) testNav 1 1).map (·.col.cur.val) == some 2

-- | 0 (hor.dec) goes to first col (0)
def navAtCol2 : NavState 5 3 (MockTable 5 3) :=
  match NavState.exec (.hor .inc) testNav 1 1 with
  | some n => n
  | none => testNav
#guard (NavState.exec (.hor .dec) navAtCol2 1 1).map (·.col.cur.val) == some 0

-- | Page down (vPage.inc) with page size 2 moves from 0 to 2
#guard (NavState.exec (.vPage .inc) testNav 2 1).map (·.row.cur.val) == some 2

-- | Page up (vPage.dec) at row 0 stays at 0
#guard (NavState.exec (.vPage .dec) testNav 2 1).map (·.row.cur.val) == some 0

-- | T (rowSel.ent) toggles row selection
#guard (NavState.exec (.rowSel .ent) testNav 1 1).map (·.row.sels) == some #[0]

-- | T twice removes selection
def navWithSel : NavState 5 3 (MockTable 5 3) :=
  match NavState.exec (.rowSel .ent) testNav 1 1 with
  | some n => n
  | none => testNav
#guard (NavState.exec (.rowSel .ent) navWithSel 1 1).map (·.row.sels) == some #[]

-- | ! (grp.ent) toggles group
#guard (NavState.exec (.grp .ent) testNav 1 1).map (·.grp) == some #["c0"]

-- | Unhandled command returns none
#guard (NavState.exec (.thm .inc) testNav 1 1).isNone

-- | update returns Effect.none for nav commands
#guard (NavState.update (.row .inc) testNav 1 1).map (·.2) == some .none

end NavTests

/-! ## Array.toggle Tests -/

section ToggleTests

-- | toggle adds element if absent
#guard #[1, 2].toggle 3 == #[1, 2, 3]

-- | toggle removes element if present
#guard #[1, 2, 3].toggle 2 == #[1, 3]

-- | toggle on empty array adds element
#guard (#[] : Array Nat).toggle 1 == #[1]

-- | toggle twice returns original (when not present initially)
#guard ((#[1, 2] : Array Nat).toggle 3).toggle 3 == #[1, 2]

end ToggleTests

/-! ## ViewStack.update Tests -/

section StackTests

-- For ViewStack tests we need a View, which requires more setup
-- These are simpler property tests

-- | Fin.clamp stays in bounds
#guard (⟨0, by decide⟩ : Fin 5).clamp 10 == ⟨4, by decide⟩
#guard (⟨4, by decide⟩ : Fin 5).clamp (-10) == ⟨0, by decide⟩
#guard (⟨2, by decide⟩ : Fin 5).clamp 1 == ⟨3, by decide⟩
#guard (⟨2, by decide⟩ : Fin 5).clamp (-1) == ⟨1, by decide⟩

end StackTests

/-! ## Info.State Tests -/

section InfoTests

def infoOff : UI.Info.State := { vis := false }
def infoOn : UI.Info.State := { vis := true }

-- | Default is on
#guard (UI.Info.State.mk).vis == true

-- | I toggles info visibility
#guard (UI.Info.State.update infoOff (.info .ent)).map (·.1.vis) == some true
#guard (UI.Info.State.update infoOn (.info .ent)).map (·.1.vis) == some false

-- | info.inc turns on
#guard (UI.Info.State.update infoOff (.info .inc)).map (·.1.vis) == some true

-- | info.dec turns off
#guard (UI.Info.State.update infoOn (.info .dec)).map (·.1.vis) == some false

-- | Unhandled returns none
#guard (UI.Info.State.update infoOff (.row .inc)).isNone

-- | Info update returns Effect.none
#guard (UI.Info.State.update infoOff (.info .ent)).map (·.2) == some .none

end InfoTests

/-! ## dispOrder Tests -/

section DispOrderTests

-- | Empty group: order unchanged
#guard dispOrder #[] #["a", "b", "c"] == #[0, 1, 2]

-- | Group first col: moves to front
#guard dispOrder #["b"] #["a", "b", "c"] == #[1, 0, 2]

-- | Group multiple: group columns come first (in column order)
#guard dispOrder #["c", "a"] #["a", "b", "c"] == #[0, 2, 1]

-- | Group non-existent: ignored
#guard dispOrder #["x"] #["a", "b", "c"] == #[0, 1, 2]

end DispOrderTests

/-! ## Validity Tests -/

section ValidityTests

-- | Navigation always valid
#guard validFor (.row .inc) .tbl == true
#guard validFor (.row .dec) .colMeta == true
#guard validFor (.vPage .inc) (.freqV #["a"] 10) == true
#guard validFor (.stk .dec) (.fld "/tmp" 1) == true

-- | freq.ent requires freqV
#guard validFor (.freq .ent) (.freqV #["col1"] 42) == true
#guard validFor (.freq .ent) .tbl == false
#guard validFor (.freq .ent) .colMeta == false
#guard validFor (.freq .ent) (.fld "/" 1) == false

-- | metaV.ent requires colMeta
#guard validFor (.metaV .ent) .colMeta == true
#guard validFor (.metaV .ent) .tbl == false
#guard validFor (.metaV .ent) (.freqV #[] 0) == false

-- | fld.ent requires fld
#guard validFor (.fld .ent) (.fld "/home" 2) == true
#guard validFor (.fld .ent) .tbl == false
#guard validFor (.fld .ent) .colMeta == false

-- | Push commands valid everywhere
#guard validFor (.metaV .dup) .tbl == true
#guard validFor (.metaV .dup) .colMeta == true
#guard validFor (.freq .dup) .tbl == true
#guard validFor (.freq .dup) (.fld "." 1) == true
#guard validFor (.fld .dup) .tbl == true

-- | col.ent (fzf col search) is universal
#guard validFor (.col .ent) .tbl == true
#guard validFor (.col .ent) .colMeta == true

end ValidityTests

/-! ## CSV/TSV Parsing Tests -/

section CsvTests

-- | fromTsv parses header names correctly
#guard (MemTable.fromTsv "a\tb\n1\t2\n3\t4").toOption.map (·.names) == some #["a", "b"]

-- | fromTsv parses correct number of columns
#guard (MemTable.fromTsv "a\tb\n1\t2\n3\t4").toOption.map (·.cols.size) == some 2

-- | fromTsv single column
#guard (MemTable.fromTsv "x\n10\n20\n30").toOption.map (·.names) == some #["x"]

-- | fromTsv preserves column count in names
#guard (MemTable.fromTsv "c1\tc2\tc3\n1\t2\t3").toOption.map (·.names.size) == some 3

-- | fromTsv empty input returns empty table
#guard (MemTable.fromTsv "").toOption.map (·.names.size) == some 0

-- | fromTsv row count (via nRows)
#guard (MemTable.fromTsv "a\tb\n1\t2\n3\t4").toOption.map MemTable.nRows == some 2

-- | fromTsv single row
#guard (MemTable.fromTsv "a\n1").toOption.map MemTable.nRows == some 1

-- | fromTsv header-only has zero data rows
#guard (MemTable.fromTsv "a\tb\n").toOption.map MemTable.nRows == some 0

end CsvTests

/-! ## Column Operation Tests -/

section ColumnTests

-- | Column.size for ints
#guard (Column.ints #[10, 20, 30]).size == 3

-- | Column.size for strs
#guard (Column.strs #["a", "b"]).size == 2

-- | Column.size for floats
#guard (Column.floats #[1.0, 2.0, 3.0, 4.0]).size == 4

-- | Column.size for empty
#guard (Column.ints #[]).size == 0

-- | Column.take preserves first n elements (check via get + toRaw)
#guard ((Column.ints #[10, 20, 30]).take 2).size == 2
#guard (((Column.ints #[10, 20, 30]).take 2).get 0).toRaw == "10"
#guard (((Column.ints #[10, 20, 30]).take 2).get 1).toRaw == "20"

-- | Column.take with n > size returns all
#guard ((Column.strs #["a", "b"]).take 5).size == 2

-- | Column.take 0 returns empty
#guard ((Column.ints #[10, 20, 30]).take 0).size == 0

-- | Column.gather reindexes correctly (ints)
#guard ((Column.ints #[10, 20, 30]).gather #[2, 0]).size == 2
#guard (((Column.ints #[10, 20, 30]).gather #[2, 0]).get 0).toRaw == "30"
#guard (((Column.ints #[10, 20, 30]).gather #[2, 0]).get 1).toRaw == "10"

-- | Column.gather reindexes correctly (strs)
#guard (((Column.strs #["a", "b", "c"]).gather #[2, 0]).get 0).toRaw == "c"
#guard (((Column.strs #["a", "b", "c"]).gather #[2, 0]).get 1).toRaw == "a"

-- | Column.gather with empty indices returns empty column
#guard ((Column.ints #[10, 20, 30]).gather #[]).size == 0

-- | Column.gather duplicate indices
#guard ((Column.ints #[10, 20]).gather #[0, 0, 1, 1]).size == 4
#guard (((Column.ints #[10, 20]).gather #[0, 0, 1, 1]).get 2).toRaw == "20"

-- | Column.get returns correct Cell.toRaw for strs
#guard ((Column.strs #["hello", "world"]).get 0).toRaw == "hello"
#guard ((Column.strs #["hello", "world"]).get 1).toRaw == "world"

-- | Column.get out of bounds returns default (empty/0)
#guard ((Column.ints #[10]).get 5).toRaw == "0"
#guard ((Column.strs #["a"]).get 5).toRaw == ""

end ColumnTests

/-! ## S3 Path Helper Tests -/

section S3Tests

-- | isS3 recognizes s3:// prefix
#guard S3.isS3 "s3://bucket/path" == true
#guard S3.isS3 "s3://my-bucket" == true

-- | isS3 rejects non-S3 paths
#guard S3.isS3 "/local/path" == false
#guard S3.isS3 "http://example.com" == false
#guard S3.isS3 "" == false

-- | parent strips last component
#guard S3.parent "s3://bucket/a/b/" == some "s3://bucket/a/"
#guard S3.parent "s3://bucket/a/" == some "s3://bucket/"

-- | parent returns none at bucket root
#guard S3.parent "s3://bucket/" == none

-- | parent without trailing slash
#guard S3.parent "s3://bucket/a/b" == some "s3://bucket/a/"

-- | join with trailing slash
#guard S3.join "s3://b/a/" "x" == "s3://b/a/x"

-- | join without trailing slash adds separator
#guard S3.join "s3://b/a" "x" == "s3://b/a/x"

-- | join preserves trailing slash on child
#guard S3.join "s3://b/" "subdir/" == "s3://b/subdir/"

end S3Tests

/-! ## FreqResult Construction Tests -/

section FreqTests

-- | freqStats produces pct array of same size as input
#guard (freqStats #[10, 20, 30]).val.1.size == 3

-- | freqStats produces bar array of same size as input
#guard (freqStats #[10, 20, 30]).val.2.size == 3

-- | freqStats with empty input
#guard (freqStats #[]).val.1.size == 0
#guard (freqStats #[]).val.2.size == 0

-- | freqStats with single element
#guard (freqStats #[100]).val.1.size == 1
#guard (freqStats #[100]).val.2.size == 1

-- | freqStats pct and bar arrays have same size as each other
#guard (freqStats #[5, 10, 15]).val.1.size == (freqStats #[5, 10, 15]).val.2.size

end FreqTests

/-! ## MemTable Invariant Tests -/

section MemTableInvariantTests

-- | Empty MemTable preserves h_eq
#guard (⟨#[], #[], rfl⟩ : MemTable).names.size == 0
#guard (⟨#[], #[], rfl⟩ : MemTable).cols.size == 0

-- | MemTable with matching columns preserves h_eq (1 col)
#guard (⟨#["x"], #[Column.ints #[1, 2, 3]], rfl⟩ : MemTable).names.size == 1

-- | MemTable with matching columns preserves h_eq (2 cols)
#guard (⟨#["a", "b"], #[Column.ints #[1], Column.strs #["x"]], rfl⟩ : MemTable).cols.size == 2

-- | MemTable.nRows returns row count from first column
#guard MemTable.nRows ⟨#["a"], #[Column.ints #[10, 20, 30]], rfl⟩ == 3

-- | MemTable.nRows for empty table
#guard MemTable.nRows ⟨#[], #[], rfl⟩ == 0

-- | MemTable.take preserves h_eq and row count
#guard (MemTable.take ⟨#["a", "b"], #[Column.ints #[1, 2, 3], Column.strs #["x", "y", "z"]], rfl⟩ 2).names.size
    == (MemTable.take ⟨#["a", "b"], #[Column.ints #[1, 2, 3], Column.strs #["x", "y", "z"]], rfl⟩ 2).cols.size

-- | MemTable.take reduces nRows
#guard MemTable.nRows (MemTable.take ⟨#["a"], #[Column.ints #[1, 2, 3, 4, 5]], rfl⟩ 3) == 3

-- | MemTable.selCols preserves h_eq (names.size = cols.size)
#guard (MemTable.selCols ⟨#["a", "b", "c"], #[Column.ints #[1], Column.ints #[2], Column.ints #[3]], rfl⟩ #["b", "c"]).names.size
    == (MemTable.selCols ⟨#["a", "b", "c"], #[Column.ints #[1], Column.ints #[2], Column.ints #[3]], rfl⟩ #["b", "c"]).cols.size

-- | MemTable.selCols picks correct columns
#guard (MemTable.selCols ⟨#["a", "b", "c"], #[Column.ints #[1], Column.ints #[2], Column.ints #[3]], rfl⟩ #["b"]).names == #["b"]

end MemTableInvariantTests

end PureTest
