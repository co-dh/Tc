/-
  In-memory table: column-major with typed cells
  CSV parsing via Lean.Data.Parsec (RFC 4180 compliant)
-/
import Lean.Data.Parsec
import Tc.Data.Table
import Tc.Nav
import Tv.Term
import Tv.Types

open Lean Parsec

namespace Tc

/-! ## CSV Parser (RFC 4180) -/

-- Text data: printable ASCII except comma and quote
def textData : Parsec Char := satisfy fun c =>
  0x20 ≤ c.val ∧ c.val ≤ 0x21 ∨
  0x23 ≤ c.val ∧ c.val ≤ 0x2B ∨
  0x2D ≤ c.val ∧ c.val ≤ 0x7E

def cr : Parsec Char := pchar '\r'
def lf : Parsec Char := pchar '\n'
def crlf : Parsec String := pstring "\r\n"
def comma : Parsec Char := pchar ','
def dQUOTE : Parsec Char := pchar '\"'
def twoDQUOTE : Parsec Char := attempt (pchar '"' *> pchar '"')

-- Escaped field: quoted, may contain comma/newline/escaped quotes
def escaped : Parsec String := attempt
  dQUOTE *> manyChars (textData <|> comma <|> cr <|> lf <|> twoDQUOTE) <* dQUOTE

-- Non-escaped field: plain text
def nonEscaped : Parsec String := manyChars textData

def field : Parsec String := escaped <|> nonEscaped

-- Many p separated by s
def manySep (p : Parsec α) (s : Parsec β) : Parsec (Array α) := do
  manyCore (attempt (s *> p)) #[←p]

def record : Parsec (Array String) := manySep field comma

-- Handle both \r\n and \n line endings
def lineEnd : Parsec Unit := (crlf *> pure ()) <|> (lf *> pure ())

def csvFile : Parsec (Array (Array String)) :=
  manySep record (lineEnd <* notFollowedBy eof) <* (optional lineEnd) <* eof

def parseCSV (s : String) : Except String (Array (Array String)) :=
  match csvFile s.mkIterator with
  | .success _ res => .ok res
  | .error it err  => .error s!"offset {it.i.byteIdx}: {err}"

/-! ## MemTable -/

-- In-memory table: column-major storage
structure MemTable where
  names  : Array String
  cols   : Array (Array Cell)  -- column-major
  widths : Array Nat

namespace MemTable

-- Parse cell value (detect type)
def parseCell (s : String) : Cell :=
  if s.isEmpty then .null
  else if let some n := s.toInt? then .int n
  else .str s

-- Transpose rows to columns
def transpose (rows : Array (Array Cell)) (nc : Nat) : Array (Array Cell) :=
  let mut cols : Array (Array Cell) := Array.mkArray nc #[]
  for r in rows do
    for i in [:nc] do
      cols := cols.modify i (·.push (r.getD i .null))
  cols

-- Compute column width from column data
def calcWidth (name : String) (col : Array Cell) : Nat :=
  let w0 := name.length
  let wMax := col.foldl (fun mx c => max mx c.toString.length) w0
  min wMax 50

-- Load CSV file into MemTable
def load (path : String) : IO (Except String MemTable) := do
  let content ← IO.FS.readFile path
  match parseCSV content with
  | .error e => pure (.error e)
  | .ok recs =>
    match recs.toList with
    | [] => pure (.ok ⟨#[], #[], #[]⟩)
    | hdr :: rest =>
      let names := hdr
      let nc := names.size
      let rows := rest.map (·.map parseCell) |>.toArray
      let cols := transpose rows nc
      let widths := names.zipWith cols calcWidth
      pure (.ok ⟨names, cols, widths⟩)

-- Get cell at (row, col)
def cell (t : MemTable) (r c : Nat) : Cell := (t.cols.getD c #[]).getD r .null

-- Row count
def nRows (t : MemTable) : Nat := (t.cols.getD 0 #[]).size

end MemTable

-- ReadTable instance for MemTable
instance : ReadTable MemTable where
  nRows     := MemTable.nRows
  colNames  := (·.names)
  colWidths := (·.widths)
  cell      := fun t r c => (MemTable.cell t r c).toString

-- Render MemTable to terminal (pure Lean)
instance : RenderTable MemTable where
  render nav colOff r0 r1 styles := do
    let t := nav.tbl
    let dispIdxs := nav.dispColIdxs
    let w ← Term.width
    -- header row (y=0)
    let mut x : UInt32 := 0
    for di in dispIdxs do
      if di < colOff then continue
      if x >= w then break
      let name := t.names.getD di ""
      let cw := t.widths.getD di 10
      let fg := if nav.selColIdxs.contains di then Term.magenta else Term.cyan
      Term.printPad x 0 cw.toUInt32 fg Term.default name
      x := x + cw.toUInt32 + 1
    -- data rows
    for row in [r0:r1] do
      let y := (row - r0 + 1).toUInt32
      x := 0
      for di in dispIdxs do
        if di < colOff then continue
        if x >= w then break
        let cell := (MemTable.cell t row di).toString
        let cw := t.widths.getD di 10
        let isCur := row == nav.row.cur.val && di == nav.curColIdx
        let isSelRow := nav.selRows.contains row
        let isSelCol := nav.selColIdxs.contains di
        let (fg, bg) := match (isCur, isSelRow, isSelCol) with
          | (true, _, _) => (styles.getD 0 Term.black, styles.getD 1 Term.white)
          | (_, true, _) => (styles.getD 2 Term.black, styles.getD 3 Term.green)
          | (_, _, true) => (styles.getD 6 Term.magenta, styles.getD 7 Term.default)
          | _ => (Term.default, Term.default)
        Term.printPad x y cw.toUInt32 fg bg cell
        x := x + cw.toUInt32 + 1
    pure ()

end Tc
