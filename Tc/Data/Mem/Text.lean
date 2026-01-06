/-
  Text parsing: space-separated input (ps aux, ls -l, etc.)
-/
import Tc.Data.Mem.Table

namespace Tc.MemTable

-- | Find mode (most common value) in array
private def mode (xs : Array Nat) : Nat := Id.run do
  let mut m : Std.HashMap Nat Nat := {}
  for x in xs do m := m.insert x (m.getD x 0 + 1)
  let mut best := 0; let mut cnt := 0
  for (k, v) in m do if v > cnt then best := k; cnt := v
  best

-- | Count word starts (non-space after space, plus position 0 if non-space)
private def countWordStarts (s : String) : Nat := Id.run do
  let chars := s.toList.toArray
  let n := chars.size
  if n == 0 then return 0
  let mut cnt := if chars[0]! != ' ' then 1 else 0
  for i in [1:n] do
    if chars[i]! != ' ' && chars[i-1]! == ' ' then cnt := cnt + 1
  return cnt

-- | Split line into n fields (last field gets remainder with spaces)
private def splitN (s : String) (n : Nat) : Array String := Id.run do
  if n == 0 then return #[]
  let mut result : Array String := #[]
  let mut rest := s.trimLeft
  for _ in [:n-1] do
    match rest.splitOn " " |>.filter (·.length > 0) with
    | [] => result := result.push ""  -- pad with empty
    | fld :: tl =>
      result := result.push fld
      rest := (" ".intercalate tl).trimLeft
  result := result.push rest.trim  -- last field gets remainder
  result

-- | Find column start positions from header (2+ consecutive spaces = separator)
private def findColStarts (hdr : String) : Array Nat := Id.run do
  let chars := hdr.toList.toArray
  let n := chars.size
  let mut starts : Array Nat := #[0]
  let mut spaceCount := 0
  for i in [:n] do
    if chars[i]! == ' ' then spaceCount := spaceCount + 1
    else
      if spaceCount >= 2 then starts := starts.push i
      spaceCount := 0
  return starts

-- | Split line by column start positions (last col extends to end)
private def splitByStarts (s : String) (starts : Array Nat) : Array String := Id.run do
  let mut result : Array String := #[]
  for i in [:starts.size] do
    let st := starts[i]!
    let en := if i + 1 < starts.size then starts[i + 1]! else s.length
    result := result.push (s.drop st |>.take (en - st) |>.trim)
  return result

-- | Parse space-separated text (like ps aux, ls -l, systemctl output)
-- Fixed-width if header has 2+ space gaps AND gives more cols than mode-based
def fromText (content : String) : Except String MemTable :=
  let lines := content.splitOn "\n" |>.filter (·.length > 0)
  match lines with
  | [] => .ok ⟨#[], #[]⟩
  | hdr :: rest =>
    let starts := findColStarts hdr
    let allLines := (hdr :: rest).toArray
    let modeNc := mode (allLines.map countWordStarts)
    -- use fixed-width only if it gives >= mode columns (handles mixed spacing)
    if starts.size >= modeNc && starts.size > 1 then
      let names := splitByStarts hdr starts
      let strCols : Array (Array String) := Id.run do
        let mut cols := (List.replicate starts.size #[]).toArray
        for line in rest do
          let fields := splitByStarts line starts
          for i in [:starts.size] do cols := cols.modify i (·.push (fields.getD i ""))
        return cols
      .ok ⟨names, strCols.map buildColumn⟩
    else
      -- else use mode of word counts (handles "total 836" outliers)
      let nc := modeNc
      if nc == 0 then .ok ⟨#[], #[]⟩ else
      let names := splitN hdr nc
      let strCols : Array (Array String) := Id.run do
        let mut cols := (List.replicate nc #[]).toArray
        for line in rest do
          let fields := splitN line nc
          for i in [:nc] do cols := cols.modify i (·.push (fields.getD i ""))
        return cols
      .ok ⟨names, strCols.map buildColumn⟩

-- | Parse tab-separated text (TSV format)
def fromTsv (content : String) : Except String MemTable :=
  let lines := content.splitOn "\n" |>.filter (·.length > 0)
  match lines with
  | [] => .ok ⟨#[], #[]⟩
  | hdr :: rest =>
    let names := (hdr.splitOn "\t").toArray
    let nc := names.size
    if nc == 0 then .ok ⟨#[], #[]⟩ else
    let strCols : Array (Array String) := Id.run do
      let mut cols := (List.replicate nc #[]).toArray
      for line in rest do
        let fields := (line.splitOn "\t").toArray
        for i in [:nc] do cols := cols.modify i (·.push (fields.getD i ""))
      return cols
    .ok ⟨names, strCols.map buildColumn⟩

-- | Load from stdin (reads all input)
def fromStdin : IO (Except String MemTable) := do
  let content ← (← IO.getStdin).readToEnd
  pure (fromText content)

end Tc.MemTable
