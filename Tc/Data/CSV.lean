/-
  Simple CSV parser (RFC 4180)
-/
namespace CSV

-- Parse state
inductive St | row | fld | quot | esc

-- Parse CSV: returns array of rows (each row is array of fields)
def parse (s : String) (delim : Char := ',') : Array (Array String) :=
  let n := s.length
  let rec go (i : Nat) (st : St) (fld : String) (row : Array String) (rows : Array (Array String)) : Array (Array String) :=
    if i < n then
      let c := s.get ⟨i⟩
      match st with
      | .row =>  -- start of row
        if c == '"' then go (i+1) .quot "" row rows
        else if c == delim then go (i+1) .row "" (row.push "") rows
        else if c == '\n' then go (i+1) .row "" #[] (if row.isEmpty then rows else rows.push row)
        else if c == '\r' then go (i+1) .row fld row rows  -- skip CR
        else go (i+1) .fld (fld.push c) row rows
      | .fld =>  -- unquoted field
        if c == delim then go (i+1) .row "" (row.push fld) rows
        else if c == '\n' then go (i+1) .row "" #[] (rows.push (row.push fld))
        else if c == '\r' then go (i+1) .fld fld row rows
        else go (i+1) .fld (fld.push c) row rows
      | .quot =>  -- inside quoted field
        if c == '"' then go (i+1) .esc fld row rows
        else go (i+1) .quot (fld.push c) row rows
      | .esc =>  -- after quote in quoted field
        if c == '"' then go (i+1) .quot (fld.push '"') row rows  -- escaped quote
        else if c == delim then go (i+1) .row "" (row.push fld) rows
        else if c == '\n' then go (i+1) .row "" #[] (rows.push (row.push fld))
        else if c == '\r' then go (i+1) .esc fld row rows
        else go (i+1) .fld (fld.push c) row rows  -- malformed but handle
    else  -- end of input
      if fld.isEmpty && row.isEmpty then rows
      else rows.push (row.push fld)
  termination_by n - i
  go 0 .row "" #[] #[]

end CSV
