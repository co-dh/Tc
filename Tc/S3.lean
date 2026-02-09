/-
  S3: helpers for browsing S3 buckets via `aws s3` CLI
-/
import Tc.Render
import Tc.Remote

namespace Tc.S3

-- | Global flag: use --no-sign-request for S3 (set via +n arg)
initialize noSign : IO.Ref Bool ← IO.mkRef false

def setNoSign (b : Bool) : IO Unit := noSign.set b
def getNoSign : IO Bool := noSign.get

-- | Extra args for S3 commands when --no-sign-request is enabled
def extra : IO (Array String) := do
  if ← getNoSign then pure #["--no-sign-request"] else pure #[]

-- | Check if path is an S3 URI
def isS3 (path : String) : Bool := path.startsWith "s3://"

-- | S3 parent: none at bucket root ("s3://bucket/" = 3 parts)
def parent (path : String) : Option String := Remote.parent path 3

-- | List S3 prefix via `aws s3 ls`, returns TSV matching listDir schema
def list (path : String) : IO String := do
  statusMsg s!"Loading {path} ..."
  let p := if path.endsWith "/" then path else s!"{path}/"
  let ex ← extra
  let out ← IO.Process.output { cmd := "aws", args := #["s3", "ls"] ++ ex ++ #[p] }
  let lines := out.stdout.splitOn "\n" |>.filter (·.length > 0)
  let hdr := "path\tsize\tdate\ttype"
  let parentEntry := if parent path |>.isSome then "..\t0\t\td" else ""
  let body := lines.map fun line =>
    if line.trimAsciiStart.toString.startsWith "PRE " then
      let name := (line.trimAscii.toString.drop 4).toString
      let name := if name.endsWith "/" then (name.take (name.length - 1)).toString else name
      s!"{name}\t0\t\td"
    else
      let parts := line.trimAscii.toString.splitOn " " |>.filter (·.length > 0)
      if parts.length >= 4 then
        let dt := s!"{parts.getD 0 ""} {parts.getD 1 ""}"
        let sz := parts.getD 2 ""
        let name := parts.drop 3 |> " ".intercalate
        s!"{name}\t{sz}\t{dt}\t "
      else line
  let entries := if parentEntry.isEmpty then body else [parentEntry] ++ body
  pure (hdr ++ (if entries.isEmpty then "" else "\n" ++ "\n".intercalate entries))

-- | Download S3 file to local temp path, returns local path
def download (s3Path : String) : IO String := do
  statusMsg s!"Downloading {s3Path} ..."
  let _ ← IO.Process.output { cmd := "mkdir", args := #["-p", "/tmp/tc-s3"] }
  let name := s3Path.splitOn "/" |>.getLast? |>.getD "file"
  let local_ := s!"/tmp/tc-s3/{name}"
  let ex ← extra
  let _ ← IO.Process.output { cmd := "aws", args := #["s3", "cp"] ++ ex ++ #[s3Path, local_] }
  pure local_

end Tc.S3
