/-
  FTP: parse curl's ls -l output into folder-view TSV.
  Names stored raw (readable); URL-encoding applied at curl command time.
-/
import Tc.Types

namespace Tc.Ftp

-- | URL-encode a string: encode all bytes not in [A-Za-z0-9._~/-]
def urlEncode (s : String) : String := Id.run do
  let mut out := ""
  for c in s.toList do
    if c.isAlphanum || c == '.' || c == '_' || c == '~' || c == '/' || c == '-' then
      out := out.push c
    else
      let bytes := (String.mk [c]).toUTF8
      for b in bytes do
        let hi := "0123456789ABCDEF".get ⟨(b / 16).toNat⟩
        let lo := "0123456789ABCDEF".get ⟨(b % 16).toNat⟩
        out := out ++ s!"%{hi}{lo}"
  return out

-- | URL-encode an FTP URL: encode only path segments, not protocol/host.
-- e.g. "ftp://host/a b/c d/" → "ftp://host/a%20b/c%20d/"
def urlEncodeUrl (pfx url : String) : String :=
  let rest := (url.drop pfx.length).toString
  let segs := rest.splitOn "/"
  match segs with
  | host :: tail => pfx ++ host ++ "/" ++ "/".intercalate (tail.map urlEncode)
  | [] => url

-- | Parse FTP ls -l output into TSV (name\tsize\tdate\ttype).
-- Format: perms links user group size month day time name...
--         p[0]  p[1]  p[2] p[3]  p[4] p[5]  p[6] p[7] p[8:]
def parseLs (raw : String) : String := Id.run do
  let mut rows : Array String := #["name\tsize\tdate\ttype"]
  for line in raw.splitOn "\n" do
    let parts := line.trim.splitOn " " |>.filter (·.length > 0)
    if parts.length < 9 then continue
    let name := " ".intercalate (parts.drop 8) |>.splitOn " -> " |>.head!
    let size := parts.getD 4 "0"
    let date := " ".intercalate (parts.drop 5 |>.take 3)
    let typ := if (parts.getD 0 "").startsWith "d" then "dir" else "file"
    rows := rows.push s!"{name}\t{size}\t{date}\t{typ}"
  return rows.joinWith "\n"

end Tc.Ftp
