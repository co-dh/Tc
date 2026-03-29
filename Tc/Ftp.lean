/-
  FTP: parse curl's ls -l output into folder-view TSV.
  URL-encodes names so paths with spaces/special chars navigate correctly.
-/
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
    rows := rows.push s!"{urlEncode name}\t{size}\t{date}\t{typ}"
  return "\n".intercalate rows.toList

end Tc.Ftp
