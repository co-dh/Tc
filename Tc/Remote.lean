/-
  Remote: shared path operations for URI-based remote backends (S3, HF, etc.)
-/
namespace Tc.Remote

-- | Join URI prefix with child name
def join (pfx name : String) : String :=
  if pfx.endsWith "/" then s!"{pfx}{name}" else s!"{pfx}/{name}"

-- | Get parent URI: drop last path component. Returns none at root (≤ minParts components).
def parent (path : String) (minParts : Nat) : Option String :=
  let p := if path.endsWith "/" then (path.take (path.length - 1)).toString else path
  let parts := p.splitOn "/"
  if parts.length ≤ minParts then none
  else some ("/".intercalate (parts.dropLast) ++ "/")

-- | Display name: last non-empty path component (preserves protocol-only paths)
def dispName (path : String) : String :=
  let p := if path.endsWith "/" then (path.take (path.length - 1)).toString else path
  let parts := p.splitOn "/" |>.filter (·.length > 0)
  if parts.length ≤ 1 then path else parts.getLast?.getD path

end Tc.Remote
