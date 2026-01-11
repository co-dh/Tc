/-
  Backend lifecycle: Core build (MemTable only, no DB backends)
-/
namespace Tc.Backend

-- | No-op init for core build
def init : IO Bool := pure true

-- | No-op shutdown for core build
def shutdown : IO Unit := pure ()

end Tc.Backend
