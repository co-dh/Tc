-- Free-monad style effect machinery for the main event loop.
-- The loop body is written once as an `AppM σ α` program value, then run
-- through one of two interpreters: production (real Term/Socket/fzf) or
-- test (keystroke array + preview-less render + buffer drain on empty).
--
-- Only the three operations that actually differ between test and prod
-- (render, nextKey, readArg) are captured as AppM constructors; everything
-- else lifts through `liftIO` so existing IO helpers (Socket.pollCmd,
-- dispatchHandler, CmdConfig lookups, handler dispatch) stay put.
namespace Tc

-- | Program value: a small free monad describing what the event loop does.
-- The state type `σ` (= `AppState` at call sites) is a parameter so this
-- file does not need to import `App.Common`, avoiding a cyclic import.
inductive AppM (σ α : Type) where
  -- pure return
  | pure    : α → AppM σ α
  -- generic IO escape hatch (used via `MonadLift IO` below)
  | liftIO  : IO β → (β → AppM σ α) → AppM σ α
  -- render the current state and receive the updated state back
  | render  : σ → (σ → AppM σ α) → AppM σ α
  -- poll for the next keystroke; `none` means "test queue drained" (prod never returns none)
  | nextKey : (Option String → AppM σ α) → AppM σ α
  -- read argument for an arg-command; prod returns "" (fzf handled in handler), test collects up to <ret>
  | readArg : (String → AppM σ α) → AppM σ α
  -- unreachable sentinel so `AppM σ α` is unconditionally Nonempty (needed to
  -- define `bind` as `partial`). Interpreters throw on this — never produced
  -- by normal programs.
  | halt    : AppM σ α

-- Unconditional inhabitant via `halt`, needed so `partial def bind` type-checks.
instance : Nonempty (AppM σ α) := ⟨AppM.halt⟩

namespace AppM

-- Structural bind on the program value. `partial` because the recursion runs
-- through the continuation function space — fine for a control flow effect.
partial def bind : AppM σ α → (α → AppM σ β) → AppM σ β
  | .pure x,       f => f x
  | .liftIO io k,  f => .liftIO io  (fun b  => (k b ).bind f)
  | .render s k,   f => .render s   (fun s' => (k s').bind f)
  | .nextKey k,    f => .nextKey    (fun o  => (k o ).bind f)
  | .readArg k,    f => .readArg    (fun s  => (k s ).bind f)
  | .halt,         _ => .halt

instance : Monad (AppM σ) where
  pure := .pure
  bind := AppM.bind

-- Lift arbitrary IO into AppM so `← someIOAction` works inside do-blocks.
instance : MonadLift IO (AppM σ) where
  monadLift io := .liftIO io .pure

-- Smart constructors — the loop body uses these instead of raw ctors.
@[inline] def doRender (s : σ) : AppM σ σ := .render s .pure
@[inline] def poll     : AppM σ (Option String) := .nextKey .pure
@[inline] def readArg' : AppM σ String := .readArg .pure

-- | Interpreter: three operations that differ between test and prod.
-- Everything else runs as plain IO via the liftIO constructor.
structure Interp (σ : Type) where
  render  : σ → IO σ
  nextKey : IO (Option String)
  readArg : IO String

-- Walk the program value, dispatching each op through the supplied interpreter.
partial def run (i : Interp σ) : AppM σ α → IO α
  | .pure x       => (Pure.pure x : IO α)
  | .liftIO io k  => do let b ← io;          (k b ).run i
  | .render s k   => do let s' ← i.render s; (k s').run i
  | .nextKey k    => do let o ← i.nextKey;   (k o ).run i
  | .readArg k    => do let s ← i.readArg;   (k s ).run i
  | .halt         => throw (IO.userError "AppM.halt reached (unreachable sentinel)")

end AppM
end Tc
