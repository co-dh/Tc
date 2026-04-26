{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Fong's schema-migration triple in Haskell.
--
-- Given any "schema migration" F : Gr -> DDS, we get three functors
-- between instance categories.  Δ_F is the easy one (precomposition,
-- a.k.a. "trajectory graph" for our example).  Σ_F (left Kan extension)
-- and Π_F (right Kan extension) need more machinery — see the
-- `kan-extensions` package on Hackage for the abstract definitions.

module Migration where

------------------------------------------------------------------------
-- Schemas as record types
------------------------------------------------------------------------

-- A Gr-instance: vertex set V, edge set E, source/target maps.
data GrInst v e = GrInst { src :: e -> v, tgt :: e -> v }

-- A DDS-instance: state set X with a self-map "next" (the dynamics).
data DDSInst x = DDSInst { next :: x -> x }

------------------------------------------------------------------------
-- Δ_F : DDSInst -> GrInst   (Fong's example, the trajectory graph)
------------------------------------------------------------------------

-- Vertices = states, edges = states (one edge per state).
-- src = id   (because F maps the schema arrow `s : E -> V` to id_S)
-- tgt = next (because F maps `t : E -> V` to next).
deltaTrajectory :: DDSInst x -> GrInst x x
deltaTrajectory (DDSInst f) = GrInst { src = id, tgt = f }

-- Concrete example: 4-cycle DDS, run through Δ_F to get its trajectory graph.
cycle4 :: DDSInst Int
cycle4 = DDSInst (\n -> (n + 1) `mod` 4)

trajectoryGraph :: GrInst Int Int
trajectoryGraph = deltaTrajectory cycle4
-- src = id, tgt = (\n -> (n+1) mod 4)
-- 4 vertices, 4 edges, each edge from n to (n+1) mod 4.

------------------------------------------------------------------------
-- Σ_F (Lan)  and  Π_F (Ran)  — abstract type signatures only
------------------------------------------------------------------------

-- These are the standard Kan extension types from `kan-extensions`.
-- They take a functor `g` to "extend along" and a functor `h` to extend.

data Lan g h a where
  Lan :: (g b -> a) -> h b -> Lan g h a

newtype Ran g h a = Ran { runRan :: forall b. (a -> g b) -> h b }

-- For Fong's `Gr -> DDS`, building Σ and Π concretely requires
-- materialising the indexing-category-of-Gr machinery (a quotient by
-- naturality, in Σ's case).  Skipped here.

------------------------------------------------------------------------
-- The schema-migration triple, parameterised by a "schema migration F"
------------------------------------------------------------------------

-- We can't really talk about "F : Gr -> DDS" as a Haskell value (Haskell
-- doesn't have a Cat type that's not Hask), so we package the migration
-- abstractly: a triple whose Δ component is concrete, whose Σ/Π
-- components are placeholders for Kan extensions.
data Triple gr dds = Triple
  { lan   :: ()                          -- Σ_F : skipped
  , delta :: dds -> gr                   -- Δ_F : pull dds-instance back
  , ran   :: ()                          -- Π_F : skipped
  }

-- Fong's specific triple: F : Gr -> DDS where every DDS-instance gives
-- a Gr-instance via `deltaTrajectory`.  Polymorphic in the carrier `x`.
fongMigration :: Triple (GrInst x x) (DDSInst x)
fongMigration = Triple
  { lan   = ()
  , delta = deltaTrajectory
  , ran   = ()
  }

-- Use:
--   ghci> let g = delta fongMigration cycle4
--   ghci> tgt g 2
--   3
--   ghci> tgt g 3
--   0

------------------------------------------------------------------------
-- Dataframe-style wrappers: rows in / rows out, Δ_F on the path
------------------------------------------------------------------------

-- DDS rows: (state, next state)
type DDSRows a = [(a, a)]

-- Gr rows: (arrow, src vertex, tgt vertex)
type GrRows a = [(a, a, a)]

ddsFromRows :: Eq a => DDSRows a -> DDSInst a
ddsFromRows rows = DDSInst (\s -> maybe s id (lookup s rows))

grToRows :: [e] -> GrInst v e -> [(e, v, v)]
grToRows edges g = [(e, src g e, tgt g e) | e <- edges]

-- Δ on data — actually goes through `deltaTrajectory`.
deltaRows :: Eq a => DDSRows a -> GrRows a
deltaRows rows =
  let dds = ddsFromRows rows
      gr  = deltaTrajectory dds
  in  grToRows (map fst rows) gr

------------------------------------------------------------------------
-- Self-test
------------------------------------------------------------------------

main :: IO ()
main = do
  let input  = [(0,1),(1,2),(2,3),(3,0)] :: DDSRows Int
      output = deltaRows input
      expected = [(0,0,1),(1,1,2),(2,2,3),(3,3,0)] :: GrRows Int
  putStrLn $ "input:    " ++ show input
  putStrLn $ "output:   " ++ show output
  putStrLn $ "expected: " ++ show expected
  if output == expected
    then putStrLn "PASS"
    else putStrLn "FAIL"
