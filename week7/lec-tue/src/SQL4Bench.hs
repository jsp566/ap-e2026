{- Benchmark of SQL4's discrimination-based relational operators against
   growing inputs, timing a single execution per size (not a statistical
   criterion-style run), until each query reaches roughly 20s.
   Compile with: cabal build exe:SQL4Bench   (ghc-options: -O2, see .cabal file) -}

module Main where

import Control.Exception (evaluate)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Mem (performGC)
import Text.Printf (printf)

import Equiv
import SQL4

-- | Rows are (key, payload) pairs; key uniformly spread over [0, domain).
type Row = (Int, Int)

mkRows :: Int -> Int -> [Row]
mkRows domain n = [ (i `mod` domain, i) | i <- [1 .. n] ]

-- | Time a single forced evaluation of an IO action, in seconds of CPU time.
timeIt :: IO a -> IO Double
timeIt act = do
  performGC
  t0 <- getCPUTime
  _  <- act
  t1 <- getCPUTime
  return (fromIntegral (t1 - t0) / 1.0e12)

report :: Int -> String -> Double -> IO ()
report n label secs = printf "  n=%-10d  %-10s  %8.3f s\n" n label (secs :: Double)

----------------------------------------------------------------------------------------
-- Example A:  SELECT * FROM r, s WHERE r.key = s.key          (equi-join)
--
-- SQL4:  select ((fst, fst) `Is` e) (r `X` s)
-- which pattern-matches the `Is` join predicate against a product `X` and
-- dispatches to djoin, the discrimination-based join.
----------------------------------------------------------------------------------------
joinQuery :: Int -> Int -> Int
joinQuery domain n =
  count (select cond (r `X` s))
  where
    r    = MSet (mkRows domain n)
    s    = MSet (mkRows domain n)
    e    = NatE (domain - 1)
    cond = (fst, fst) `Is` e

----------------------------------------------------------------------------------------
-- Example B:  SELECT key, COUNT(*) FROM r GROUP BY key
--
-- SQL4:  perform (Func count) (groupBy e r)       -- the GROUP BY ... COUNT(*)
--        reduce (+) 0 ...                         -- forces every group fully
----------------------------------------------------------------------------------------
groupByCountQuery :: Int -> Int -> Int
groupByCountQuery domain n =
  reduce (+) 0 (perform (Func count) (groupBy e r))
  where
    r = MSet (mkRows domain n)
    e = MapE fst (NatE (domain - 1))  -- group rows by their key component

----------------------------------------------------------------------------------------
-- Example C:  SELECT DISTINCT key FROM r
--
-- SQL4:  coalesceBy e (perform (Func fst) r)
----------------------------------------------------------------------------------------
distinctQuery :: Int -> Int -> Int
distinctQuery domain n =
  count (coalesceBy e (perform (Func fst) r))
  where
    r = MSet (mkRows domain n)
    e = NatE (domain - 1)

----------------------------------------------------------------------------------------

runSeries :: String -> Int -> (Int -> Int) -> [Int] -> IO ()
runSeries name domain q ns = do
  putStrLn name
  mapM_ (\n -> do
            secs <- timeIt (evaluate (q n))
            report n ("dom=" ++ show domain) secs)
        ns

-- | Calibration mode: `SQL4Bench calib <A|B|C> <n>` runs one size of one
-- query and prints its time, so sizes can be picked without recompiling.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["calib", "A", nStr] -> calib "A" (joinQuery 1000) (read nStr)
    ["calib", "B", nStr] -> calib "B" (groupByCountQuery 1000) (read nStr)
    ["calib", "C", nStr] -> calib "C" (distinctQuery 1000) (read nStr)
    _ -> fullRun

calib :: String -> (Int -> Int) -> Int -> IO ()
calib label q n = do
  secs <- timeIt (evaluate (q n))
  report n label secs

fullRun :: IO ()
fullRun = do
  putStrLn "SQL4 discrimination-based relational operators: scaling benchmark\n"

  runSeries "A. SELECT * FROM r, s WHERE r.key = s.key   (equi-join / djoin)"
            1000 (joinQuery 1000)
            [400000, 800000, 1600000, 3200000, 6400000, 12800000, 23000000]

  putStrLn ""

  runSeries "B. SELECT key, COUNT(*) FROM r GROUP BY key  (groupBy + aggregate)"
            1000 (groupByCountQuery 1000)
            [1000000, 4000000, 16000000, 32000000, 48000000, 61000000]

  putStrLn ""

  runSeries "C. SELECT DISTINCT key FROM r                (coalesceBy)"
            1000 (distinctQuery 1000)
            [1000000, 4000000, 16000000, 64000000, 128000000, 195000000]
