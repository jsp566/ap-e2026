module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, replicateM)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "simple-job" $ do
          spc <- startSPC
          _ <- workerAdd spc ""
          ref <- newIORef False
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= Just JobRunning
          r2 <- jobWait spc j
          r2 @?= Just Done
          v <- readIORef ref
          v @?= True,
        testCase "multiple-jobs" $ do
          spc <- startSPC
          _ <- workerAdd spc ""
          let num_jobs = 10
          ref <- newIORef (0 :: Int)
          js <- replicateM num_jobs $ jobAdd spc $ Job (modifyIORef ref (+ 1)) 1
          rs <- mapM (jobWait spc) js
          rs @?= replicate num_jobs (Just Done)
          v <- readIORef ref
          v @?= 10,
        testCase "timeout" $ do
          spc <- startSPC
          _ <- workerAdd spc ""
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= Just JobRunning
          r2 <- jobWait spc j
          r2 @?= Just DoneTimeout,
        testCase "cancel" $ do
          spc <- startSPC
          _ <- workerAdd spc ""
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 20000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= Just JobRunning
          jobCancel spc j
          r2 <- jobWait spc j
          r2 @?= Just DoneCancelled,
        testCase "crash" $ do
          spc <- startSPC
          _ <- workerAdd spc ""
          j1 <- jobAdd spc $ Job (error "boom") 1
          r1 <- jobWait spc j1
          r1 @?= Just DoneCrashed
          -- Ensure new jobs can still work.
          ref <- newIORef False
          j2 <- jobAdd spc $ Job (writeIORef ref True) 1
          r2 <- jobWait spc j2
          r2 @?= Just Done
          v <- readIORef ref
          v @?= True,
        testCase "unknown job" $ do
          spc1 <- startSPC
          _ <- workerAdd spc1 ""
          j1 <- jobAdd spc1 $ Job (pure ()) 1
          r1 <- jobWait spc1 j1
          r1 @?= Just Done
          spc2 <- startSPC
          _ <- workerAdd spc2 ""
          r2 <- jobWait spc2 j1
          r2 @?= Nothing

        -- Adding worker
        -- Adding worker that already exists
        -- Removing worker
        -- Removing worker twice
        -- Adding worker that has been removed 
      ]
