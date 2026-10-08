module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC"
      [ testCase "pingSPC" $ do
          s <- startSPC
          i0 <- pingSPC s
          i0 @?= 0
          i1 <- pingSPC s
          i1 @?= 1
          i2 <- pingSPC s
          i2 @?= 2
          ji3 <- jobAdd s $ Job (pure ()) 1
          ji3s <- jobStatus s ji3
          ji3s @?= (Just JobRunning)
          jobCancel s ji3
          ji3s' <- jobStatus s ji3
          ji3s' @?= (Just (JobDone Done))
          pure (),
        testCase "running job" $ do
          ref <- newIORef False
          spc <- startSPC
          j <- jobAdd spc $ Job (writeIORef ref True) 10
          r <- jobWait spc j
          r @?= Just Done
          x <- readIORef ref
          x @?= True,
        testCase "timeout" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= Just JobRunning
          r2 <- jobWait spc j
          r2 @?= Just DoneTimeout
          ]
