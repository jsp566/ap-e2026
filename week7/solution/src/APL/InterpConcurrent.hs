module APL.InterpConcurrent (runEval) where

import APL.Monad
import Data.IORef
import KVDB
import SPC

runEval :: EvalM a -> IO (Either Error a)
runEval computation = do
  kvdb <- startKVDB
  spc <- startSPC
  let runEval' :: Env -> EvalM a -> IO (Either Error a)
      runEval' _ (Pure x) = pure (Right x)
      runEval' r (Free (ReadOp k)) = runEval' r $ k r
      runEval' r (Free (KvGetOp key k)) = do
        val <- kvGet kvdb key
        runEval' r $ k val
      runEval' r (Free (KvPutOp key val m)) = do
        kvPut kvdb key val
        runEval' r m
      runEval' _ (Free (ErrorOp e)) = pure (Left e)
      runEval' r (Free (StepOp c)) = runEval' r c
      runEval' r (Free (BothOfOp x y c)) = do
        x_ref <- newIORef Nothing
        x_job <- jobAdd spc $ Job $ do
          writeIORef x_ref . Just =<< runEval' r x
        y_ref <- newIORef Nothing
        y_job <- jobAdd spc $ Job $ do
          writeIORef y_ref . Just =<< runEval' r y
        _ <- jobWaitAny spc [x_job]
        _ <- jobWaitAny spc [y_job]
        x' <- readIORef x_ref
        y' <- readIORef y_ref
        case (x', y') of
          (Just (Right x''), Just (Right y'')) ->
            runEval' r $ c $ ValTuple [x'', y'']
          (Just (Left err), _) ->
            pure (Left err)
          (_, Just (Left err)) ->
            pure (Left err)
          _ ->
            error "Unexpected failure"
      runEval' r (Free (OneOfOp x y c)) = do
        x_ref <- newIORef Nothing
        x_job <- jobAdd spc $ Job $ do
          writeIORef x_ref . Just =<< runEval' r x
        y_ref <- newIORef Nothing
        y_job <- jobAdd spc $ Job $ do
          writeIORef y_ref . Just =<< runEval' r y
        (which, Done) <- jobWaitAny spc [x_job, y_job]
        if which == x_job
          then do
            Just x' <- readIORef x_ref
            case x' of
              Right x'' -> do
                jobCancel spc y_job
                runEval' r $ c x''
              Left _ -> do
                _ <- jobWaitAny spc [y_job]
                Just y' <- readIORef y_ref
                case y' of
                  Right y'' -> runEval' r $ c y''
                  Left err -> pure (Left err)
          else do
            Just y' <- readIORef y_ref
            case y' of
              Right y'' -> do
                jobCancel spc x_job
                runEval' r $ c y''
              Left _ -> do
                _ <- jobWaitAny spc [x_job]
                Just x' <- readIORef x_ref
                case x' of
                  Right x'' -> runEval' r $ c x''
                  Left err -> pure (Left err)
  runEval' envEmpty computation
