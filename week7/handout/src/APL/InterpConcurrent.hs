module APL.InterpConcurrent (runEval) where

import APL.Monad
import Data.IORef
import KVDB
import SPC


type State = KVDB Val Val

stateInitial :: IO (State)
stateInitial = startKVDB


runEval :: EvalM a -> IO (Either Error a)
runEval initial = do
    initstate <- stateInitial
    runEval' envEmpty initstate initial
  where
    runEval' :: Env -> State -> EvalM a -> IO (Either Error a)
    runEval' _ _ (Pure x) = pure $ Right x
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (KvGetOp key k)) = do
        val <- kvGet s key 
        runEval' r s $ k val
    runEval' r s (Free (KvPutOp key val m)) = do
        kvPut s key val
        runEval' r s m
    runEval' _ _ (Free (ErrorOp e)) = pure (Left e)
    runEval' r s (Free (StepOp m)) = runEval' r s m
    runEval' r s (Free (BothOfOp x y c)) = do
        spc <- startSPC
        x_ref <- newIORef Nothing
        x_job <- jobAdd spc $ Job $ do
            writeIORef x_ref . Just =<< runEval' r s x
        y_ref <- newIORef Nothing
        y_job <- jobAdd spc $ Job $ do
            writeIORef y_ref . Just =<< runEval' r s y 
        _ <- jobWaitAny spc [x_job]
        _ <- jobWaitAny spc [y_job]
        Just x' <- readIORef x_ref
        Just y' <- readIORef y_ref
        case (x', y') of
           (Right x'', Right y'') -> runEval' r s $ c $ ValTuple [x'', y'']
           (Left e, _) -> pure (Left e)
           (_, Left e) -> pure (Left e)
    runEval' r s (Free (OneOfOp x y c)) = do
        spc <- startSPC
        x_ref <- newIORef Nothing
        x_job <- jobAdd spc $ Job $ do
            writeIORef x_ref . Just =<< runEval' r s x
        y_ref <- newIORef Nothing
        y_job <- jobAdd spc $ Job $ do
            writeIORef y_ref . Just =<< runEval' r s y
        (donejobid, _) <- jobWaitAny spc [x_job, y_job]
        if donejobid == x_job
            then do
                Just x' <- readIORef x_ref
                case x' of 
                    Right x'' -> runEval' r s $ c x''
                    Left _ -> do 
                        _ <- jobWaitAny spc [x_job, y_job]
                        Just y' <- readIORef y_ref
                        case y' of 
                            Right y'' -> runEval' r s $ c y''
                            Left e -> pure (Left e)

        else do
                Just y' <- readIORef y_ref
                case y' of 
                    Right y'' -> runEval' r s $ c y''
                    Left _ -> do 
                        _ <- jobWaitAny spc [x_job, y_job]
                        Just x' <- readIORef x_ref
                        case x' of 
                            Right x'' -> runEval' r s $ c x''
                            Left e -> pure (Left e)
