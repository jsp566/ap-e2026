module APL.InterpPure (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

runEval :: EvalM a -> Either Error a
runEval = fmap fst $ runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> (Either Error a, State)
    runEval' _ s (Pure x) = (pure x, s)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Nothing -> (Left $ "Invalid key: " ++ show key, s)
        Just val -> runEval' r s $ k val
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter ((/= key) . fst) s
       in runEval' r s' m
    runEval' _ s (Free (ErrorOp e)) = (Left e, s)
    runEval' r s (Free (StepOp m)) = runEval' r s m
    runEval' r s (Free (BothOfOp x y c)) = 
      let (res, s') = runEval' r s x
      in case res of
        Left err -> (Left err, s')
        Right val -> 
          let (res', s'') = runEval' r s' y
          in case res' of 
            Left err' -> (Left err', s'')
            Right val' -> 
              runEval' r s'' $ c $ ValTuple [val, val']
    runEval' r s (Free (OneOfOp x y c)) = 
      let (res, s') = runEval' r s x
      in case res of
        Right val -> runEval' r s' $ c val
        Left _ -> 
          let (res', s'') = runEval' r s y
          in case res' of 
            Right val' -> runEval' r s'' $ c val'
            Left err -> (Left err, s'')