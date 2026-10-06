module APL.InterpPure (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

runEval :: EvalM a -> Either Error a
runEval = fst . runEval' envEmpty stateInitial
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
    runEval' r s (Free (StepOp c)) = runEval' r s c
    runEval' r s (Free (BothOfOp x y c)) =
      case runEval' r s x of
        (Left err, s') -> (Left err, s')
        (Right x', s') ->
          case runEval' r s' y of
            (Left err, s'') -> (Left err, s'')
            (Right y', s'') ->
              runEval' r s'' $ c $ ValTuple [x', y']
    runEval' r s (Free (OneOfOp x y c)) =
      case runEval' r s x of
        (Left _, s') ->
          case runEval' r s' y of
            (Left err, s'') -> (Left err, s'')
            (Right y', s'') ->
              runEval' r s'' $ c y'
        (Right x', s') ->
          runEval' r s' $ c x'
