-- Like the pure interpreter, but returns the number of step effects.
-- Only counts the successful execution for ||.
module APL.InterpStep (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

runEval :: EvalM a -> Int
runEval = fst . fst . runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ((Int, Either Error a), State)
    runEval' _ s (Pure x) = ((0, pure x), s)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Nothing -> ((0, Left $ "Invalid key: " ++ show key), s)
        Just val -> runEval' r s $ k val
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter ((/= key) . fst) s
       in runEval' r s' m
    runEval' _ s (Free (ErrorOp e)) = ((0, Left e), s)
    runEval' r s (Free (StepOp c)) =
      let ((n, x), s') = runEval' r s c
       in ((n + 1, x), s')
    runEval' r s (Free (BothOfOp x y c)) =
      case runEval' r s x of
        ((n, Left err), s') -> ((n, Left err), s')
        ((n, Right x'), s') ->
          case runEval' r s' y of
            ((m, Left err), s'') -> ((n + m, Left err), s'')
            ((m, Right y'), s'') ->
              let ((k, c'), s''') = runEval' r s'' $ c $ ValTuple [x', y']
               in ((n + m + k, c'), s''')
    runEval' r s (Free (OneOfOp x y c)) =
      case runEval' r s x of
        ((n, Left err), s') ->
          case runEval' r s' y of
            ((_, Left _), s'') -> ((n, Left err), s'')
            ((m, Right y'), s'') ->
              let ((k, c'), s''') = runEval' r s'' $ c y'
               in ((m + k, c'), s''')
        ((n, Right x'), s') ->
          let ((m, c'), s'') = runEval' r s' $ c x'
           in ((n + m, c'), s'')
