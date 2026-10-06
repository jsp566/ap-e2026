module APL.InterpSim (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

step :: Env -> State -> EvalM a -> (EvalM a, State)
step _ s (Pure x) = (Pure x, s)
step r s (Free (ReadOp c)) = step r s $ c r
step r s (Free (KvGetOp key k)) =
  case lookup key s of
    Nothing -> (Free (KvGetOp key k), s)
    Just val -> step r s $ k val
step r s (Free (KvPutOp key val m)) =
  let s' = (key, val) : filter ((/= key) . fst) s
   in step r s' m
step _ s (Free (ErrorOp e)) = (Free (ErrorOp e), s)
step _ s (Free (StepOp c)) = (c, s)
step _ s (Free (BothOfOp (Free (ErrorOp e)) _ _)) =
  (Free (ErrorOp e), s)
step _ s (Free (BothOfOp _ (Free (ErrorOp e)) _)) =
  (Free (ErrorOp e), s)
step r s (Free (BothOfOp (Pure x) (Pure y) c)) =
  step r s $ c $ ValTuple [x, y]
step r s (Free (BothOfOp x y c)) =
  let (x', s') = step r s x
      (y', s'') = step r s' y
   in (Free (BothOfOp x' y' c), s'')
step _ s (Free (OneOfOp (Free (ErrorOp e)) (Free (ErrorOp _)) _)) =
  (Free (ErrorOp e), s)
step r s (Free (OneOfOp (Pure x) _ c)) =
  step r s $ c x
step r s (Free (OneOfOp _ (Pure y) c)) =
  step r s $ c y
step r s (Free (OneOfOp x y c)) =
  let (x', s') = step r s x
      (y', s'') = step r s' y
   in (Free (OneOfOp x' y' c), s'')

runEval :: EvalM a -> Either Error a
runEval initial = fst $ runEval' envEmpty stateInitial initial
  where
    runEval' :: Env -> State -> EvalM a -> (Either Error a, State)
    runEval' r s m =
      case step r s m of
        (Pure x, s') -> (Right x, s')
        (Free (ErrorOp e), s') -> (Left e, s')
        (m', s') -> runEval' r s' m'
