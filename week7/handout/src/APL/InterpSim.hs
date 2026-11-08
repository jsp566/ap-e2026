module APL.InterpSim (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

-- | Continue execution of the provided computation as far as
-- possible, but executing at most one 'StepOp' effect. Any nested
-- computations (in 'BothOp' and 'OneOfOp') must also be stepped
-- similarly. If the computation is stuck on a 'KvGetOp' for which the
-- key is not in the state, then the computation is merely returned
-- unchanged.
--
-- Evaluation of 'BothOp':
--
-- * If either of the nested computations are 'Free (ErrorOp ...)',
--   then propagate that error.
--
-- * If both are 'Pure', then return a pair of the results.
--
-- * Otherwise evaluate both one step.
--
-- Evaluation of 'OneOfOp':
--
-- * If both of the nested computations are 'Free (ErrorOp ...)', then
--   propagate one of the errors.
--
-- * If one is 'Pure', then return that result.
--
-- * Otherwise evaluate both one step.
step :: Env -> State -> EvalM a -> (EvalM a, State)
step _ s (Pure x) = (Pure x, s)
step r s (Free (ReadOp k)) = step r s $ k r
step r s (Free (KvGetOp key k)) =
    case lookup key s of
    Nothing -> ((Free (KvGetOp key k)), s)
    Just val -> step r s $ k val
step r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter ((/= key) . fst) s
       in step r s' m
step _ s (Free (ErrorOp e)) = (Free (ErrorOp e), s)
step _ s (Free (StepOp m)) = (m, s)
step _ s (Free (BothOfOp (Free (ErrorOp e)) _ _)) = (Free (ErrorOp e), s)
step _ s (Free (BothOfOp _ (Free (ErrorOp e)) _)) = (Free (ErrorOp e), s)
step r s (Free (BothOfOp (Pure x) (Pure y) c)) = step r s $ c $ ValTuple [x, y]
step r s (Free (BothOfOp x y c)) =
    let (x', s') = step r s x
        (y', s'') = step r s' y
    in step r s'' (Free (BothOfOp x' y' c))
step _ s (Free (OneOfOp (Free (ErrorOp e1)) (Free (ErrorOp _)) _)) = (Free (ErrorOp e1), s)
step r s (Free (OneOfOp (Pure x) _ c)) = step r s $ c x
step r s (Free (OneOfOp _ (Pure y) c)) = step r s $ c y
step r s (Free (OneOfOp x y c)) =
    let (x', s') = step r s x
        (y', s'') = step r s' y
    in step r s'' (Free (OneOfOp x' y' c))


runEval :: EvalM a -> Either Error a
runEval initial = fst $ runEval' envEmpty stateInitial initial
  where
    runEval' :: Env -> State -> EvalM a -> (Either Error a, State)
    runEval' r s m =
      case step r s m of
        (Pure x, s') -> (Right x, s')
        (Free (ErrorOp e), s') -> (Left e, s')
        (m', s') -> runEval' r s' m'
    