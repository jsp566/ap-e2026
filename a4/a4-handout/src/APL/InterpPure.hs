module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' _ _ (Free (TryCatchOp m1 m2 k)) = error "TODO"
    runEval' _ _ (Free (KvGetOp key k)) = error "TODO"
    runEval' _ _ (Free (KvPutOp key val m)) = error "TODO"
    runEval' _ _ (Free (TransactionOp m k)) = error "TODO"
