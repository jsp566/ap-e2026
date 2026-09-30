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
    runEval' r s (Free (TryCatchOp m1 m2 k)) = 
      case runEval' r s m1 of
        (p, Left _) -> 
          let (ps, newres) = runEval' r s (m2 >>= k)
          in (p ++ ps, newres)
        (p, Right _) -> 
          let (ps, newres) = runEval' r s (m1 >>= k)
          in (p ++ ps, newres)
    runEval' r s (Free (KvGetOp key k)) = 
      case lookup key s of
        Just val -> runEval' r s $ k val
        Nothing -> ([], Left ("Invalid key: " ++ show key))
    runEval' r s (Free (KvPutOp key val m)) = 
      let s' = (key, val) : s
      in runEval' r s' m
    runEval' r s (Free (TransactionOp m k)) =
      case runEval' r s m of
        (p, Left err) -> (p, Left err)
        (p, Right _) -> 
          let (ps, newres) = runEval' r s (m >>= k)
          in (p ++ ps, newres)

      
