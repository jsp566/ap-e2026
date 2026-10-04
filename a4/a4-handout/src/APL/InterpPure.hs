module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval evalm =
  let (ps, result) = runEval' envEmpty stateInitial evalm
   in case result of
       Left _ -> (ps, Left "Break outside loop")
       Right res -> (ps, res)
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Val (Either Error a))
    runEval' _ _ (Pure x) = ([], pure (pure x))
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Right (Left e))
    runEval' r s (Free (TryCatchOp m1 m2 k)) = 
      let (p, res) = runEval' r s m1
       in case res of
            Left val -> (p, Left val)
            Right res' -> 
              let 
                newm = case res' of
                  Right val' -> k val'
                  Left _ -> m2 >>= k
                (ps, newres) = runEval' r s newm
              in (p ++ ps, newres)
    runEval' r s (Free (KvGetOp key k)) = 
      case lookup key s of
        Just val -> runEval' r s $ k val
        Nothing -> ([], Right (Left ("Invalid key: " ++ show key)))
    runEval' r s (Free (KvPutOp key val m)) = 
      let s' = (key, val) : s
      in runEval' r s' m
    runEval' r s (Free (TransactionOp m k)) =
      runEval' r s (m >>= k)
    runEval' r s (Free (LoopingOp m k)) =
      let (p, res) = runEval' r s m
       in case res of
            Right (Left err) -> (p, Right (Left err))
            Right (Right val) ->
              let (ps, newres) = runEval' r s (k val)
               in (p ++ ps, newres)
            Left val ->
              let (ps, newres) = runEval' r s (k val)
               in (p ++ ps, newres)
    runEval' _ _ (Free (BreakLoopOp val _)) =
      ([], Left val)

      
