module APL.Eval
  ( eval,
  )
where

import APL.AST (Exp (..))
import APL.Monad
import Data.List (genericDrop)

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "if: non-boolean conditional"
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ evalStep $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (KvPut key_e val_e) = do
  key <- eval key_e
  val <- eval val_e
  evalKvPut key val
  pure val
eval (KvGet key_e) = do
  key <- eval key_e
  evalKvGet key
eval (ForLoop (loopparam, initial) (iv, bound) body) = do
  initial_v <- eval initial
  bound_v <- eval bound
  case bound_v of
    ValInt bound_int ->
      loop 0 bound_int initial_v
    _ ->
      failure "for-loop: non-integer bound"
  where
    loop i bound_int loop_v
      | i >= bound_int = pure loop_v
      | otherwise = do
          loop_v' <-
            evalStep $
              localEnv (envExtend iv (ValInt i) . envExtend loopparam loop_v) $
                eval body
          loop (succ i) bound_int loop_v'
eval (Tuple es) =
  ValTuple <$> mapM eval es
eval (Project e i) = do
  v <- eval e
  case v of
    ValTuple vs ->
      case genericDrop i vs of
        [] -> failure "project: invalid index"
        v' : _ -> pure v'
    _ -> failure "project: non-pair"
eval (WhileLoop (loopparam, initial) cond body) = do
  initial_v <- eval initial
  loop initial_v
  where
    loop loop_v = do
      cond_v <- localEnv (envExtend loopparam loop_v) $ eval cond
      case cond_v of
        ValBool False -> pure loop_v
        ValBool True -> do
          loop_v' <- evalStep $ localEnv (envExtend loopparam loop_v) $ eval body
          loop loop_v'
        _ -> failure "while-loop: non-boolean condition"
eval (BothOf e1 e2) =
  evalBothOf (eval e1) (eval e2)
eval (OneOf e1 e2) =
  evalOneOf (eval e1) (eval e2)
