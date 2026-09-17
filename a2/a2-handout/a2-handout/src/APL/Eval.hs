module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)


type State = ([String], [(Val,Val)])

stateEmpty :: State
stateEmpty = ([],[])

addPrint :: State -> String -> State
addPrint (slist,kvs) news = ((slist ++ [news]),kvs)

addKVpair :: State -> Val -> Val -> State
addKVpair (slist,kvs) k v = (slist,((k,v) : kvs))

removeKVS :: (State, Either Error a) -> ([ String ], Either Error a)
removeKVS ((s,_), v) = (s,v)

type Env = ([(VName, Val)],State)

envEmpty :: Env
envEmpty = ([], stateEmpty)

envExtend :: VName -> Val -> Env -> Env
envExtend v val (env,st) = ((v, val) : env,st)

envLookup :: VName -> Env -> Maybe Val
envLookup v (env,_st) = lookup v env

kvsLookup :: Val -> Env -> Maybe Val
kvsLookup v (_ens,(_s,kvs)) = lookup v kvs

type Error = String

--newtype EvalM a = EvalM (Env -> Either Error a)
newtype EvalM a = EvalM (Env -> (State, Either Error a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \(_env,st) -> (st, Right x)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \(env,st) ->
    case x (env,st) of
      (st1, Left err) -> (st1, Left err)
      (st1, Right x') ->
        let EvalM y = f x'
         in y (env,st1)

askEnv :: EvalM Env
askEnv = EvalM $ \(env,st) -> (st, Right (env,st))

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \(_env,st) -> (st, Left s)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \(env,st) ->
  case m1 (env,st) of
    (st1, Left _) -> m2 (env,st1)
    (st1, Right x) -> (st1, Right x)

evalPrint :: String -> EvalM ()
evalPrint s = EvalM $ \(_env,st) -> ((addPrint st s), Right ())

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut k v = EvalM $ \(_env,st) -> ((addKVpair st k v), Right ())

evalKvGet :: Val -> EvalM Val
evalKvGet k = EvalM $ \(env,st) -> 
  case kvsLookup k (env,st) of
    Just x -> (st, Right x)
    Nothing -> (st, Left ("Invalid key: " ++ (show k)))

--runEval :: EvalM a -> Either Error a
runEval :: EvalM a -> ([ String ], Either Error a)
runEval (EvalM m) = removeKVS (m envEmpty)

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
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
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
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (ForLoop (loopparam, initial) (iv, bound) body) = do
  initial_v <- eval initial
  bound_v <- eval bound
  case bound_v of
    ValInt bound_int ->
      loop 0 bound_int initial_v
    _ ->
      failure "Non-integral loop bound"
  where
    loop i bound_int loop_v
      | i >= bound_int = pure loop_v
      | otherwise = do
          loop_v' <-
            localEnv (envExtend iv (ValInt i) . envExtend loopparam loop_v) $
              eval body
          loop (succ i) bound_int loop_v'
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print s e) = do
  v <- eval e
  case v of 
    (ValInt i) -> do 
      evalPrint (s ++ ": " ++ show i)
      pure $ ValInt i
    (ValBool b) -> do 
      evalPrint (s ++ ": " ++ show b)
      pure $ ValBool b
    (ValFun f_env var body) -> do 
      evalPrint (s ++ ": " ++ "#<fun>")
      pure (ValFun f_env var body)
eval (KvPut k_exp v_exp) = do
  k <- eval k_exp
  v <- eval v_exp
  evalKvPut k v
  pure v
eval (KvGet k_exp) = do
  k <- eval k_exp
  v <- evalKvGet k
  pure v