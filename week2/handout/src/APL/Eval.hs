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

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Env -> Either Error a) -- TODO

instance Functor EvalM where
  -- (<$>) :: (a -> b) -> EvalM a -> EvalM b
  fmap f (EvalM x) =
    EvalM $ \env -> case x env of
      Right v -> Right $ f v
      Left err -> Left err

instance Applicative EvalM where
  -- pure :: a -> EvalM a
  pure a = EvalM (\_env -> Right a)
  -- (<*>) :: EvalM (a -> b) -> EvalM a -> EvalM b
  EvalM ef <*> EvalM ex = EvalM $ \env ->
    case (ef env, ex env) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right f, Right x) -> Right (f x)

instance Monad EvalM where
  -- (>>=) :: EvalM a -> (a -> EvalM b) -> EvalM b
  EvalM x >>= f = EvalM $ \env ->
    case x env of
      Left err -> Left err
      Right x' ->
        let EvalM y = f x'
         in y env


runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x envEmpty -- TODO


failure :: String -> EvalM a
failure x = EvalM (\_env -> Left x)


intOperations :: Env -> Exp -> (Integer -> Integer -> EvalM Integer) -> Exp -> EvalM Val
intOperations env e1 op e2 = do
  x <- eval env e1
  y <- eval env e2 
  case (x, y) of
    (ValInt x', ValInt y') -> ValInt <$> op x' y'
    _ -> failure "Non-integer operand"

safeOp :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> EvalM Integer)
safeOp f = newf
 where newf x y = pure $ f x y

safeEval :: Env -> Exp -> (Integer -> Integer -> Integer) -> Exp -> EvalM Val
safeEval env exp1 f exp2 = 
  intOperations env exp1 (safeOp f) exp2

divchecked :: Integer -> Integer -> EvalM Integer
divchecked x y = 
  case y of
    0 -> failure "Division by Zero"
    _ -> pure $ div x y

powchecked :: Integer -> Integer -> EvalM Integer
powchecked x y = 
  if y < 0 then failure "Negative Exponent"
  else pure $ (^) x y


catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM x) (EvalM y) = EvalM $ \env ->
  case x env of 
    Left _ -> y env
    Right x -> Right x


askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)


eval :: Env -> Exp -> EvalM Val -- TODO
eval _ (CstInt x) = pure $ ValInt x
eval _ (CstBool x) = pure $ ValBool x
eval env (Add e1 e2) = safeEval env e1 (+) e2
eval env (Sub e1 e2) = safeEval env e1 (-) e2
eval env (Mul e1 e2) = safeEval env e1 (*) e2
eval env (Div e1 e2) = intOperations env e1 divchecked e2
eval env (Pow e1 e2) = intOperations env e1 powchecked e2
eval env (Eql e1 e2) = undefined
eval env (If e1 e2 e3) = undefined
eval env (Var v) = do
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval env (Let v e1 e2) = undefined
eval env (ForLoop (p, initial) (i, bound) body) = undefined
eval env (Lambda vname bodyexp) = pure $ ValFun env vname bodyexp
eval env (Apply funexp argexp) = do
  v1 <- eval env funexp
  v2 <- eval env argexp
  case (v1) of
    (ValFun funenv vname bodyexp) -> eval (envExtend vname v2 funenv) bodyexp
    _ -> failure "First Exp does not evaluate to ValFun"
eval env (TryCatch e1 e2) = catch (eval env e1) (eval env e2)