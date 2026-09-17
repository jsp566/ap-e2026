module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String

type Env = [VName]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Env -> Env
envExtend v env = v : env

envLookup :: VName -> Env -> Bool
envLookup v env = elem v env

newtype CheckM a = CheckM (Env -> Maybe Error) -- TODO - give this a proper definition.

instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  pure _ = CheckM $ \_env -> Nothing
  (<*>) = ap

instance Monad CheckM where
  CheckM x >>= f = CheckM $ \env ->
    case x env of
      Just err -> Just err
      Nothing ->
        let CheckM y = f undefined
         in y env

localEnv :: (Env -> Env) -> CheckM a -> CheckM a
localEnv f (CheckM m) = CheckM $ \env -> m (f env)

--failure :: String -> CheckM a
--failure s = CheckM $ \_env -> Just s

check :: Exp -> CheckM ()
check (CstInt _) = pure ()
check (CstBool _) = pure ()
check (Var v) = CheckM $ \env -> 
  if envLookup v env
  then Nothing
  else Just ("Variable not in scope: " ++ v)
check (Add e1 e2) = do
  check e1
  check e2
check (Sub e1 e2) = do
  check e1
  check e2
check (Mul e1 e2) = do
  check e1
  check e2
check (Div e1 e2) = do
  check e1
  check e2
check (Pow e1 e2) = do
  check e1
  check e2
check (Eql e1 e2) = do
  check e1
  check e2
check (If cond e1 e2) = do
  check cond
  check e1
  check e2
check (Let var e1 e2) = do
  check e1
  localEnv (envExtend var) $ check e2
check (ForLoop (loopparam, initial) (iv, bound) body) = do
  check initial
  check bound
  localEnv (envExtend iv . envExtend loopparam) $ check body
check (Lambda var body) =
  localEnv (envExtend var) $ check body
check (Apply e1 e2) = do
  check e1
  check e2
check (TryCatch e1 e2) = do
  check e1
  check e2
check (Print _ e) = check e
check (KvPut k_exp v_exp) = do
  check k_exp
  check v_exp
check (KvGet k_exp) = check k_exp


runCheck :: CheckM a -> Maybe Error
runCheck (CheckM m) = m envEmpty

checkExp :: Exp -> Maybe Error
checkExp = runCheck . check
