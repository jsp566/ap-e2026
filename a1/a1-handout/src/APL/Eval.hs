module APL.Eval
  ( Val (..),
    Env,
    envEmpty,
    eval,
  )
where

import APL.AST (Exp (..), VName)

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

evalIntBinOp :: (Integer -> Integer -> Either Error Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp f env e1 e2 =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> case f x y of
      Left err -> Left err
      Right z -> Right $ ValInt z
    (Right _, Right _) -> Left "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp' f env e1 e2 =
  evalIntBinOp f' env e1 e2
  where
    f' x y = Right $ f x y



forloop :: Env -> VName -> Integer -> Integer -> Exp -> VName -> Either Error Val
forloop env i iint n body p = 
  if iint < n
  then case eval env body of
    Right x -> 
      let newenv = envExtend p x env
          newiint = iint + 1
      -- what if i gets updated above^? then should we get I again? No
      in forloop (envExtend i (ValInt newiint) newenv) i newiint n body p
    Left err -> Left err
  else eval env (Var p)

eval :: Env -> Exp -> Either Error Val
eval _env (CstInt x) = Right $ ValInt x
eval _env (CstBool b) = Right $ ValBool b
eval env (Var v) = case envLookup v env of
  Just x -> Right x
  Nothing -> Left $ "Unknown variable: " ++ v
eval env (Add e1 e2) = evalIntBinOp' (+) env e1 e2
eval env (Sub e1 e2) = evalIntBinOp' (-) env e1 e2
eval env (Mul e1 e2) = evalIntBinOp' (*) env e1 e2
eval env (Div e1 e2) = evalIntBinOp checkedDiv env e1 e2
  where
    checkedDiv _ 0 = Left "Division by zero"
    checkedDiv x y = Right $ x `div` y
eval env (Pow e1 e2) = evalIntBinOp checkedPow env e1 e2
  where
    checkedPow x y =
      if y < 0
        then Left "Negative exponent"
        else Right $ x ^ y
eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Invalid operands to equality"
eval env (If cond e1 e2) =
  case eval env cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "Non-boolean conditional."
eval env (Let var e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right v -> eval (envExtend var v env) e2
-- TODO: Add cases after extending Exp.
eval env (ForLoop (p, initial) (i, bound) body) = 
  case (eval env initial, eval env bound) of
    (Right v, Right (ValInt n)) -> forloop (envExtend p v (envExtend i (ValInt 0) env)) i 0 n body p
    (Right _, Right _) -> Left "Non-integral loop bound"
    (Left err, _) -> Left err
    (_, Left err) -> Left err        
eval env (Lambda vname bodyexp) =
  Right $ ValFun env vname bodyexp
eval env (Apply funexp argexp) =
  case (eval env funexp) of
    Right (ValFun funenv vname bodyexp) -> 
      case (eval env argexp) of
        Left err -> Left err
        Right arg -> eval (envExtend vname arg funenv) bodyexp
    Right _ -> Left "First Exp does not evaluate to ValFun"
    Left err -> Left err
eval env (TryCatch exp1 exp2) =
  case eval env exp1 of 
    Right v -> Right v
    Left _ -> eval env exp2
    


