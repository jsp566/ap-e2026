module APL.Eval
  (Val (..),
  eval,
  envEmpty
  )
where

import APL.AST (Exp (..), VName)

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]


-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend vname val env = (vname, val) : env

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup vname env = lookup vname env



makeVal :: Either Error Integer -> Either Error Val
makeVal x = 
  case x of
    Left err -> Left err
    Right z -> Right $ ValInt z


evalWithOperator :: Env -> Exp -> (Integer -> Integer -> Either Error Integer) -> Exp -> Either Error Val
evalWithOperator env exp1 f exp2 = 
  case (eval env exp1, eval env exp2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> makeVal (f x y)


makeOutputCorrect :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> Either Error Integer)
makeOutputCorrect f = newf
 where newf x y = Right $ f x y

safeEval :: Env -> Exp -> (Integer -> Integer -> Integer) -> Exp -> Either Error Val
safeEval env exp1 f exp2 = 
  evalWithOperator env exp1 (makeOutputCorrect f) exp2


divchecked :: Integer -> Integer -> Either Error Integer
divchecked x y = 
  case y of
    0 -> Left "Division by Zero"
    _ -> Right $ div x y

powchecked :: Integer -> Integer -> Either Error Integer
powchecked x y = 
  if y < 0 then Left "Negative Exponent"
  else Right $ (^) x y

equalityCheck :: Env -> Exp -> Exp -> Either Error Val
equalityCheck env exp1 exp2 = 
  case (eval env exp1, eval env exp2) of
    (Right (ValInt x), Right (ValInt y)) -> Right (ValBool (x == y))
    (Right (ValBool x), Right (ValBool y)) -> Right (ValBool (x == y))
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (_, _) -> Left "Type Mismatch"
    
ifEval :: Env -> Exp -> Exp -> Exp -> Either Error Val
ifEval env exp1 exp2 exp3 = 
  case (eval env exp1) of
    (Left err) -> Left err
    (Right (ValBool True)) -> eval env exp2 
    (Right (ValBool False)) -> eval env exp3 
    (Right (ValInt _)) -> Left "Condition evaluates to integer"


lookupToVal :: VName -> Env -> Either Error Val
lookupToVal vname env = 
  case (envLookup vname env) of
    Just x -> Right $x
    Nothing -> Left $ "Unknown variable: " ++ vname


eval :: Env -> Exp -> Either Error Val
eval env (CstInt x) = Right $ ValInt x
eval env (CstBool x) = Right $ ValBool x
eval env (Add exp1 exp2) = safeEval env exp1 (+) exp2
eval env (Sub exp1 exp2) = safeEval env exp1 (-) exp2
eval env (Mul exp1 exp2) = safeEval env exp1 (*) exp2
eval env (Div exp1 exp2) = evalWithOperator env exp1 divchecked exp2
eval env (Pow exp1 exp2) = evalWithOperator env exp1 powchecked exp2
eval env (Eql exp1 exp2) = equalityCheck env exp1 exp2
eval env (If exp1 exp2 exp3) = ifEval env exp1 exp2 exp3
eval env (Var vname) = lookupToVal vname env
eval env (Let vname exp1 exp2) = case eval env exp1 of
    Left err -> Left err
    Right val -> eval (envExtend vname val env) exp2
