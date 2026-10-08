module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, printExp, VName)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import APL.Parser (parseAPL, keywords)
import APL.Eval (eval, runEval)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , frequency
  , elements
  , listOf
  , choose
  , vectorOf
  , sized
  , withMaxSuccess
  )

instance Arbitrary Exp where
  arbitrary = sized genExp'

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genVar :: Gen VName
genVar = do
    alpha <- elements $ ['a' .. 'z'] ++ ['A' .. 'Z']
    alphaNums <- 
      frequency 
        [ (1, listOf $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])
        , (1, do
            n <- choose (1,3)
            vectorOf n $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])]
    let v = alpha : alphaNums
    if v `elem` keywords
      then genVar
      else pure v

genExp' :: Int -> Gen Exp
genExp' = genExp []

genExp :: [VName] -> Int -> Gen Exp
genExp vlist 0 = 
  frequency $ case vlist of 
      [] -> glist
      _ -> (20, Var <$> elements vlist) : glist
  where 
    glist = 
      [ (20, CstInt <$> arbitrary)
      , (20, CstBool <$> arbitrary)
      , (1, Var <$> genVar)]
genExp vlist size =
  frequency $ case vlist of 
      [] -> glist
      _ -> (20, Var <$> elements vlist) : glist
  where 
    glist = 
      [ (20, CstInt <$> arbitrary)
      , (20, CstBool <$> arbitrary)
      , (20, Add <$> genExp vlist halfSize <*> genExp vlist halfSize)
      , (20, Sub <$> genExp vlist halfSize <*> genExp vlist halfSize)
      , (20, Mul <$> genExp vlist halfSize <*> genExp vlist halfSize)
      , (20, Div <$> genExp vlist halfSize <*> genExp vlist halfSize)
      , (20, Pow <$> genExp vlist halfSize <*> genExp vlist halfSize)
      , (20, Eql <$> genExp vlist halfSize <*> genExp vlist halfSize)
      , (20, If <$> genExp vlist thirdSize <*> genExp vlist thirdSize <*> genExp vlist thirdSize)
      , (1, Var <$> genVar)
      , (20, do
          x <- genVar
          Let <$> pure x <*> genExp vlist halfSize <*> genExp (x : vlist) halfSize)
      , (20,  do
          x <- genVar
          Lambda <$> pure x <*> genExp (x : vlist) (size - 1))
      , (20, Apply <$> genExp vlist halfSize <*> genExp vlist halfSize)
      , (20, TryCatch <$> genExp vlist halfSize <*> genExp vlist halfSize)
      ]
    halfSize = size `div` 2
    thirdSize = size `div` 3

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted e = case parseAPL "" (printExp e) of 
  Left _ -> False
  Right e' -> e == e'

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e = case (runEval . eval) e of
  Left err -> elem err (checkExp e)
  Right _ -> True 

-- The number of tests is part of the specification of this test suite: some of
-- these properties fail only rarely.  Do not reduce it.
properties :: [(String, Property)]
properties =
  [ ("expCoverage", property $ withMaxSuccess 10000 expCoverage)
  , ("parsePrinted", property $ withMaxSuccess 10000 parsePrinted)
  , ("onlyCheckedErrors", property $ withMaxSuccess 10000 onlyCheckedErrors)
  ]
