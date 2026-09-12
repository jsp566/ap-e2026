module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), eval, envEmpty)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "CstInt" $
        eval envEmpty (CstInt 2)
          @?= Right (ValInt 2),
      testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 3))
          @?= Right (ValInt 5),
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 3) (CstInt 1))
          @?= Right (ValInt 2),
      testCase "Mul" $
        eval envEmpty (Mul (CstInt 2) (CstInt 3))
          @?= Right (ValInt 6),
      testCase "Div" $
        eval envEmpty (Div (CstInt 6) (CstInt 3))
          @?= Right (ValInt 2),
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      testCase "DivByZero" $
        eval envEmpty (Div (CstInt 6) (CstInt 0))
          @?= Left "Division by Zero",
      testCase "NegExp" $
        eval envEmpty (Pow (CstInt 6) (CstInt (-3)))
          @?= Left "Negative Exponent",
      testCase "EqlBool" $
        eval envEmpty (Eql (CstBool (False)) (CstBool (False)))
          @?= Right (ValBool True),
      testCase "EqlInt" $
        eval envEmpty (Eql (CstInt (-3)) (CstInt (-3)))
          @?= Right (ValBool True),
      testCase "EqlTypeMismatch" $
        eval envEmpty (Eql (CstInt 6) (CstBool (False)))
          @?= Left "Type Mismatch",
      testCase "If True" $
        eval envEmpty (If (CstBool True) (CstInt (-3)) (CstInt 6))
          @?= Right (ValInt (-3)),
      testCase "If False" $
        eval envEmpty (If (CstBool False) (Pow (CstInt 6) (CstInt (-3))) (CstInt 6))
          @?= Right (ValInt 6),
      testCase "If Number" $
        eval envEmpty (If (CstInt (-3)) (CstInt (-3)) (CstBool True))
          @?= Left "Condition evaluates to integer",
      testCase "Full Test1" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "x")))
          @?= Right (ValInt 6),
      testCase "Full Test2" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "y")))
          @?= Left "Unknown variable: y"
    ]
