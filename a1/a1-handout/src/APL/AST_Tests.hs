module APL.AST_Tests (tests) where

import APL.AST (Exp (..),printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [ testCase "CstInt" $
        printExp (CstInt 7)
          @?= "7",
      --
      testCase "CstBool false" $
        printExp (CstBool False)
          @?= "false",
      --
      testCase "CstBool true" $
        printExp (CstBool True)
          @?= "true",
      -- Add 
      testCase "Adding" $
        printExp (Add (CstInt 5) (CstInt 3))
          @?= "5 + 3",
      -- Sub
      testCase "Subtracting" $
        printExp (Sub (CstInt 69) (CstInt 2))
          @?= "69 - 2",
      testCase "Subtracting negative values" $
        printExp (Sub (CstInt (-1)) (CstInt 2))
          @?= "-1 - 2",
      -- Mul
      testCase "Multiplying " $
        printExp (Mul (CstInt (-1)) (CstInt 2))
          @?= "-1 * 2",
      -- Div
      testCase "Dividing " $
        printExp (Div (CstInt (-1)) (CstInt 2))
          @?= "-1 / 2",
      testCase "Dividing by 0" $
        printExp (Div (CstInt (-1)) (CstInt 0))
          @?= "-1 / 0",
      -- Pow
      testCase "Exponenting" $
        printExp (Pow (CstInt (-1)) (CstInt 2))
          @?= "-1 ** 2",
      testCase "Exponenting by 0" $
        printExp (Pow (CstInt (-1)) (CstInt 0))
          @?= "-1 ** 0",
      -- Eql
      testCase "Equating" $
        printExp (Eql (CstInt (-1)) (CstInt 0))
          @?= "-1 == 0",

      -- If
      testCase "If'ing" $
        printExp (If (CstBool True) (CstInt 5) (CstInt 10))
          @?= "if true then 5 else 10",

      -- Var
        testCase "Var'ing" $
          printExp (Var "x")
            @?= "x",

      -- Let
      testCase "Let'ing" $
        printExp
          (Let "x" (CstInt 5) (Add (Var "x") (CstInt 2)))
            @?= "let x = 5 in (x + 2)",

      -- ForLoop
      testCase "ForLoop'ing" $
        printExp (ForLoop ("p", CstInt 0) ("i", CstInt 5) (Add (Var "p") (Var "i")))
          @?= "loop p = 0 for i < 5 do (p + i)",


      -- Lambda
      testCase "Lambda'ing" $
        printExp (Lambda "x" (Add (Var "x") (CstInt 2)))
          @?= "\\x -> (x + 2)",


      -- Apply
      testCase "Applying" $
        printExp (Apply (Var "f") (CstInt 5))
          @?= "f 5",

      testCase "Applying with lambda" $
        printExp (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 5))
          @?= "(\\x -> (x + 1)) 5",

      -- Apply with another apply not parenthesized


      -- TryCatch
      testCase "TryCatch" $
        printExp (TryCatch (Div (CstInt 5) (CstInt 0)) (CstInt 10))
          @?= "try (5 / 0) catch 10",

      testCase "True" $
        True
        @?= True
    ]
