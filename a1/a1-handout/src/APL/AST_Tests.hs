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

      -- Sub

      -- Mul

      -- Div

      -- Pow

      -- Eql

      -- If

      -- Var

      -- Let

      -- ForLoop

      -- Lambda

      -- Apply

      -- TryCatch

      testCase "True" $
        True
        @?= True
    ]
