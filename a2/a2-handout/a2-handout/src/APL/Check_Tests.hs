module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [ testCase "Example 1" $
        checkExp (CstInt 2) 
          @?= Nothing,
      --
      testCase "Example 2" $
        checkExp (Var "x")
          @?= Just "Variable not in scope: x",
      --
      testCase "Example 3" $
        checkExp (Lambda "x" (Var "x"))
          @?= Nothing
      -- Add more tests:
      -- check simple positive

      -- check simple fail

      -- check let

      -- check forloop

      -- check Lambda

      -- check apply?

      -- check KvPut then KvGet
      
    ]
