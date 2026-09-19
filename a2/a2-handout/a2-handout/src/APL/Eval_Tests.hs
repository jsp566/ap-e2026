module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--eval' :: Exp -> Either Error Val
eval' :: Exp -> ([ String ], Either Error Val)
eval' = runEval . eval

evalTests :: TestTree
evalTests =
  testGroup
    "EValuation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt 7)),
      --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?= ([], Left "Non-integer operand"),
      --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt (-3))),
      --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= ([], Right (ValInt 2)),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= ([], Right (ValInt 8)),
      --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= ([], Right (ValInt 1)),
      --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= ([], Left "Negative exponent"),
      --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= ([], Right (ValBool False)),
      --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= ([], Right (ValBool True)),
      --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= ([], Right (ValInt 2)),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "ForLoop" $
        eval'
          (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
          @?= ([], Right (ValInt 45)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= ([], Right (ValInt 16)),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= ([], Right (ValBool True))
    ]


printTests :: TestTree
printTests =
  testGroup
    "Task 1: Printing"
    [ testCase "Example 1" $
        eval' ( 
          Print "foo" $ CstInt 2)
          @?= (["foo: 2"],Right (ValInt 2)),
      --
      testCase "Example 2" $
        eval' ( 
          Let "x" (Print "foo" $ CstInt 2) 
          (Print "bar" $ CstInt 3))
          @?= (["foo: 2","bar: 3"],Right (ValInt 3)),
      --
      testCase "Example 3" $
        eval' (
          Let "x" (Print "foo" $ CstInt 2) 
          (Var "bar"))
          @?= (["foo: 2"],Left "Unknown variable: bar"),
          
      testCase "Print integer" $
        eval'
          (Print "foo" (CstInt 2))
          @?= (["foo: 2"], Right (ValInt 2))

    , testCase "Print boolean" $
        eval'
          (Print "answer" (CstBool True))
          @?= (["answer: True"], Right (ValBool True))

    , testCase "Print function" $
        eval'
          (Print "answer" (Lambda "x" (Var "x")))
          @?= (["answer: #<fun>"],Right (ValFun ([],([],[])) "x" (Var "x")))

    , testCase "Print returns value" $
        eval'
          (Let
            "x"
            (Print "foo" (CstInt 2))
            (Add (Var "x") (CstInt 3)))
          @?= (["foo: 2"], Right (ValInt 5))

    , testCase "Print multiple values in order" $
        eval'
          (Let
            "_"
            (Print "first" (CstInt 1))
            (Print "second" (CstInt 2)))
          @?= (["first: 1", "second: 2"], Right (ValInt 2))

    , testCase "Print function and apply" $
        eval'
          (Let
            "f"
            (Print "fun" (Lambda "x" (Var "x")))
            (Apply (Var "f") (CstInt 10)))
          @?= (["fun: #<fun>"], Right (ValInt 10))

    , testCase "Print before later error" $
        eval'
          (Let
            "_"
            (Print "foo" (CstInt 2))
            (Var "bar"))
          @?= (["foo: 2"], Left "Unknown variable: bar")

    , testCase "TryCatch preserves printed output" $
        eval'
          (TryCatch
            (Let
              "_"
              (Print "before" (CstInt 1))
              (Div (CstInt 1) (CstInt 0)))
            (Print "after" (CstInt 2)))
          @?= (["before: 1", "after: 2"], Right (ValInt 2))

      -- printing in for loop
      -- printing in lambda
      
      -- printing in apply
    ]


kvTests :: TestTree
kvTests =
  testGroup
    "Task 2: Key-value store"
    [ testCase "Example 1" $
        eval' ( 
          Let "x" (KvPut (CstInt 0) (CstBool True)) 
          (KvGet (CstInt 0)))
          @?= ([],Right (ValBool True)),
      --
      testCase "Example 2" $
        eval' (
          Let "x" (KvPut (CstInt 0) (CstBool True)) 
          (KvGet (CstInt 1)))
          @?= ([],Left "Invalid key: ValInt 1"),
      --
      testCase "Example 3" $
        eval' ( 
          Let "x" (KvPut (CstInt 0) (CstBool True)) 
          (Let "y" (KvPut (CstInt 0) (CstBool False)) 
          (KvGet (CstInt 0))))
          @?= ([],Right (ValBool False)),
          
      testCase "KvPut returns value" $
        eval'
          (KvPut (CstInt 0) (CstBool True))
          @?= ([],Right (ValBool True))

    , testCase "KvPut then KvGet" $
        eval'
          (Let
            "_"
            (KvPut (CstInt 0) (CstBool True))
            (KvGet (CstInt 0)))
          @?= ([],Right (ValBool True))

    , testCase "KvGet invalid key" $
        eval'
          (Let
            "_"
            (KvPut (CstInt 0) (CstBool True))
            (KvGet (CstInt 1)))
          @?= ([],Left "Invalid key: ValInt 1")

    , testCase "KvPut replaces existing key" $
        eval'
          (Let
            "_"
            (KvPut (CstInt 0) (CstBool True))
            (Let
              "_"
              (KvPut (CstInt 0) (CstBool False))
              (KvGet (CstInt 0))))
          @?= ([],Right (ValBool False))

    , testCase "Boolean key" $
        eval'
          (Let
            "_"
            (KvPut (CstBool True) (CstInt 42))
            (KvGet (CstBool True)))
          @?= ([],Right (ValInt 42))

    , testCase "KvPut and Print share state" $
        eval'
          (Let
            "_"
            (KvPut (CstInt 0) (CstBool True))
            (Print "stored" (KvGet (CstInt 0))))
          @?= (["stored: True"], Right (ValBool True))

    , testCase "KvGet failure" $
        eval'
          (KvGet (CstInt 99))
          @?= ([], Left "Invalid key: ValInt 99")

      -- Try putkey and fail keeps key in catch
      -- putkey in for loop
      -- putkey in lambda
      -- putkey in apply
    ]


tests :: TestTree
tests = testGroup "Evaluation" [evalTests, printTests, kvTests]
