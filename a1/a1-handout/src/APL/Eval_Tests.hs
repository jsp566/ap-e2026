module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- Consider this example when you have added the necessary constructors.
-- The Y combinator in a form suitable for strict evaluation.
yComb :: Exp
yComb =
  Lambda "f" $
    Apply
      (Lambda "g" (Apply (Var "g") (Var "g")))
      ( Lambda
          "g"
          ( Apply
              (Var "f")
              (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
          )
      )

fact :: Exp
fact =
  Apply yComb $
    Lambda "rec" $
      Lambda "n" $
        If
          (Eql (Var "n") (CstInt 10))
          (CstInt 1)
          (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
          --
          -- TODO - add more
      testCase "Loop Example" $
        eval 
          envEmpty 
          ( ForLoop 
              ("p", CstInt 0) 
              ("i", CstInt 10)
              (Add (Var "p") (Var "i")))
          @?= Right (ValInt 45),
      --
      testCase "Lambda Example" $
        eval 
          [] 
          (Let "x" (CstInt 2)
          (Lambda "y" (Add (Var "x") (Var "y"))))
          @?= Right (ValFun [("x",ValInt 2)] "y" (Add (Var "x") (Var "y"))),
      --
      testCase "Apply Example" $
        eval 
          [] 
          (Apply 
            (Let "x" (CstInt 2) 
            (Lambda "y" (Add (Var "x") (Var "y"))))
            (CstInt 3))
          @?= Right (ValInt 5),
      --
      testCase "TryCatch successful" $
        eval envEmpty (TryCatch (CstInt 5) (CstInt 10))
          @?= Right (ValInt 5),

      testCase "TryCatch failure" $
        eval envEmpty (TryCatch (Div (CstInt 5) (CstInt 0)) (CstInt 10))
          @?= Right (ValInt 10),
      testCase "Factorial 3" $
        eval envEmpty (Apply fact (CstInt 3))
          @?= Right (ValInt 6),

      testCase "Lambda Succesful" $
        eval
          [ ("y", ValInt 10) , ("z", ValInt 20)]
          (Lambda "x" (Add (Var "x") (Add (Var "y") (Var "z"))))
          @?= Right
            (ValFun
              [ ("y", ValInt 10)
              , ("z", ValInt 20)
              ]
              "x"
              (Add (Var "x") (Add (Var "y") (Var "z")))),

      --

      testCase "Lambda with boolean" $

        eval envEmpty
          (Lambda "x" (Eql (Var "x") (CstInt 5)))
          @?= Right
            (ValFun
              envEmpty
              "x"
              (Eql (Var "x") (CstInt 5))),


      testCase "Apply everything is successful" $
        eval
          [ ("y", ValInt 10)
          , ("z", ValInt 20)
          ]
          (Apply
            (Lambda "x" (Add (Var "x") (Add (Var "y") (Var "z"))))
            (CstInt 5))
          @?= Right (ValInt 35),

      testCase "Apply argument errors" $
        eval envEmpty
          (Apply
            (Lambda "x" (Var "x"))
            (Div (CstInt 5) (CstInt 0)))
          @?= Left "Division by zero",


      testCase "Apply not given function" $
        eval envEmpty
          (Apply
            (CstInt 5)
            (CstInt 2))
          @?= Left "First Exp does not evaluate to ValFun",

      testCase "Apply first expression errors" $
        eval envEmpty
          (Apply
            (Div (CstInt 5) (CstInt 0))
            (Var "SomeString"))
          @?= Left "Division by zero",

      --
      -- Add more here
      -- Loop tests
      -- non-integral loop bound
      -- initial fails
      -- p and i has same name
      -- p and i has same name so i becomes non integral
      -- mid loop fails

      -- Lambda tests
      -- valfun
      
      -- Apply tests
      -- positive test
      -- not a valfun
      
      -- two tests from top?

      -- Try-Catch tests
      -- try
      -- catch


      --

      testCase "True" $
        True
        @?= True
      



    ]
