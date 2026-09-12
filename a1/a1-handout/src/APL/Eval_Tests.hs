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
          (Eql (Var "n") (CstInt 0))
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
            (CstInt 2))
          @?= Left "Division by zero",

      --
      -- Add more here
      -- Loop tests
      -- initial fails
      testCase "Loop initial fails" $
        eval [] (ForLoop ("p", (Var "initialmissing")) ("i", (Var "boundmissing")) (Var "bodymissing"))
          @?= Left "Unknown variable: initialmissing",
      -- bound fails
      testCase "Loop bound fails" $
        eval [] (ForLoop ("p", (CstBool True)) ("i", (Var "boundmissing")) (Var "bodymissing"))
          @?= Left "Unknown variable: boundmissing",
      -- non-integral loop bound
      testCase "Loop non-integral bound" $
        eval [] (ForLoop ("p", (CstBool True)) ("i", (CstBool True)) (Var "bodymissing"))
          @?= Left "Non-integral loop bound",
      -- p and i has same name so i becomes non integral
      testCase "Loop non-integral loop incrementer" $
        eval [] (ForLoop ("ip", (CstBool True)) ("ip", (CstInt 10)) (Var "bodymissing"))
          @?= Left "Non-integral loop incrementer",
      -- p and i has same name so i becomes non integral in the body
      testCase "Loop non-integral loop incrementer" $
        eval [] (ForLoop ("ip", (CstInt 1)) ("ip", (CstInt 10)) (CstBool True))
          @?= Left "Non-integral loop incrementer",
      -- mid loop fails
      testCase "Loop body fails" $
        eval [] (ForLoop ("p", (CstInt 5)) ("i", (CstInt 10)) (Var "bodymissing"))
          @?= Left "Unknown variable: bodymissing",
      -- positive test
      testCase "Loop positive test" $
        eval [] (ForLoop ("p", (CstInt 1)) ("i", (CstInt 10)) (Mul (Var "p") (Add (Var "i") (CstInt 1))))
          @?= Right (ValInt 3628800),
      -- p and i has same name
      testCase "Loop i and p same name and body adds 20 to i" $
        eval [] (ForLoop ("ip", (CstInt 3)) ("ip", (CstInt 10)) (Add (Var "ip") (CstInt 20)))
          @?= Right (ValInt 24),
      testCase "Loop i and p same name and body subtracts 1 from i" $
        eval [] (ForLoop ("ip", (CstInt 1)) ("ip", (CstInt 10)) (Sub (Var "ip") (CstInt 1)))
          @?= Left "Timeout",

      -- Lambda tests
      -- valfun
      
      -- Apply tests
      -- positive test
      -- not a valfun
      -- argument gives error
      
      -- two tests from top?

      -- Try-Catch tests
      -- try
      -- catch


      --

      testCase "True" $
        True
        @?= True
      



    ]
