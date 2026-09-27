module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "truex" $ Var "truex",
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ],
      testGroup
        "Task 1: Function application"
        [ 
          parserTest "x y" $
                      Apply (Var "x") (Var "y"),

          parserTest "x y z" $
            Apply
              (Apply (Var "x") (Var "y"))
              (Var "z"),

          parserTest "x(y z)" $
            Apply
              (Var "x")
              (Apply (Var "y") (Var "z")),

          parserTestFail "x if x then y else z"
        ],
      testGroup
        "Task 2: Equality and power operators"
        [ parserTest "x**y" $
            Pow (Var "x") (Var "y"),
         parserTestFail "x * * y", 
          parserTest "x**y**z" $
            Pow
              (Var "x")
              (Pow (Var "y") (Var "z")),

          parserTest "x**y*z" $
            Mul
              (Pow (Var "x") (Var "y"))
              (Var "z"),

          parserTest "x*y**z" $
            Mul
              (Var "x")
              (Pow (Var "y") (Var "z")),

          parserTest "x**y+z" $
            Add
              (Pow (Var "x") (Var "y"))
              (Var "z"),

          parserTest "x**y==z" $
            Eql
              (Pow (Var "x") (Var "y"))
              (Var "z"),

          parserTest "(x**y)**z" $
            Pow
              (Pow (Var "x") (Var "y"))
              (Var "z"),
              
          parserTest "x==y" $
              Eql (Var "x") (Var "y"),

            parserTest "x==y==z" $
              Eql
                (Eql (Var "x") (Var "y"))
                (Var "z"),

            parserTest "x+y==y+x" $
              Eql
                (Add (Var "x") (Var "y"))
                (Add (Var "y") (Var "x")),

            parserTest "x*y==y*x" $
              Eql
                (Mul (Var "x") (Var "y"))
                (Mul (Var "y") (Var "x"))

        ],
      testGroup
        "Task 3: Printing, putting, and getting"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y"),
          parserTest "getx" $ Var "getx",
          parserTest "print \"foo\" x" $ Print "foo" (Var "x"),
          parserTest "print \"hello world\" x" $
            Print "hello world" (Var "x"),

          parserTest "print \"\" x" $
            Print "" (Var "x"),

          parserTest "put (x + y) z" $
            KvPut
              (Add (Var "x") (Var "y"))
              (Var "z")
      ],
      testGroup
        "Task 4: Lambdas, let-binding, loops, and try-catch"
        [ parserTest "let x = y in z" $ Let "x" (Var "y") (Var "z"),
          parserTest "let x = 2 in x" $ Let "x" (CstInt 2) (Var "x"),
          parserTestFail "let true = y in z",
          parserTestFail "x let v = 2 in v",
          parserTestFail "let x = y",
          parserTest "\\x -> x + x" $ Lambda "x" (Add (Var "x") (Var "x")),
          parserTest "\\x -> x * 2" $ Lambda "x" (Mul (Var "x") (CstInt 2)),
          parserTest "(\\x -> x) + x" $ Add (Lambda "x" (Var "x")) (Var "x"),
          parserTest "\\x -> x x" $ Lambda "x" (Apply (Var "x") (Var "x")),
          parserTestFail "\\if -> x",
          parserTest "try x catch y" $ TryCatch (Var "x") (Var "y"),
          parserTest "try x + y catch z" $
            TryCatch (Add (Var "x") (Var "y")) (Var "z"),
          parserTestFail "try x",
          parserTestFail "try x catch",
          parserTest "loop p = x for i < y do z" $
            ForLoop ("p", Var "x") ("i", Var "y") (Var "z"),
          parserTest "loop p = 0 for i < 10 do p + i" $
            ForLoop
              ("p", CstInt 0)
              ("i", CstInt 10)
              (Add (Var "p") (Var "i")),
          parserTestFail "loop p = x for i do z",
          parserTestFail "loop p = x for i < y z",
          parserTest "letx" $ Var "letx",
          parserTest "tryx" $ Var "tryx",
          parserTest "loopx" $ Var "loopx",
          parserTestFail "loop if = x for i do z"

        ]
    ]
