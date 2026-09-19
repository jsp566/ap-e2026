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
    [ testGroup
        "Constants"
        [ testPos (CstInt 5)
        , testPos (CstBool True)
        ]

    , testGroup
        "Variables"
        [ testNeg (Var "x")

        , testPos
            (Let "x" (CstInt 5) (Var "x"))

        , testPos
            (Lambda "x" (Var "x"))

        , testNeg
            (Lambda "x" (Var "y"))
        ]

    , testGroup
        "Arithmetic"
        [ testPos
            (Add (CstInt 2) (CstInt 3))

        , testNeg
            (Add (Var "x") (CstInt 3))

        , testPos
            (Sub (CstInt 5) (CstInt 2))

        , testNeg
            (Sub (CstInt 5) (Var "x"))

        , testPos
            (Mul (CstInt 2) (CstInt 4))

        , testNeg
            (Mul (Var "x") (CstInt 4))

        , testPos
            (Div (CstInt 8) (CstInt 2))

        , testNeg
            (Div (CstInt 8) (Var "x"))

        , testPos
            (Pow (CstInt 2) (CstInt 3))

        , testNeg
            (Pow (Var "x") (CstInt 3))
        ]

    , testGroup
        "Equality"
        [ testPos
            (Eql (CstInt 2) (CstInt 2))

        , testPos
            (Eql (CstBool True) (CstBool False))

        , testNeg
            (Eql (Var "x") (CstInt 2))

        , testNeg
            (Eql (CstBool True) (Var "x"))
        ]

    , testGroup
        "If"
        [ testPos
            (If
              (CstBool True)
              (CstInt 1)
              (CstInt 2))

        , testNeg
            (If
              (Var "x")
              (CstInt 1)
              (CstInt 2))

        , testNeg
            (If
              (CstBool True)
              (Var "x")
              (CstInt 2))

        , testNeg
            (If
              (CstBool True)
              (CstInt 1)
              (Var "x"))
        ]

    , testGroup
        "Let"
        [ testPos
            (Let
              "x"
              (CstInt 5)
              (Var "x"))

        , testNeg
            (Let
              "x"
              (Var "x")
              (CstInt 5))

        , testPos
            (Let
              "x"
              (CstInt 5)
              (Let "y" (CstInt 10) (Add (Var "x") (Var "y"))))

        , testNeg
            (Let
              "x"
              (CstInt 5)
              (Add (Var "x") (Var "y")))

        , testPos
            (Let
              "x"
              (CstInt 5)
              (Let "x" (CstBool True) (Var "x")))
        ]

    , testGroup
        "ForLoop"
        [ testPos
            (ForLoop
              ("p", CstInt 0)
              ("i", CstInt 10)
              (Add (Var "p") (Var "i")))

        , testNeg
            (ForLoop
              ("p", CstInt 0)
              ("i", CstInt 10)
              (Add (Var "p") (Var "x")))

        , testNeg
            (ForLoop
              ("p", Var "x")
              ("i", CstInt 10)
              (Var "p"))

        , testNeg
            (ForLoop
              ("p", CstInt 0)
              ("i", Var "x")
              (Var "p"))
        ]

    , testGroup
        "Lambda and Apply"
        [ testPos
            (Lambda "x" (Var "x"))

        , testPos
            (Lambda
              "x"
              (Add (Var "x") (CstInt 1)))

        , testNeg
            (Lambda "x" (Var "y"))

        , testPos
            (Apply
              (Lambda "x" (Var "x"))
              (CstInt 5))

        , testNeg
            (Apply
              (Lambda "x" (Var "y"))
              (CstInt 5))

        , testNeg
            (Apply
              (Var "f")
              (CstInt 5))
        ]

    , testGroup
        "TryCatch"
        [ testPos
            (TryCatch
              (CstInt 5)
              (CstInt 10))

        , testNeg
            (TryCatch
              (Var "x")
              (CstInt 10))

        , testNeg
            (TryCatch
              (CstInt 5)
              (Var "x"))
        ]

    , testGroup
        "Print"
        [ testPos
            (Print "foo" (CstInt 5))

        , testPos
            (Print "foo" (CstBool True))

        , testNeg
            (Print "foo" (Var "x"))

        , testPos
            (Let
              "x"
              (CstInt 5)
              (Print "foo" (Var "x")))
        ]

    , testGroup
        "Key-value store"
        [ testPos
            (KvPut
              (CstInt 0)
              (CstBool True))

        , testNeg
            (KvPut
              (Var "x")
              (CstBool True))

        , testNeg
            (KvPut
              (CstInt 0)
              (Var "x"))

        , testPos
            (KvGet (CstInt 0))

        , testNeg
            (KvGet (Var "x"))

        , testPos
            (Let
              "x"
              (CstInt 0)
              (KvGet (Var "x")))
        ]
    ]