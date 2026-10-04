module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),

      --

      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),

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

      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),

      --

      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),

      --

      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),

      --

      testCase "TryCatchOp Example 1" $
        runEval
          (Free $ TryCatchOp (failure "Oh no!") (pure $ ValInt 1) pure)
          @?= ([], Right (ValInt 1)),

      --

      testCase "TryCatchOp Example 2" $
        eval' (TryCatch (CstInt 5) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Right (ValInt 5)),

      --

      testCase "TryCatchOp Example 3" $
        eval'
          (TryCatch
            (Eql (CstInt 0) (CstBool True))
            (Div (CstInt 1) (CstInt 0)))
          @?= ([], Left "Division by zero"),


      testCase "TryCatch catches failure" $
        eval'
          (TryCatch
            (Div (CstInt 1) (CstInt 0))
            (CstInt 42))
          @?= ([], Right (ValInt 42)),

      testCase "TryCatch keeps successful result" $
        eval'
          (TryCatch
            (CstInt 5)
            (CstInt 10))
          @?= ([], Right (ValInt 5)),

      testCase "TryCatch propagates fallback failure" $
        eval'
          (TryCatch
            (Div (CstInt 1) (CstInt 0))
            (Div (CstInt 2) (CstInt 0)))
          @?= ([], Left "Division by zero"),


      testCase "KvPut followed by KvGet" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 1)),

      --

      testCase "KvGet missing key" $
        runEval
          (evalKvGet (ValInt 0))
          @?= ([], Left "Invalid key: ValInt 0"),

      --

      testCase "KvPut overwrites existing key" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvPut (ValInt 0) (ValInt 2)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 2)),

      --

      testCase "KvPut stores Bool values" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValBool True)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValBool True)),

      --

      testCase "KvGet does not change state" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValInt 1)
              _ <- evalKvGet (ValInt 0)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 1)),


    testCase "Transaction commits successful changes" $
      eval'
        (Let
          "_"
          (Transaction
            (KvPut (CstInt 0) (CstInt 1)))
          (KvGet (CstInt 0)))
        @?= ([], Right (ValInt 1)),

    testCase "Failed transaction rolls back changes" $
      eval'
        (TryCatch
          (Transaction
            (Let
              "_"
              (KvPut (CstInt 0) (CstBool False))
              (Var "die")))
          (KvGet (CstInt 0)))
        @?= ([], Left "Invalid key: ValInt 0"),

    testCase "Failed transaction returns error" $
      eval'
        (Transaction
          (Let
            "_"
            (KvPut (CstInt 0) (CstBool False))
            (Var "die")))
        @?= ([], Left "Unknown variable: die"),

    testCase "Transaction preserves output" $
      eval'
        (Transaction
          (Let
            "_"
            (Print "foo" (CstInt 0))
            (CstInt 42)))
        @?= (["foo: 0"], Right (ValInt 42)),

    testCase "Nested transaction rolls back inner changes" $
      eval'
        (Let
          "_"
          (Transaction
            (Let
              "_"
              (KvPut (CstInt 0) (CstInt 1))
              (TryCatch
                (Transaction
                  (Let
                    "_"
                    (KvPut (CstInt 0) (CstBool False))
                    (Var "die")))
                (CstBool True))))
          (KvGet (CstInt 0)))
        @?= ([], Right (ValInt 1)),

    testCase "Transaction returns its result" $
      eval'
        (Transaction (CstInt 42))
        @?= ([], Right (ValInt 42)),

      testCase "Transaction Example 1" $
        eval'
          ( Let
              "_"
              (Transaction (KvPut (CstInt 0) (CstInt 1)))
              (KvGet (CstInt 0))
          )
          @?= ([], Right (ValInt 1)),

      --

      testCase "Transaction Example 2" $
        eval'
          ( TryCatch
              ( Transaction
                  (Let
                    "_"
                    (KvPut (CstInt 0) (CstBool False))
                    (Var "die"))
              )
              (KvGet (CstInt 0))
          )
          @?= ([], Left "Invalid key: ValInt 0"),

      --

      testCase "Transaction Example 3" $
        eval'
          (Transaction
            (Let
              "_"
              (KvPut (CstInt 0) (CstBool False))
              (Var "die")))
          @?= ([], Left "Unknown variable: die"),

      --

      testCase "Transaction Example 4" $
        eval'
          (Transaction
            (Let
              "_"
              (KvPut (Print "foo" (CstInt 0)) (CstBool False))
              (Var "die")))
          @?= (["foo: 0"], Left "Unknown variable: die"),

      --

      testCase "Transaction Example 5" $
        eval'
          ( Let
              "_"
              (Transaction
                (Let
                  "_"
                  (KvPut (CstInt 0) (CstInt 1))
                  (TryCatch
                    (Transaction
                      (Let
                        "_"
                        (KvPut (CstInt 0) (CstBool False))
                        (Var "die")))
                    (CstBool True))))
              (KvGet (CstInt 0))
          )
          @?= ([], Right (ValInt 1)),

      --

      testCase "Transaction Example 6" $
        eval'
          ( Let
              "_"
              (TryCatch
                (Transaction
                  (Transaction
                    (Let
                      "_"
                      (KvPut (CstInt 0) (CstBool False))
                      (Var "die"))))
                (CstBool True))
              (KvGet (CstInt 0))
          )
          @?= ([], Left "Invalid key: ValInt 0"),

      --

      testCase "Break Example 1" $
        eval'
          (ForLoop
            ("p", CstInt 0)
            ("i", CstInt 100)
            (Let "_" (Break (CstBool True)) (Var "i")))
          @?= ([], Right (ValBool True)),

      --

      testCase "Break Example 2" $
        eval' (Break (CstBool True))
          @?= ([], Left "Break outside loop"),

      testCase "Break returns integer" $
        eval'
          (ForLoop
            ("p", CstInt 0)
            ("i", CstInt 100)
            (Break (CstInt 42)))
          @?= ([], Right (ValInt 42))
    ]


ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print 1" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),

      --

      testCase "print 2" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (Print "This is also 1" $
                Print "This is 1" $
                  CstInt 1)
        (out, res)
          @?= ( ["This is 1: 1", "This is also 1: 1"]
              , Right $ ValInt 1
              ),

      testCase "TryCatch catches failure" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (TryCatch
                (Div (CstInt 1) (CstInt 0))
                (CstInt 42))

        (out, res) @?= ([], Right (ValInt 42)),

      testCase "TryCatch keeps successful result" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (TryCatch
                (CstInt 5)
                (CstInt 10))

        (out, res) @?= ([], Right (ValInt 5)),

      testCase "TryCatch propagates fallback failure" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (TryCatch
                (Div (CstInt 1) (CstInt 0))
                (Div (CstInt 2) (CstInt 0)))

        (out, res) @?= ([], Left "Division by zero"),

      testCase "TryCatchOp Example 1" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO
              (Free $
                TryCatchOp
                  (failure "Oh no!")
                  (pure $ ValInt 1)
                  pure)
        (out, res) @?= ([], Right (ValInt 1)),

      --

      testCase "TryCatchOp Example 2" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (TryCatch
                (CstInt 5)
                (Div (CstInt 1) (CstInt 0)))
        (out, res) @?= ([], Right (ValInt 5)),

      --

      testCase "TryCatchOp Example 3" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (TryCatch
                (Eql (CstInt 0) (CstBool True))
                (Div (CstInt 1) (CstInt 0)))
        (out, res) @?= ([], Left "Division by zero"),

      --

      testCase "KvPut followed by KvGet" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvGet (ValInt 0)
        (out, res) @?= ([], Right (ValInt 1)),


      --

      testCase "KvPut overwrites existing key" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvPut (ValInt 0) (ValInt 2)
              evalKvGet (ValInt 0)
        (out, res) @?= ([], Right (ValInt 2)),

      --

      testCase "KvPut stores Bool values" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValBool True)
              evalKvGet (ValInt 0)
        (out, res) @?= ([], Right (ValBool True)),

      --

      testCase "KvPut and Print" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValInt 1)
              evalPrint "hello"
              evalKvGet (ValInt 0)
        (out, res) @?= (["hello"], Right (ValInt 1)),
      testCase "Missing key with integer replacement" $ do
        (out, res) <-
          captureIO ["ValInt 5"] $
            runEvalIO $
              evalKvGet (ValInt 0)
        (out, res) @?= ([], Right (ValInt 5)),

      testCase "Missing key with boolean replacement" $ do
        (out, res) <-
          captureIO ["ValBool True"] $
            runEvalIO $
              evalKvGet (ValInt 0)
        (out, res) @?= ([], Right (ValBool True)),

      testCase "Missing key with invalid replacement" $ do
        (out, res) <-
          captureIO ["lol"] $
            runEvalIO $
              evalKvGet (ValInt 0)
        (out, res) @?= ([], Left "Invalid value input: lol"),

      testCase "Missing key replacement is not stored" $ do
        (out, res) <-
          captureIO ["ValInt 5", "ValInt 10"] $
            runEvalIO $ do
              _ <- evalKvGet (ValInt 0)
              evalKvGet (ValInt 0)
        (out, res) @?= ([], Right (ValInt 10)),


      testCase "Existing key does not prompt" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValInt 42)
              evalKvGet (ValInt 0)
        (out, res) @?= ([], Right (ValInt 42)),
      --

      testCase "Transaction commits successful changes" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (Let
                "_"
                (Transaction
                  (KvPut (CstInt 0) (CstInt 1)))
                (KvGet (CstInt 0)))

        (out, res) @?= ([], Right (ValInt 1)),

      testCase "Failed transaction rolls back changes" $ do
        (out, res) <-
          captureIO ["ValInt 42"] $
            evalIO'
              (TryCatch
                (Transaction
                  (Let
                    "_"
                    (KvPut (CstInt 0) (CstBool False))
                    (Var "die")))
                (KvGet (CstInt 0)))

        (out, res)
          @?= ( ["Invalid key: ValInt 0. Enter a replacement: "]
              , Right (ValInt 42)
              ),

      testCase "Failed transaction returns error" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (Transaction
                (Let
                  "_"
                  (KvPut (CstInt 0) (CstBool False))
                  (Var "die")))

        (out, res) @?= ([], Left "Unknown variable: die"),

      testCase "Transaction preserves output" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (Transaction
                (Let
                  "_"
                  (Print "foo" (CstInt 0))
                  (CstInt 42)))

        (out, res) @?= (["foo: 0"], Right (ValInt 42)),

      testCase "Nested transaction rolls back inner changes" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (Let
                "_"
                (Transaction
                  (Let
                    "_"
                    (KvPut (CstInt 0) (CstInt 1))
                    (TryCatch
                      (Transaction
                        (Let
                          "_"
                          (KvPut (CstInt 0) (CstBool False))
                          (Var "die")))
                      (CstBool True))))
                (KvGet (CstInt 0)))

        (out, res) @?= ([], Right (ValInt 1)),

      testCase "Transaction returns its result" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (Transaction (CstInt 42))

        (out, res) @?= ([], Right (ValInt 42)),
        
      testCase "Transaction Example 1" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              ( Let
                  "_"
                  (Transaction (KvPut (CstInt 0) (CstInt 1)))
                  (KvGet (CstInt 0))
              )
        (out, res) @?= ([], Right (ValInt 1)),

      --

      testCase "Transaction Example 2" $ do
        (out, res) <-
          captureIO ["42"] $
            evalIO' (
              TryCatch (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))) 
              (KvGet (CstInt 0)))
        (out, res) @?= (["Invalid key: ValInt 0. Enter a replacement: "] , Left "Invalid value input: 42"),

      --

      testCase "Transaction Example 3" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (Transaction
                (Let
                  "_"
                  (KvPut (CstInt 0) (CstBool False))
                  (Var "die")))
        (out, res) @?= ([], Left "Unknown variable: die"),

      --

      testCase "Transaction Example 4" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (Transaction
                (Let
                  "_"
                  (KvPut (Print "foo" (CstInt 0)) (CstBool False))
                  (Var "die")))
        (out, res) @?= (["foo: 0"], Left "Unknown variable: die"),

      --

      testCase "Transaction Example 5" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              ( Let
                  "_"
                  (Transaction
                    (Let
                      "_"
                      (KvPut (CstInt 0) (CstInt 1))
                      (TryCatch
                        (Transaction
                          (Let
                            "_"
                            (KvPut (CstInt 0) (CstBool False))
                            (Var "die")))
                        (CstBool True))))
                  (KvGet (CstInt 0))
              )
        (out, res) @?= ([], Right (ValInt 1)),

      --

      testCase "Transaction Example 6" $ do
        (out, res) <-
          captureIO ["42"] $
            evalIO' (
              Let "_" (TryCatch (Transaction
                (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))))
                 (CstBool True))
              (KvGet (CstInt 0)))
        (out, res) @?= (["Invalid key: ValInt 0. Enter a replacement: "],Left "Invalid value input: 42"),

      --

      testCase "Break Example 1" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (ForLoop
                ("p", CstInt 0)
                ("i", CstInt 100)
                (Let "_" (Break (CstBool True)) (Var "i")))
        (out, res) @?= ([], Right (ValBool True)),

      

      testCase "Break Example 2" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (Break (CstBool True))
        (out, res) @?= ([], Left "Break outside loop)"),

      testCase "Break returns integer" $ do
        (out, res) <-
          captureIO [] $
            evalIO'
              (ForLoop
                ("p", CstInt 0)
                ("i", CstInt 100)
                (Break (CstInt 42)))

        (out, res) @?= ([], Right (ValInt 42)),

      testCase "Break preserves output before break" $
        eval'
          (ForLoop
            ("p", CstInt 0)
            ("i", CstInt 100)
            (Let
              "_"
              (Print "before break" (CstInt 0))
              (Break (CstBool True))))
          @?= (["before break: 0"], Right (ValBool True))
    ]