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
        eval' (TryCatch (Eql (CstInt 0) (CstBool True)) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Right (ValInt 1)),
      -- 
      --
      testCase "Key-value Store Example 1" $
        runEval 
          (Free $ (KvPutOp (ValInt 0) (ValInt 1)) (Free $ KvGetOp (ValInt 0) $ \val-> pure val))
          @?= ([],Right (ValInt 1)),
      --
      testCase "Transaction Example 1" $
        eval' (
          Let "_" (Transaction (KvPut (CstInt 0) (CstInt 1))) 
          (KvGet (CstInt 0)))
          @?= ([],Right (ValInt 1)),
      --
      testCase "Transaction Example 2" $
        eval' (
          TryCatch (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))) 
          (KvGet (CstInt 0)))
          @?= ([],Left "Invalid key: ValInt 0"),
      --
      testCase "Transaction Example 3" $
        eval' (
          Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
          @?= ([],Left "Unknown variable: die"),
      --
      testCase "Transaction Example 4" $
        eval' (
          Transaction (Let "_" (KvPut (Print "foo" (CstInt 0)) (CstBool False)) (Var "die")))
          @?= (["foo: 0"],Left "Unknown variable: die"),
      --
      testCase "Transaction Example 5" $
        eval' (
          Let "_" (Transaction
            (Let "_" (KvPut (CstInt 0) (CstInt 1))
              (TryCatch (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
                (CstBool True))))
          (KvGet (CstInt 0)))
          @?= ([],Right (ValInt 1)),
      --
      testCase "Transaction Example 6" $
        eval' (
          Let "_" (TryCatch (Transaction
            (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))))
              (CstBool True))
          (KvGet (CstInt 0)))
          @?= ([],Left "Invalid key: ValInt 0"),


      testCase "Break Example 1" $
        eval' (ForLoop ("p", CstInt 0) ("i", CstInt 100) $ Let "_" (Break (CstBool True)) (Var "i"))
          @?= ([],Right (ValBool True)),
      --
      testCase "Break Example 2" $
        eval' (Break (CstBool True))
          @?= ([],Left "Break outside loop")
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        testCase "print 2" $ do
            (out, res) <-
              captureIO [] $
                evalIO' $
                  Print "This is also 1" $
                    Print "This is 1" $
                      CstInt 1
            (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1),
        --
        testCase "Missing key test" $ do
            (_, res) <-
              captureIO ["ValInt 1"] $
                runEvalIO $
                  Free $ KvGetOp (ValInt 0) $ \val-> pure val
            res @?= Right (ValInt 1)

    ]
