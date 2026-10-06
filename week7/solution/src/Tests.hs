-- | Omnibus test suite grouped by which task each test applies to.
-- This is supposed to ease grading.
module Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpConcurrent as Concurrent
import APL.InterpPure as Pure
import APL.InterpSim as Sim
import APL.InterpStep as Step
import APL.Monad (Val (..))
import APL.Parser (parseAPL)
import Control.Concurrent (forkIO, newChan, readChan, threadDelay, writeChan)
import KVDB (kvGet, kvPut, startKVDB)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

infiniteLoop :: Exp
infiniteLoop =
  WhileLoop ("x", CstInt 0) (CstBool True) (Var "x")

finiteLoop :: Exp
finiteLoop =
  ForLoop
    ("x", CstInt 1)
    ("i", CstInt 10)
    (Mul (Var "x") (CstInt 2))

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

pureEvalTest :: String -> Exp -> Val -> TestTree
pureEvalTest desc e v =
  testCase desc $
    Pure.runEval (eval e) @?= Right v

pureEvalTestFail :: String -> Exp -> TestTree
pureEvalTestFail desc e =
  testCase desc $
    case Pure.runEval (eval e) of
      Left _ -> pure ()
      Right v ->
        assertFailure $
          "Expected error but received this value:\n" ++ show v

concurrentEvalTest :: String -> Exp -> Val -> TestTree
concurrentEvalTest desc e v = testCase desc $ do
  res <- Concurrent.runEval $ eval e
  res @?= Right v

concurrentEvalTestFail :: String -> Exp -> TestTree
concurrentEvalTestFail desc e = testCase desc $ do
  res <- Concurrent.runEval $ eval e
  case res of
    Right _ -> assertFailure "Expected failure."
    Left _ -> pure ()

simEvalTest :: String -> Exp -> Val -> TestTree
simEvalTest desc e v = testCase desc $ do
  Sim.runEval (eval e) @?= Right v

simEvalTestFail :: String -> Exp -> TestTree
simEvalTestFail desc e = testCase desc $
  case Sim.runEval (eval e) of
    Right _ -> assertFailure "Expected failure."
    Left _ -> pure ()

taskA :: TestTree
taskA =
  testGroup
    "Task A: tuples"
    [ testGroup
        "Parsing"
        [ testGroup
            "Tuples"
            [ parserTest "(x,y)" $ Tuple [Var "x", Var "y"],
              parserTest "(x,y,z)" $ Tuple [Var "x", Var "y", Var "z"],
              parserTest "()" $ Tuple [],
              parserTestFail "(a,",
              parserTestFail "(a,b,",
              parserTest "(a)" $ Var "a"
            ],
          testGroup
            "Projection"
            [ parserTest "x.0" $ Project (Var "x") 0,
              parserTest "x.0.1" $ Project (Project (Var "x") 0) 1,
              parserTest "(x.0).1" $ Project (Project (Var "x") 0) 1,
              parserTestFail "x.(y.0)",
              parserTest "x .0" $ Project (Var "x") 0,
              parserTest "x. 0" $ Project (Var "x") 0,
              parserTest "x . 0" $ Project (Var "x") 0,
              parserTest "x . 0 . 1" $ Project (Project (Var "x") 0) 1,
              parserTest "((\\x -> x) x).0" $ Project (Apply (Lambda "x" (Var "x")) (Var "x")) 0
            ]
        ],
      testGroup
        "Evaluation"
        [ testGroup
            "Tuples"
            [ evalTest
                "(e1,e2)"
                (Tuple [CstInt 1, CstInt 2])
                (ValTuple [ValInt 1, ValInt 2]),
              evalTest
                "(put 0 1, get 0)"
                (Tuple [KvPut (CstInt 0) (CstInt 1), KvGet (CstInt 0)])
                (ValTuple [ValInt 1, ValInt 1]),
              evalTestFail
                "(get 0, put 0 1)"
                (Tuple [KvGet (CstInt 0), KvPut (CstInt 0) (CstInt 1)])
            ],
          testGroup
            "Projection"
            [ evalTest
                "(e1,e2).0"
                (Project (Tuple [CstInt 1, CstInt 2]) 0)
                (ValInt 1),
              evalTest
                "(e1,e2).1"
                (Project (Tuple [CstInt 1, CstInt 2]) 1)
                (ValInt 2),
              evalTestFail
                "(e1,e2).2"
                (Project (Tuple [CstInt 1, CstInt 2]) 2),
              evalTestFail
                "1.2"
                (Project (CstInt 1) 2)
            ]
        ]
    ]
  where
    evalTest = pureEvalTest
    evalTestFail = pureEvalTestFail

taskB :: TestTree
taskB =
  testGroup
    "Task B: loops and stepping"
    [ testGroup
        "Parsing"
        [ parserTestFail "loop",
          parserTestFail "do",
          parserTestFail "for",
          parserTestFail "while",
          parserTest "loop x=y for i<n do x" $
            ForLoop ("x", Var "y") ("i", Var "n") (Var "x"),
          parserTest "loop x=y for i < n do x" $
            ForLoop ("x", Var "y") ("i", Var "n") (Var "x"),
          parserTestFail "loop x=y for i<n dox",
          parserTest "loop x=y while x do x" $
            WhileLoop ("x", Var "y") (Var "x") (Var "x")
        ],
      testGroup
        "Stepping"
        [ testCase "for-loop" $
            Step.runEval
              (eval $ ForLoop ("x", CstInt 0) ("i", CstInt 4) (Var "x"))
              @?= 4,
          testCase "for-loop (use iterator)" $
            Step.runEval
              (eval $ ForLoop ("x", CstInt 0) ("i", CstInt 4) (Add (Var "x") (Var "i")))
              @?= 4,
          testCase "while-loop" $
            Step.runEval
              ( eval $
                  ( WhileLoop
                      ("x", Tuple [CstInt 1, CstInt 10])
                      (If (Eql (Project (Var "x") 1) (CstInt 0)) (CstBool False) (CstBool True))
                      (Tuple [Mul (Project (Var "x") 0) (CstInt 2), Sub (Project (Var "x") 1) (CstInt 1)])
                  )
              )
              @?= 10,
          testCase "application" $
            Step.runEval
              ( eval $
                  Let
                    "f"
                    (Lambda "x" (Var "x"))
                    ( Add
                        (Apply (Var "f") (CstInt 2))
                        (Apply (Var "f") (CstInt 2))
                    )
              )
              @?= 2
        ],
      testGroup
        "Evaluation"
        [ evalTest
            "For loop"
            (ForLoop ("x", CstInt 1) ("i", CstInt 10) (Mul (Var "x") (CstInt 2)))
            (ValInt 1024),
          --
          evalTest
            "For loop zero"
            (ForLoop ("x", CstInt 42) ("i", CstInt 0) (Add (Var "x") (Var "x")))
            (ValInt 42),
          --
          evalTest
            "For loop negative"
            (ForLoop ("x", CstInt 42) ("i", CstInt (-1)) (Add (Var "x") (Var "x")))
            (ValInt 42),
          --
          evalTest
            "For loop depend on i"
            (ForLoop ("acc", CstInt 0) ("i", CstInt 101) (Add (Var "acc") (Var "i")))
            (ValInt 5050),
          --
          evalTestFail
            "For loop: p not in scope when evaluating the bound"
            (ForLoop ("p", CstInt 100) ("i", Var "p") (Add (Var "p") (Var "i"))),
          --
          evalTestFail
            "For loop noninteger"
            (ForLoop ("x", CstInt 42) ("i", CstBool True) (Add (Var "x") (Var "x"))),
          --
          evalTest
            "While loop"
            (WhileLoop ("x", Tuple [CstInt 1, CstInt 10]) (If (Eql (Project (Var "x") 1) (CstInt 0)) (CstBool False) (CstBool True)) (Tuple [Mul (Project (Var "x") 0) (CstInt 2), Sub (Project (Var "x") 1) (CstInt 1)]))
            (ValTuple [ValInt 1024, ValInt 0]),
          --
          evalTest
            "While loop false"
            (WhileLoop ("x", CstInt 42) (CstBool False) (Add (Var "x") (Var "x")))
            (ValInt 42),
          --
          evalTestFail
            "While loop nonbool"
            (WhileLoop ("x", CstInt 42) (CstInt 0) (Add (Var "x") (Var "x")))
        ]
    ]
  where
    evalTest = pureEvalTest
    evalTestFail = pureEvalTestFail

taskC :: TestTree
taskC =
  testGroup
    "Task C: parsing && and ||, InterpPure"
    [ testGroup
        "Parsing"
        [ parserTest "x&&y&&z" $
            BothOf (BothOf (Var "x") (Var "y")) (Var "z"),
          parserTest "x||y||z" $
            OneOf (OneOf (Var "x") (Var "y")) (Var "z"),
          parserTest "x||y&&z" $
            OneOf (Var "x") (BothOf (Var "y") (Var "z")),
          parserTest "x+y&&y*z" $
            BothOf (Add (Var "x") (Var "y")) (Mul (Var "y") (Var "z")),
          parserTest "x+y||y*z" $
            OneOf (Add (Var "x") (Var "y")) (Mul (Var "y") (Var "z"))
        ],
      testGroup
        "Interpretation (pure)"
        [ evalTest
            "e1 && e2"
            (BothOf (CstInt 0) (CstInt 1))
            (ValTuple [ValInt 0, ValInt 1]),
          --
          evalTestFail
            "e1 && e2 (first fails)"
            (BothOf (KvGet (CstInt 0)) (CstInt 1)),
          --
          evalTestFail
            "e1 && e2 (second fails)"
            (BothOf (KvGet (CstInt 0)) (CstInt 1)),
          --
          evalTest
            "e1 || e2"
            (OneOf (CstInt 0) (CstInt 1))
            (ValInt 0),
          --
          evalTest
            "e1 || e2 (first fails)"
            (OneOf (KvGet (CstInt 0)) (CstInt 1))
            (ValInt 1),
          --
          evalTestFail
            "e1 || e2 (both fails)"
            (OneOf (KvGet (CstInt 0)) (KvGet (CstInt 0))),
          --
          -- Strictly speaking we don't require any particular error
          -- messages, but the division-by-zero one is defined in the
          -- handout, so hopefully it is stable.
          testCase "e1 || e2 (both fails, check error is second)" $
            Pure.runEval
              (eval (OneOf (KvGet (CstInt 0)) (Div (CstInt 0) (CstInt 0))))
              @?= Left "Division by zero"
        ]
    ]
  where
    evalTest = pureEvalTest
    evalTestFail = pureEvalTestFail

-- Concurrent expressions.
expectingVal :: [(String, Exp, Val)]
expectingVal =
  [ ( "Infinite and finite loop",
      (OneOf infiniteLoop (BothOf (CstInt 0) finiteLoop)),
      (ValTuple [ValInt 0, ValInt 1024])
    ),
    ( "Finite and infinite loop",
      (OneOf (BothOf (CstInt 0) finiteLoop) infiniteLoop),
      (ValTuple [ValInt 0, ValInt 1024])
    ),
    ( "Concurrent put/get",
      BothOf
        (KvGet (CstInt 42))
        (KvPut (CstInt 42) finiteLoop),
      (ValTuple [ValInt 1024, ValInt 1024])
    ),
    -- Note that it is OK for students to pick the error if they
    -- explicitly state so in their report.
    ( "OneOf error left",
      (OneOf (Div (CstInt 42) (CstInt 0)) finiteLoop),
      (ValInt 1024)
    ),
    ( "OneOf error right",
      (OneOf finiteLoop (Div (CstInt 42) (CstInt 0))),
      (ValInt 1024)
    ),
    ( "client/server (may timeout if eval of for-loops doesn't bind the iterator in the body)",
      Let
        "requestTok"
        (Lambda "request" (Var "request"))
        ( Let
            "respondTok"
            (Lambda "respond" (Var "respond"))
            ( Let
                "client"
                (Lambda "ignore" (ForLoop ("acc", CstInt 1) ("i", CstInt 10) (Let "ignore" (KvPut (Tuple [Var "requestTok", Var "i"]) (Var "acc")) (Add (Var "acc") (KvGet (Tuple [Var "respondTok", Var "i"]))))))
                ( Let
                    "server"
                    (Lambda "ignore" (WhileLoop ("clock", CstInt 0) (CstBool True) (Let "r" (KvGet (Tuple [Var "requestTok", Var "clock"])) (Let "ignore" (KvPut (Tuple [Var "respondTok", Var "clock"]) (Mul (Var "r") (CstInt 2))) (Add (Var "clock") (CstInt 1))))))
                    (OneOf (Apply (Var "client") (CstBool True)) (Apply (Var "server") (CstBool True)))
                )
            )
        ),
      ValInt 59049
    ),
    ( "OneOf <failure> <long computation> (warning: may time out on slow impls!)",
      OneOf
        (CstInt 1 `Div` CstInt 0)
        ( ForLoop
            ("x", CstInt 0)
            ("i", CstInt 999999)
            (Var "x" `Add` CstInt 1)
        ),
      ValInt 999999
    ),
    ( "OneOf <failure> <medium computation>",
      OneOf
        (CstInt 1 `Div` CstInt 0)
        ( ForLoop
            ("x", CstInt 0)
            ("i", CstInt 9999)
            (Var "x" `Add` CstInt 1)
        ),
      ValInt 9999
    ),
    ( "OneOf <failure> <short computation>",
      OneOf
        (CstInt 1 `Div` CstInt 0)
        ( ForLoop
            ("x", CstInt 0)
            ("i", CstInt 99)
            (Var "x" `Add` CstInt 1)
        ),
      ValInt 99
    ),
    ( "Blocking get",
      OneOf
        (KvGet (CstInt 0))
        ( ForLoop
            ("x", CstInt 0)
            ("i", CstInt 3)
            ( If
                (Eql (Var "i") (CstInt 1))
                (KvPut (CstInt 0) (CstInt 1))
                (If (Eql (Var "i") (CstInt 2)) (Div (CstInt 0) (CstInt 0)) (CstInt (-1)))
            )
        ),
      ValInt 1
    )
  ]

-- Failing concurrent expressions.
expectingError :: [(String, Exp)]
expectingError =
  [ ( "Error",
      Div (CstInt 42) (CstInt 0)
    ),
    ( "BothOf error",
      BothOf (CstInt 0) (Div (CstInt 42) (CstInt 0))
    ),
    ( "OneOf <failure> <failure>",
      OneOf
        (CstInt 1 `Div` CstInt 0)
        (CstInt 1 `Div` CstInt 0)
    )
  ]

taskD :: TestTree
taskD =
  testGroup
    "Task D: InterpSim"
    $ map (\(desc, e, val) -> simEvalTest desc e val) expectingVal
      ++ map (\(desc, e) -> simEvalTestFail desc e) expectingError

taskE :: TestTree
taskE =
  testGroup
    "Task E: KVDB"
    [ testCase "put 1 2 -> get" $ do
        kvdb <- startKVDB
        kvPut kvdb (1 :: Int) (2 :: Int)
        x <- kvGet kvdb 1
        x @?= 2,
      testCase "put False True -> get" $ do
        kvdb <- startKVDB
        kvPut kvdb True False
        x <- kvGet kvdb True
        x @?= False,
      testCase "put True False -> put True True -> get" $ do
        kvdb <- startKVDB
        kvPut kvdb True False
        kvPut kvdb True True
        x <- kvGet kvdb True
        x @?= True,
      testCase "blocking" $ do
        kvdb <- startKVDB
        c <- newChan
        _ <- forkIO $ writeChan c =<< kvGet kvdb True
        threadDelay 1000
        kvPut kvdb True 42
        x <- readChan c
        x @?= (42 :: Int),
      testCase "blocking (two threads)" $ do
        kvdb <- startKVDB
        c1 <- newChan
        c2 <- newChan
        _ <- forkIO $ writeChan c1 =<< kvGet kvdb True
        _ <- forkIO $ writeChan c2 =<< kvGet kvdb True
        threadDelay 1000
        kvPut kvdb True 42
        x <- readChan c1
        x @?= (42 :: Int)
        y <- readChan c2
        y @?= (42 :: Int)
    ]

taskF :: TestTree
taskF =
  testGroup
    "Task F: InterpConcurrent"
    $ map (\(desc, e, val) -> concurrentEvalTest desc e val) expectingVal
      ++ map (\(desc, e) -> concurrentEvalTestFail desc e) expectingError

-- Parser tests related to the original language, just to check that
-- they haven't ruined the parser somehow.
oldParserTests :: TestTree
oldParserTests =
  testGroup
    "Old parser tests"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y"),
          parserTest "x==y" $ Eql (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y==y+x" $ Eql (Add (Var "x") (Var "y")) (Add (Var "y") (Var "x"))
        ],
      testGroup
        "Function application"
        [ parserTest "x y z" $
            Apply (Apply (Var "x") (Var "y")) (Var "z"),
          parserTest "x(y z)" $
            Apply (Var "x") (Apply (Var "y") (Var "z")),
          parserTestFail "x let v = 2 in v",
          parserTestFail "x \\x -> x",
          parserTestFail "x if x then y else z",
          parserTest "get x x" $
            Apply (KvGet (Var "x")) (Var "x"),
          parserTest "(get x) x" $
            Apply (KvGet (Var "x")) (Var "x")
        ],
      testGroup
        "Let-binding"
        [ parserTest "let x = y in z" $
            Let "x" (Var "y") (Var "z"),
          parserTest "let x = y+2 in z z" $
            Let "x" (Add (Var "y") (CstInt 2)) (Apply (Var "z") (Var "z")),
          parserTestFail "let true = y in z"
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
        "Anonymous functions"
        [ parserTest "\\x -> 2" $ Lambda "x" (CstInt 2),
          parserTest "\\x -> \\y -> x+y" $
            Lambda "x" $
              Lambda "y" $
                Add (Var "x") (Var "y"),
          parserTest "1 + \\x -> 2" $ Add (CstInt 1) (Lambda "x" (CstInt 2))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "iff x thenn y elsee z" $
            foldl Apply (Var "iff") [Var "x", Var "thenn", Var "y", Var "elsee", Var "z"],
          parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2,
          parserTest "getx" $ Var "getx",
          parserTest "putx" $ Var "putx",
          parserTestFail "2x"
        ],
      testGroup
        "put/get"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "get x" $ KvGet (Var "x"),
          parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y"),
          parserTest "y + get x" $ Add (Var "y") (KvGet (Var "x"))
        ]
    ]

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ taskA,
      taskB,
      taskC,
      taskD,
      taskE,
      taskF,
      oldParserTests
    ]
