{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Self-contained Haskell code from the slides for Week 5 on property-based testing.
-- Try the following:
--
--   sample (list1 (arbitrary :: Gen Integer))   -- a bad list generator
--   sample (list4 (arbitrary :: Gen Integer))   -- a good one
--   quickCheck prop_appendCommutative           -- fails, as it should
--   quickCheck prop_insertSorted                -- gives up: too many discards
--   quickCheck prop_insertSortedDiag            -- ... and shows what survived
--   quickCheck prop_insertSorted''              -- passes: xs drawn from genSorted
--   quickCheck prop_insertSortedCover           -- fails: insufficient coverage
--   quickCheck prop_insertSortedCover'          -- passes: designed generator
--   quickCheck prop_putGet                      -- the state laws
--   quickCheck prop_simBind                     -- IState simulates State
--   sample (arbitrary :: Gen (Program Int))     -- naive command sequences
--   sample (genProgram :: Gen (Program Int))    -- state-aware ones
--   quickCheck prop_array                       -- passes, having tested little
--   quickCheck prop_array'                      -- same, but diagnoses failures
--   quickCheck prop_arrayNaiveCoverage          -- fails: never resizes
--   quickCheck prop_arrayCoverage               -- passes

module Main where

import Control.Monad (forM_)
import Data.Array.IO (IOArray)
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sort)
-- Hide QuickCheck's Failure and Success 
import Test.QuickCheck hiding (Failure, Success)

-- Monads reloaded

class (Monad m) => StateMonad m s | m -> s where
  get :: m s
  put :: s -> m ()

-- Functional state monad

newtype State s a = State (s -> (a, s))

runState :: s -> State s a -> (a, s)
runState s (State f) = f s

instance Monad (State s) where
  c >>= f = State $ \s ->
    let (a, s') = runState s c
     in runState s' (f a)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  mf <*> ma = mf >>= \f -> fmap f ma

instance Functor (State s) where
  fmap f m = m >>= pure . f

instance StateMonad (State s) s where
  get = State $ \s -> (s, s)
  put s = State $ \_ -> ((), s)

-- Imperative state monad

newtype IState s a = IState {runIState :: IORef s -> IO a}

-- Create the reference from an initial state, then run.
runIStateFrom :: s -> IState s a -> IO a
runIStateFrom s c = do
  ref <- newIORef s
  runIState c ref

instance Monad (IState s) where
  c >>= f = IState $ \ref -> do
    a <- runIState c ref
    runIState (f a) ref

instance Applicative (IState s) where
  pure a = IState $ \_ -> pure a
  mf <*> ma = mf >>= \f -> fmap f ma

instance Functor (IState s) where
  fmap f m = m >>= pure . f

instance StateMonad (IState s) s where
  get = IState readIORef
  put s = IState $ \ref -> writeIORef ref s


-- QuickCheck properties: Examples without preconditions

-- Metamorphic property of length: forall xs, ys. length (xs ++ ys) = length (ys ++ xs)
-- Note: length satisfies it.
prop_lengthAppend :: [Integer] -> [Integer] -> Bool
prop_lengthAppend xs ys = length (xs ++ ys) == length xs + length ys

-- With '===' instead of '==' (gives more information during testing)
prop_lengthAppend' :: [Integer] -> [Integer] -> Property
prop_lengthAppend' xs ys = length (xs ++ ys) === length xs + length ys

-- Generator by manual enumeration
tediousTestCases :: [([Integer], [Integer])]
tediousTestCases = [([], []),  ([0], [1, 2]),  ([3, 4, 5], [])] -- etc.

-- Metamorphic property (of ++): forall xs, ys. xs ++ ys = ys ++ xs
-- Note: (++) does not satisfy it.
prop_appendCommutative :: [Integer] -> [Integer] -> Bool
prop_appendCommutative xs ys = xs ++ ys == ys ++ xs

-- Round-trip property: forall xs. read (show xs) === xs
prop_showRead :: [Integer] -> Property
prop_showRead xs = read (show xs) === xs


msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where
    (ys, zs) = splitAt (length xs `div` 2) xs
    merge [] bs = bs
    merge as [] = as
    merge (a : as) (b : bs)
      | a <= b = a : merge as (b : bs)
      | otherwise = b : merge (a : as) bs

-- Reference implementation property: forall xs. msort xs  = sort xs
prop_msort :: [Integer] -> Property
prop_msort xs = msort xs === sort xs

-- Metamorphic property: forall xs, ys. msort (xs ++ ys) = msort (ys +++ xs)
prop_msortAppend :: [Integer] -> [Integer] -> Property
prop_msortAppend xs ys = msort (xs ++ ys) === msort (ys ++ xs)

-- Direct/oracle property: forall xs. sorted (msort xs)
prop_msortSorted :: [Integer] -> Bool
prop_msortSorted xs = sorted (msort xs)

-- Functional QuickSort
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

-- Reference implementation property: forall xs. qsort xs = msort xs
-- Use msort as reference implementation
prop_qsortMsort :: [Integer] -> Property
prop_qsortMsort xs = qsort xs === msort xs

-- QuickCheck properties: Examples with preconditions

-- Algebraic (equational) property: for all x, y.  y /= 0 ==> (x `div` y) * y + (x `mod` y) === x
prop_divMod :: Integer -> Integer -> Property
prop_divMod x y = y /= 0 ==> (x `div` y) * y + (x `mod` y) === x

-- With precondition built into type Nonzero Integer ~ {y :: Int | y /= 0} (refinement type)
prop_divMod' :: Integer -> NonZero Integer -> Property
prop_divMod' x (NonZero y) = (x `div` y) * y + (x `mod` y) === x


sorted :: (Ord a) => [a] -> Bool
sorted xs = and $ zipWith (<=) xs (drop 1 xs)

insertSorted :: (Ord a) => a -> [a] -> [a]
insertSorted x [] = [x]
insertSorted x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insertSorted x ys

-- Conditional property: forall x, xs. sorted xs ==> sorted (insertSorted x xs)
prop_insertSorted :: Integer -> [Integer] -> Property
prop_insertSorted x xs =
  sorted xs ==> sorted (insertSorted x xs)

-- Instrumented to report statistics on  the valid (non-discarded) test data
prop_insertSortedDiag :: Integer -> [Integer] -> Property
prop_insertSortedDiag x xs =
  sorted xs
    ==> classify (length xs < 2) "trivial"
    $ tabulate "length of xs" [lengthBucket xs]
    $ sorted (insertSorted x xs)

-- Generators.

-- Simple recursive generator combinator for lists, [] and (:) equally probable
list1 :: Gen a -> Gen [a]
list1 g = oneof [pure [], (:) <$> g <*> list1 g]


-- Simple recursive generator combinator for lists with [] 10%, (:) 90% probability
list2 :: Gen a -> Gen [a]
list2 g = frequency [(1, pure []), (9, (:) <$> g <*> list2 g)]

-- List generator: Sample length first, then generate list of that length 
list3 :: Gen a -> Gen [a]
list3 g = abs <$> (arbitrary :: Gen Int) >>= go
  where
    go 0 = pure []
    go n = (:) <$> g <*> go (n - 1)

-- List generator: Sample length first, then generate list of that length or less
list4 :: Gen a -> Gen [a]
list4 g = sized $ \n -> chooseInt (0, n) >>= go
  where
    go 0 = pure []
    go n = (:) <$> g <*> go (n - 1)


-- Test data design: partitioning and coverage.

-- Classification of a list by its length
lengthBucket :: [a] -> String
lengthBucket xs
  | n == 0 = "0"
  | n < 5 = "1-4"
  | n < 20 = "5-19"
  | otherwise = ">=20"
  where
    n = length xs

-- Instrumented property: Histogram of valid xs according to their classification
prop_insertSorted' :: Integer -> [Integer] -> Property
prop_insertSorted' x xs =
  sorted xs ==>
    classify (length xs < 2) "trivial" $
      tabulate "length of xs" [lengthBucket xs] $
        sorted (insertSorted x xs)

-- Custom list generator: Generate only sorted lists (by sorting generated lists)
genSorted :: Gen [Integer]
genSorted = sort <$> arbitrary


-- Instrumented property: Histogram of sorted xs according to their classification
prop_insertSorted'' :: Integer -> Property
prop_insertSorted'' x =
  forAll genSorted $ \xs ->
    tabulate "length of xs" [lengthBucket xs] $
      sorted (insertSorted x xs)

-- Instrumented property: Record and check coverage of valid data by the blocks of data they belong to
prop_insertSortedCover :: Integer -> Property
prop_insertSortedCover x =
  forAll genSorted $ \xs ->
    checkCoverage
      . cover 2 (null xs) "empty list"
      . cover 20 (not (null xs) && x < head xs) "insert at front"
      . cover 20 (not (null xs) && x > last xs) "insert at back"
      . cover 20 (x `elem` xs) "insert duplicate"
      $ sorted (insertSorted x xs)

-- Weighted generator of elements given a (eventually sorted) list, small element, large element, element in list
boundaries :: [Integer] -> [(Int, Gen Integer)]
boundaries xs =
  [ (3, pure $ head xs - 1),
    (3, pure $ last xs + 1),
    (3, elements xs)
  ]

-- Generator of value/(sorted) list pairs covering pairs of small, middle, large values relative to list equally
genInsertion :: Gen (Integer, [Integer])
genInsertion = do
  xs <- genSorted
  x <-
    frequency $
      (2, arbitrary) : if null xs then [] else boundaries xs
  pure (x, xs)

-- Generator with balanced coverage of blocks empty list, small/existing/large value blocks,, with verified coverage
prop_insertSortedCover' :: Property
prop_insertSortedCover' =
  forAll genInsertion $ \(x, xs) ->
    checkCoverage
      . cover 2 (null xs) "empty list"
      . cover 20 (not (null xs) && x < head xs) "insert at front"
      . cover 20 (not (null xs) && x > last xs) "insert at back"
      . cover 20 (x `elem` xs) "insert duplicate"
      $ sorted (insertSorted x xs)


-- Shrinking.

data Pair a b = Pair a b
  deriving (Show)

-- shrink pairs by shrinking each component individually
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

  shrink (Pair x y) = [Pair x' y | x' <- shrink x] ++ [Pair x y' | y' <- shrink y]


-- Commutative append property. Small counterexample by shrinking
prop_appendCommutative' :: Pair [Integer] [Integer] -> Bool
prop_appendCommutative' (Pair xs ys) = xs ++ ys == ys ++ xs


-- Testing an abstract data type against a reference implementation.

runIStateFull :: s -> IState s a -> IO (a, s)
runIStateFull s c = do
  ref <- newIORef s
  a <- runIState c ref
  s' <- readIORef ref
  pure (a, s')


infix 4 ~=

-- Observational equality of functional state monad implementation
-- c1 ~= c2 =  forall s, s'. runState s c1 === runState s c2
(~=) :: (Eq a, Show a) => State Int a -> State Int a -> Property
c1 ~= c2 = property $ \s -> runState s c1 === runState s c2

-- Put-get property: forall s. (put s >> get) ~= (put s >> pure s)
prop_putGet :: Int -> Property
prop_putGet s = (put s >> get) ~= (put s >> pure s)

-- Put-put property: forall s, s'. (put s >> put s') ~= put s'
prop_putPut :: Int -> Int -> Property
prop_putPut s s' = (put s >> put s') ~= put s'

-- Get-put property: forall s. (get >>= put) ~= pure ()
prop_getPut :: Property
prop_getPut = (get >>= put) ~= pure ()

-- Simulation of functional and imperative state monad implementations
-- c `simulates` r = forall s. runIStateFull s c ~= pure (runState s r)
simulates :: (Eq a, Show a) => IState Int a -> State Int a -> Property
c `simulates` r = property $ \s ->
  ioProperty $ do
    x <- runIStateFull s c
    pure (x === runState s r)

-- Simulation property: forall types t. get :: IState t t `simulates State t t
prop_simGet :: Property
prop_simGet = get `simulates` get

-- Simulation property: forall s :: Int. put s :: IState Int  () `simulates` State Int ()
prop_simPut :: Int -> Property
prop_simPut s = put s `simulates` put s

-- Simulation property: forall types t. forall x :: Int. pure x :: IState t Int `simulates` State t Int
prop_simPure :: Int -> Property
prop_simPure x = pure x `simulates` pure x

-- Simulation property:
-- forall f :: Int -> Int. get >>= put . f :: IState Int () `simulates` get >>= put . f :: State Int ()
prop_simBind :: Fun Int Int -> Property
prop_simBind fun =
  (get >>= put . f) `simulates` (get >>= put . f)
  where
    f = applyFun fun

-- Model-based testing of a stateful system: a dynamic array.

data DynamicArray a
  = DynamicArray
  { -- Number of elements inserted.
    daUsed :: IORef Int,
    -- Capacity.
    daCap :: IORef Int,
    -- Underlying array.
    daArr :: IORef (IOArray Int a)
  }

newDynamicArray :: IO (DynamicArray a)
newDynamicArray = do
  let capacity = 10
  arr <- newArray_ (0, capacity - 1)
  used_ref <- newIORef 0
  capacity_ref <- newIORef capacity
  arr_ref <- newIORef arr
  pure $
    DynamicArray
      { daUsed = used_ref,
        daCap = capacity_ref,
        daArr = arr_ref
      }
-- get element at index
index :: Int -> DynamicArray a -> IO (Maybe a)
index i (DynamicArray used_ref _cap_ref arr_ref) = do
  used <- readIORef used_ref
  if i >= 0 && i < used
    then do
      arr <- readIORef arr_ref
      Just <$> readArray arr i
    else pure Nothing

-- append element to right end of array
insert :: a -> DynamicArray a -> IO ()
insert x (DynamicArray used_ref cap_ref arr_ref) = do
  used <- readIORef used_ref
  cap <- readIORef cap_ref
  arr <- readIORef arr_ref
  if used < cap
    then do 
      writeArray arr used x
      writeIORef used_ref $ used + 1
    else do -- allocate new array of double length and copy old array into it
      let cap' = cap * 2
      arr' <- newArray_ (0, cap')
      forM_ [0 .. used - 1] $ \i ->
        writeArray arr' i =<< readArray arr i
      writeArray arr' used x
      writeIORef arr_ref arr'
      writeIORef used_ref $ used + 1
      writeIORef cap_ref cap'

-- store element at index
write :: Int -> a -> DynamicArray a -> IO (Maybe ())
write i x (DynamicArray used_ref _cap_ref arr_ref) = do
  used <- readIORef used_ref
  if i >= 0 && i < used
    then do
      arr <- readIORef arr_ref
      writeArray arr i x
      pure $ Just ()
    else pure Nothing

-- delete element at index (shift its right elements left)
delete :: Int -> DynamicArray a -> IO (Maybe ())
delete i (DynamicArray used_ref cap_ref arr_ref) = do
  used <- readIORef used_ref
  if i >= 0 && i < used
    then do
      cap <- readIORef cap_ref
      arr <- readIORef arr_ref
      forM_ [i + 1 .. used - 1] $ \j ->
        writeArray arr (j - 1) =<< readArray arr j
      writeIORef used_ref $ used - 1
      if used < cap `div` 2
        then do
          let cap' = cap `div` 2
          arr' <- newArray_ (0, cap')
          forM_ [0 .. used - 1] $ \j ->
            writeArray arr' j =<< readArray arr j
          writeIORef used_ref $ used - 1
          writeIORef arr_ref arr'
          writeIORef cap_ref cap'
        else pure ()
      pure $ Just ()
    else pure Nothing

-- Model of dynamic array is sequence of elements
data Model a = Model [a]
  deriving (Show)

initModel :: Model a
initModel = Model []

-- Commands
data Command a
  = Insert a
  | Index Int
  | Write Int a
  | Delete Int
  deriving (Eq, Show)

-- Possible responses from model operations 
data Response a = Success | Failure | Elem a
  deriving (Eq, Show)

-- Model operations are purely functional updates on sequences

-- Insert command at right end 
-- Caution: n inserts take O(n^2) time
cmdInsert :: a -> Model a -> (Model a, Response a)
cmdInsert x (Model xs) = (Model $ xs ++ [x], Success)

-- Retrieve i-th command
cmdIndex :: Int -> Model a -> (Model a, Response a)
cmdIndex i (Model xs) =
  if i >= 0 && i < length xs
    then (Model xs, Elem (xs !! i))
    else (Model xs, Failure)

-- insert command after the i-the command
cmdWrite :: Int -> a -> Model a -> (Model a, Response a)
cmdWrite i x (Model xs) =
  if i >= 0 && i < length xs
    then (Model $ take i xs ++ [x] ++ drop (i + 1) xs, Success)
    else (Model xs, Failure)

-- delete i-th command
cmdDelete :: Int -> Model a -> (Model a, Response a)
cmdDelete i (Model xs) =
  if i >= 0 && i < length xs
    then (Model $ take i xs ++ drop (i + 1) xs, Success)
    else (Model xs, Failure)

-- apply command to reference implementation/executable specification (model)
step :: Model a -> Command a -> (Model a, Response a)
step m (Insert x) = cmdInsert x m
step m (Index i) = cmdIndex i m
step m (Write i x) = cmdWrite i x m
step m (Delete i) = cmdDelete i m

-- apply command to implementation (SUT)
exec :: DynamicArray a -> Command a -> IO (Response a)
exec a (Insert x) = do
  insert x a
  pure Success
exec a (Index i) = do
  r <- index i a
  case r of
    Just x -> pure $ Elem x
    Nothing -> pure Failure
exec a (Write i x) = do
  r <- write i x a
  case r of
    Just () -> pure Success
    Nothing -> pure Failure
exec a (Delete i) = do
  r <- delete i a
  case r of
    Just () -> pure Success
    Nothing -> pure Failure

-- Program is list of commands
newtype Program a = Program [Command a]
  deriving (Show)

-- Default generator for commands: each operation equally probable
instance (Arbitrary a) => Arbitrary (Command a) where
  arbitrary =
    oneof
      [ Insert <$> arbitrary,
        Index <$> arbitrary,
        Write <$> arbitrary <*> arbitrary,
        Delete <$> arbitrary
      ]

-- Default generator for programs via standard list generator
instance (Arbitrary a) => Arbitrary (Program a) where
  arbitrary = Program <$> listOf arbitrary
  shrink (Program l) = map Program (shrink l)

-- Print the first generated programs
samplePrograms :: IO ()
samplePrograms = sample (arbitrary :: Gen (Program Int))

-- Execute program on both model and SUT 
runProgram :: (Eq a) => DynamicArray a -> Model a -> Program a -> IO Bool
runProgram c0 m0 (Program cmds0) = go c0 m0 cmds0
  where
    go _c _m [] = pure True
    go c m (cmd : cmds) = do
      sut_resp <- exec c cmd
      let (m', model_resp) = step m cmd
      if sut_resp == model_resp
        then go c m' cmds
        else pure False

-- Simulation property: forall prog. evalModel prog initModel ~= evalDynArray prog initArray
-- evalModel, evalDynArray not shown; equivalent simulation property by step-by-step simulation used here
prop_array :: Program Int -> Property
prop_array prog = ioProperty $ do
  c <- newDynamicArray
  runProgram c initModel prog

-- Test the property (with built-in generator) using QuickCheck
test_prop_array :: IO ()
test_prop_array = quickCheck prop_array

-- Instrumented properties: Explicit annotation of failures using 'counterexample'
checkProgram ::
  (Eq a, Show a) =>
  DynamicArray a ->
  Model a ->
  Program a ->
  IO Property
checkProgram c0 m0 (Program cmds0) = go m0 cmds0
  where
    go _m [] = pure $ property True
    go m (cmd : cmds) = do
      sut_resp <- exec c0 cmd
      let (m', model_resp) = step m cmd
      if sut_resp == model_resp
        then go m' cmds
        else
          pure $
            counterexample
              ( unlines
                  [ "Diverged on command: " ++ show cmd,
                    "In model state:      " ++ show m,
                    "SUT response:        " ++ show sut_resp,
                    "Model response:      " ++ show model_resp
                  ]
              )
              False

-- Simulation property, with counterexample-instrumented generator
prop_array' :: Program Int -> Property
prop_array' prog = ioProperty $ do
  c <- newDynamicArray
  checkProgram c initModel prog


-- Custom generators for programs (lists of commands)

-- Model-state dependent command generator: separate commands int valid index commands and general commands,
-- for general commends bias toward Insert for small model states
genCommand :: (Arbitrary a) => Model a -> Gen (Command a)
genCommand (Model xs) = frequency $ unconstrained ++ inBounds
  where
    unconstrained =
      [ (insertWeight, Insert <$> arbitrary),
        (1, Index <$> arbitrary),
        (1, Write <$> arbitrary <*> arbitrary),
        (1, Delete <$> arbitrary)
      ]
    inBounds
      | null xs = []
      | otherwise =
          [ (3, Index <$> validIndex),
            (3, Write <$> validIndex <*> arbitrary),
            (3, Delete <$> validIndex)
          ]
    validIndex = chooseInt (0, length xs - 1)
    insertWeight = max 2 (30 - 2 * length xs)

-- Program generator using model-state dependent command generator
genProgram :: (Arbitrary a) => Gen (Program a)
genProgram = sized $ \n -> do
  k <- chooseInt (0, n)
  Program <$> go initModel k
  where
    go _ 0 = pure []
    go m k = do
      cmd <- genCommand m
      let (m', _) = step m cmd
      (cmd :) <$> go m' (k - 1)

-- Simulation property, with model-state dependent program generator and available model shrinker
prop_array'' :: Property
prop_array'' =
  forAllShrink (genProgram :: Gen (Program Int)) shrink $ \prog ->
    ioProperty $ do
      c <- newDynamicArray
      checkProgram c initModel prog


-- Valid programs (no out-of-bound commands) and shrinking

precondition :: Model a -> Command a -> Bool
precondition (Model xs) cmd = case cmd of
  Insert _ -> True
  Index i -> ok i
  Write i _ -> ok i
  Delete i -> ok i
  where
    ok i = i >= 0 && i < length xs

validProgram :: Program a -> Bool
validProgram (Program cmds0) = go initModel cmds0
  where
    go _ [] = True
    go m (cmd : cmds) =
      precondition m cmd && go (fst (step m cmd)) cmds

-- Shrink valid programs to valid programs only
shrinkProgram :: Program a -> [Program a]
shrinkProgram (Program cmds) =
  filter validProgram $ map Program $ shrinkList (const []) cmds


-- Partitioning the state space

-- Trace application of program to initial (model) state
modelTrace :: Program a -> [(Model a, Command a, Response a)]
modelTrace (Program cmds0) = go initModel cmds0
  where
    go _ [] = []
    go m (cmd : cmds) =
      let (m', resp) = step m cmd
       in (m, cmd, resp) : go m' cmds

-- Size of largest model state encountered during execution
maxElems :: Program a -> Int
maxElems prog = maximum $ 0 : [length xs | (Model xs, _, _) <- modelTrace prog]

-- Does program contain a Delete command applied to a 'large' model state (from initial state)?
deletedWhenLarge :: Program a -> Bool
deletedWhenLarge prog =
  or [length xs > 10 | (Model xs, Delete _, _) <- modelTrace prog]

-- Does program execution contain an out-of-bounds operation?
anyRejected :: (Eq a) => Program a -> Bool
anyRejected prog = or [resp == Failure | (_, _, resp) <- modelTrace prog]

-- Instrument property of program execution to classify test programs
-- into those that grow past initial capacity, delete after enlarging array, have out-of-bounds commands
programCoverage :: Program Int -> Property -> Property
programCoverage prog =
  checkCoverage
    . tabulate "maximum number of elements" [bucket (maxElems prog)]
    . cover 30 (maxElems prog > 10) "grows past capacity"
    . cover 10 (deletedWhenLarge prog) "deletes while large"
    . cover 30 (anyRejected prog) "command out of bounds"
  where
    bucket n
      | n == 0 = "0"
      | n <= 10 = "1-10"
      | n <= 20 = "11-20"
      | otherwise = ">20"

-- Simulation property without balanced coverage check
prop_arrayNaiveCoverage :: Program Int -> Property
prop_arrayNaiveCoverage prog =
  programCoverage prog $
    ioProperty $ do
      c <- newDynamicArray
      checkProgram c initModel prog

-- Simulation property with balanced coverage check
prop_arrayCoverage :: Property
prop_arrayCoverage =
  forAllShrink (genProgram :: Gen (Program Int)) shrink $ \prog ->
    programCoverage prog $
      ioProperty $ do
        c <- newDynamicArray
        checkProgram c initModel prog


-- Demo driver for executing above code

main :: IO ()
main = demo

demo :: IO ()
demo = do
  section "Generators: the four attempts"
  mapM_ (\(n, g) -> putStrLn ("-- " ++ n) >> sample g)
    [ ("list1", list1 g'), ("list2", list2 g'), ("list3", list3 g'), ("list4", list4 g') ]

  section "Properties of pure functions"
  quickCheck prop_lengthAppend
  putStrLn "-- prop_appendCommutative is FALSE and should fail:"
  quickCheck prop_appendCommutative
  quickCheck prop_msort
  quickCheck prop_qsortMsort
  quickCheck prop_msortAppend

  section "The state laws, and IState simulating State"
  quickCheck prop_putGet
  quickCheck prop_putPut
  quickCheck prop_getPut
  quickCheck prop_simGet
  quickCheck prop_simPut
  quickCheck prop_simPure
  quickCheck prop_simBind

  section "Test data design: the four steps of the sorted-insertion example"
  putStrLn "-- 1. a rarely satisfied precondition: QuickCheck gives up"
  quickCheck prop_insertSorted'
  putStrLn "-- 2. generate values that satisfy it instead"
  quickCheck prop_insertSorted''
  putStrLn "-- 3. ask for coverage: the obvious generator is not good enough"
  quickCheck prop_insertSortedCover
  putStrLn "-- 4. let the test design drive the generator"
  quickCheck prop_insertSortedCover'

  section "Shrinking"
  putStrLn "-- unshrunk-looking counterexamples until Pair has a shrink function:"
  quickCheck prop_appendCommutative'

  section "Model-based testing of the dynamic array"
  putStrLn "-- naive command sequences:"
  sample (arbitrary :: Gen (Program Int))
  putStrLn "-- state-aware command sequences:"
  sample (genProgram :: Gen (Program Int))
  putStrLn "-- the naive generator hardly ever resizes the array:"
  quickCheck prop_arrayNaiveCoverage
  putStrLn "-- the state-aware generator does:"
  quickCheck prop_arrayCoverage
  where
    g' = arbitrary :: Gen Integer
    section t = putStrLn ("\n===== " ++ t ++ " =====")
