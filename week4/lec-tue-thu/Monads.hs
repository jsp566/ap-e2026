{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- Advanced Programming 2026, week 4: More monads.
-- Source code accompanying lectures/monads/monads.tex.
--
-- Load with:  ghci Monads.hs

module Monads where

import Control.Monad (join, (>=>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

--------------------------------------------------------------------------
-- The free monad over a functor
--------------------------------------------------------------------------

data Free e a
  = Pure a
  | Free (e (Free e a))

-- Only (>>=) says anything about Free; fmap and (<*>) are the standard
-- definitions using (>>=), as for every other monad in this file.
instance (Functor e) => Functor (Free e) where
  fmap f m = m >>= pure . f

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  mf <*> ma = mf >>= \f -> ma >>= pure . f

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ fmap (>>= f) g

--------------------------------------------------------------------------
-- Method 1: parameterise over the monad
--------------------------------------------------------------------------

class (Monad m) => StateMonad m s | m -> s where
  get :: m s
  put :: s -> m ()

modify :: (StateMonad m s) => (s -> s) -> m ()
modify f = get >>= put . f

-- Programs written against the interface. 

tick :: (StateMonad m Int) => m Int
tick = do { n <- get; put (n + 1); pure n }

push :: (StateMonad m [a]) => a -> m ()
push x = modify (x :)

pop :: (StateMonad m [a]) => m (Maybe a)
pop = do
  xs <- get
  case xs of
    []       -> pure Nothing
    (y : ys) -> do { put ys; pure $ Just y }

stackExample :: (StateMonad m [Int]) => m (Maybe Int)
stackExample = do
  push 3
  push 5
  a <- pop
  b <- pop
  pure $ addMaybe a b
  where
    addMaybe mx my = do
      x <- mx
      y <- my
      pure $ x + y

--------------------------------------------------------------------------
-- A purely functional state implementation
--------------------------------------------------------------------------

newtype State s a = State (s -> (a, s))

runState :: s -> State s a -> (a, s)
runState s (State f) = f s

instance Monad (State s) where
  c >>= f = State $ \s -> let (a, s') = runState s c
                          in runState s' (f a)

-- Haskell requires Functor and Applicative to be DECLARED, but for a monad
-- they need not be INVENTED: the two definitions below use nothing about
-- State, and are repeated verbatim for every monad in this file.  pure is
-- the one operation (>>=) cannot supply.
instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  mf <*> ma = mf >>= \f -> fmap f ma

instance Functor (State s) where
  fmap f m = m >>= pure . f

-- The slides show get and put here monomorphically, before the StateMonad
-- class exists:
--        get :: State s s;        get   = State $ \s -> (s, s)
--        put :: s -> State s ();  put s = State $ \_ -> ((), s)
-- Same definitions; here they are the class methods.
instance StateMonad (State s) s where
  get = State $ \s -> (s, s)
  put s = State $ \_ -> ((), s)

--------------------------------------------------------------------------
-- An imperative state implementation
--------------------------------------------------------------------------

-- Like State, a newtype whose selector is the true inverse of the
-- constructor: the caller supplies the reference.
newtype IState s a = IState {runIState :: IORef s -> IO a}

instance Monad (IState s) where
  c >>= f = IState $ \ref -> do
    a <- runIState c ref
    runIState (f a) ref

-- (>>=) hands the SAME reference to both computations: the state is
-- overwritten in place, not threaded.  fmap and (<*>) are derived from
-- (>>=); pure must be given directly -- it is the one operation (>>=)
-- cannot supply.
instance Functor (IState s) where
  fmap f m = m >>= pure . f

instance Applicative (IState s) where
  pure a = IState $ \_ -> pure a
  mf <*> ma = mf >>= \f -> fmap f ma

instance StateMonad (IState s) s where
  get = IState readIORef
  put s = IState $ \ref -> writeIORef ref s

-- Convenience: create the reference from an initial state, then run.  At the
-- GHCi prompt the slides write this out as  newIORef 0 >>= runIState tickI.
runIStateFrom :: s -> IState s a -> IO a
runIStateFrom s c = do
  ref <- newIORef s
  runIState c ref

--------------------------------------------------------------------------
-- A counting monad 
--------------------------------------------------------------------------

-- The counting monad is a monad and has get and put operations, but fails
-- the get-put, put-get and put/put state monad laws
newtype Counting a = Counting {runCounting :: Int -> (a, Int)}

instance Monad Counting where
  Counting g >>= f =
    Counting $ \n -> let (a, n') = g n in runCounting (f a) n'

instance Functor Counting where
  fmap f m = m >>= pure . f

instance Applicative Counting where
  pure a = Counting $ \n -> (a, n)
  mf <*> ma = mf >>= \f -> fmap f ma

instance StateMonad Counting Int where
  get = Counting $ \n -> (0, n)
  put _ = Counting $ \n -> ((), n + 1)

-- The forall sits INSIDE the constructor (RankNTypes), so the one value p
-- can be used at two instances in the same expression.  As a plain function
-- argument it would be instantiated once, at the call site.

newtype Prog = Prog (forall m. (StateMonad m Int) => m Int)

threeTicks :: Prog
threeTicks = Prog (do { _ <- tick; _ <- tick; tick })

runIt :: Prog -> ((Int, Int), (Int, Int))
runIt (Prog p) = (runState 0 p, runCounting p 0)

--------------------------------------------------------------------------
-- The free state monad, directly
--------------------------------------------------------------------------

-- Step 1.  Give each operation its continuation, in the monad M we start
-- from.  putk s = (put s >>=) and getk = (get >>=), so that
--
--        put s = putk s pure      put s >>= m = putk s m
--        get   = getk pure        get   >>= m = getk m
--
-- The left column is the right unit law; no property of M beyond the monad
-- laws is used.  Every (>>=) now has a primitive operation on its left,
-- absorbed by the continuation-passing operation.

putk :: (StateMonad m s) => s -> (() -> m a) -> m a
putk s = (put s >>=)

getk :: (StateMonad m s) => (s -> m a) -> m a
getk = (get >>=)

-- Step 2.  Take the continuation-passing operations as UNINTERPRETED
-- constructors.  () -> M is iso to M a, so Putk drops the ().
-- M a is now just the type being built.  Return is forced: Getk and Putk
-- only extend a computation, neither can start one.

data FSM s a
  = Return a
  | Getk (s -> FSM s a)
  | Putk s (FSM s a)

-- Step 3.  (>>=) is now DERIVED: each operation already carries its
-- continuation, so (>>=) only reaches the Return leaves and grafts f on.
-- fmap and (<*>) are defined FROM (>>=), not independently of it.

instance Monad (FSM s) where
  Return x >>= f = f x
  Getk k >>= f = Getk $ k >=> f
  Putk s m >>= f = Putk s $ m >>= f

instance Functor (FSM s) where
  fmap f m = m >>= pure . f

instance Applicative (FSM s) where
  -- Haskell requires pure's definition to live in the Applicative instance.
  pure = Return
  mf <*> ma = mf >>= \f -> fmap f ma

instance StateMonad (FSM s) s where
  get = Getk Return
  put s = Putk s $ Return ()

runFSM :: s -> FSM s a -> (a, s)
runFSM s (Return x) = (x, s)
runFSM s (Getk k) = runFSM s (k s)
runFSM _ (Putk s m) = runFSM s m

tickFSM :: FSM Int Int
tickFSM = tick

--------------------------------------------------------------------------
-- Free state monad via the general free monad construction
--------------------------------------------------------------------------

data StateOp s r
  = StateGet (s -> r)
  | StatePut s r

-- Step 5.  FSM's bind applied (>>= f) to every subcomputation inside an
-- operation, leaving the operation's own data alone.  In the abstracted
-- layer that operation IS fmap, so the Functor constraint on Free is not a
-- side condition: it is FSM's bind, factored out and named.
instance Functor (StateOp s) where
  fmap h (StateGet k)   = StateGet $ h . k
  fmap h (StatePut s a) = StatePut s $ h a

type FreeState s a = Free (StateOp s) a

instance StateMonad (Free (StateOp s)) s where
  get = Free $ StateGet Pure
  put s = Free $ StatePut s $ Pure ()

-- FSM and FreeState are the same type, constructor by constructor.
toFree :: FSM s a -> FreeState s a
toFree (Return x) = Pure x
toFree (Getk k) = Free $ StateGet $ toFree . k
toFree (Putk s m) = Free $ StatePut s $ toFree m

fromFree :: FreeState s a -> FSM s a
fromFree (Pure x) = Return x
fromFree (Free (StateGet k)) = Getk $ fromFree . k
fromFree (Free (StatePut s m)) = Putk s $ fromFree m

-- An analysis of a free computation: an ordinary function on data, needing no
-- instance.  Nothing to run, and nothing that could run.  Returns Nothing
-- when the computation branches on a state we would have to invent.
countPuts :: FreeState s a -> Maybe Int
countPuts (Pure _) = Just 0
countPuts (Free (StatePut _ m)) = fmap (1 +) $ countPuts m
countPuts (Free (StateGet _)) = Nothing

--------------------------------------------------------------------------
-- Interpretation
--------------------------------------------------------------------------

-- An interpretation of the effects is a function e x -> m x, uniform in x:
-- it says what ONE operation means, and says nothing about sequencin them.
-- 'interpret' extends it to whole computations, and it is the only such
-- extension that is a monad morphism.  That is what "free" means.

interpret :: (Monad m, Functor e)
          => (forall x. e x -> m x)
          -> Free e a -> m a
interpret _ (Pure x) = pure x
interpret h (Free g) = join $ fmap (interpret h) $ h g

stateOps :: (StateMonad m s) => StateOp s x -> m x
stateOps (StateGet k) = fmap k get
stateOps (StatePut s x) = put s >> pure x

runFreeState :: (StateMonad m s) => FreeState s a -> m a
runFreeState = interpret stateOps

-- One computation, three iinterpretations

runFreeStateF :: FreeState s a -> s -> (a, s)
runFreeStateF m s = runState s (runFreeState m)

runFreeStateI :: FreeState s a -> IORef s -> IO a
runFreeStateI m = runIState (runFreeState m)

-- Or interpret directly, fusing away the intermediate monad.  This is what
-- you would hand-optimise 'runFreeStateF' into.  Up to the order of the
-- arguments it is the same function.

runFreeStateDirect :: s -> FreeState s a -> (a, s)
runFreeStateDirect s (Pure x) = (x, s)
runFreeStateDirect s (Free (StateGet k)) = runFreeStateDirect s (k s)
runFreeStateDirect _ (Free (StatePut s' m)) = runFreeStateDirect s' m

-- The same source text, at three different types.

tickF :: State Int Int
tickF = tick

tickI :: IState Int Int
tickI = tick

tickFree :: FreeState Int Int
tickFree = tick

stackExampleF :: State [Int] (Maybe Int)
stackExampleF = stackExample

stackExampleI :: IState [Int] (Maybe Int)
stackExampleI = stackExample

stackExampleFree :: FreeState [Int] (Maybe Int)
stackExampleFree = stackExample

--------------------------------------------------------------------------
-- Effects whose arguments are themselves computations
--------------------------------------------------------------------------

data ErrorOp e a
  = ErrorThrow e
  | forall x. ErrorCatch (ErrorM e x) (e -> ErrorM e x) (x -> a)

instance Functor (ErrorOp e) where
  fmap _ (ErrorThrow e) = ErrorThrow e
  fmap f (ErrorCatch m h c) = ErrorCatch m h $ f . c

type ErrorM e a = Free (ErrorOp e) a

throw :: e -> ErrorM e a
throw e = Free $ ErrorThrow e

catch :: ErrorM e a -> (e -> ErrorM e a) -> ErrorM e a
catch m h = Free $ ErrorCatch m h Pure

runError :: ErrorM e a -> Either e a
runError (Pure x) = Right x
runError (Free (ErrorThrow e)) = Left e
runError (Free (ErrorCatch m h c)) =
  case runError m of
    Right x -> runError $ c x
    Left err -> runError $ h err >>= c

-- Why not put the branches at the parameter, 'ErrorCatch a (e -> a)'?
-- Because then fmap -- and hence (>>=) -- would rewrite the scrutinee too:
--
--        (m `catch` h) >>= k  =  (m >>= k) `catch` (\e -> h e >>= k)
--
-- so k would run INSIDE the handler.  

--------------------------------------------------------------------------
-- Effects that are not a known monad in disguise: logging + memoisation
--------------------------------------------------------------------------

-- The slides introduce FibOp, FibM, fib and the three interpreters with
-- logging only, and then EXTEND FibOp with FibMemo.  A module can hold only
-- the extended version, so that is what follows; the logging-only version is
-- this text with the FibMemo constructor, its fmap clause, fibMemo and the
-- FibMemo clauses of the interpreters removed.
data FibOp a = FibLog String a
             | FibMemo Int (FibM Int) (Int -> a)
type FibM a = Free FibOp a

instance Functor FibOp where
  fmap f (FibLog s c)    = FibLog s $ f c
  fmap f (FibMemo n m c) = FibMemo n m $ f . c

fibMemo :: Int -> FibM Int -> FibM Int
fibMemo n m = Free $ FibMemo n m Pure

fibLog :: String -> FibM ()
fibLog s = Free $ FibLog s $ Pure ()

fib :: Int -> FibM Int
fib 0 = pure 1
fib 1 = pure 1
fib n = fibMemo n $ do
  fibLog ("fib(" ++ show n ++ ")")
  x <- fib (n - 1)
  y <- fib (n - 2)
  pure $ x + y

-- Four interpretations of the very same 'fib'.

-- (a) Ignore the logging, do not memoise.
pureFibM :: FibM a -> a
pureFibM (Pure x) = x
pureFibM (Free (FibLog _ c)) = pureFibM c
pureFibM (Free (FibMemo _ fn c)) = pureFibM $ c $ pureFibM fn

-- (b) Print the log, do not memoise.
ioFibM :: FibM a -> IO a
ioFibM (Pure x) = pure x
ioFibM (Free (FibLog s c)) = do
  putStrLn s
  ioFibM c
ioFibM (Free (FibMemo _ fn c)) = do
  x <- ioFibM fn
  ioFibM (c x)

-- (c) Collect the log, purely.  Note the order agrees with (b).
logFibM :: FibM a -> (a, [String])
logFibM (Pure x) = (x, [])
logFibM (Free (FibLog s c)) =
  let (x, msgs) = logFibM c
   in (x, s : msgs)
logFibM (Free (FibMemo _ fn c)) =
  let (x, msgs) = logFibM fn
      (y, msgs') = logFibM (c x)
   in (y, msgs ++ msgs')

-- (d) Actually memoise.  Nothing in 'fib' changed.
memoFibM :: FibM a -> a
memoFibM m = fst $ run [] m
  where
    run :: [(Int, Int)] -> FibM b -> (b, [(Int, Int)])
    run cache (Pure x)              = (x, cache)
    run cache (Free (FibLog _ c))   = run cache c
    run cache (Free (FibMemo n fn c)) =
      case lookup n cache of
        Just x  -> run cache (c x)
        Nothing -> let (x, cache') = run cache fn
                   in  run ((n, x) : cache') (c x)

--------------------------------------------------------------------------
-- Rewriting an effect stack before interpreting it
--------------------------------------------------------------------------

-- A free-monad computation is data, so we can transform it.  'modifyEffects'
-- rewrites every operation node, leaving the sequencing alone.  

modifyEffects ::
  (Functor e, Functor h) =>
  (e (Free e a) -> h (Free e a)) ->
  Free e a ->
  Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = Free (fmap (modifyEffects g) (g e))

-- Example: rename every logged message.  Careful -- this is WRONG:
--
--        > logFibM (relabelNaive (map succ) (fib 3))
--        (3,["fib(3)","fib(2)"])
--
-- Nothing was renamed.  'modifyEffects' walks the SPINE of the computation, and
-- every FibLog in 'fib' sits inside the FibM payload of a FibMemo, which the
-- spine walk never enters.
relabelNaive :: (String -> String) -> FibM a -> FibM a
relabelNaive f = modifyEffects g
  where
    g (FibLog s c) = FibLog (f s) c
    g op = op

-- An operation carrying a computation must rewrite that computation too:
--
--        > logFibM (relabel (map succ) (fib 3))
--        (3,["gjc)4*","gjc)3*"])
--
-- In A4 this is exactly why 'localEnv' needs a case for every operation whose
-- payload is an 'EvalM' -- forget one and the local environment silently
-- fails to reach inside it.
relabel :: (String -> String) -> FibM a -> FibM a
relabel _ (Pure x) = Pure x
relabel f (Free (FibLog s c)) =
  Free $ FibLog (f s) (relabel f c)
relabel f (Free (FibMemo n fn c)) =
  Free $ FibMemo n (relabel f fn) (relabel f . c)

--------------------------------------------------------------------------
-- Computations as data: analysing without running
--------------------------------------------------------------------------

-- When every operation returns (), a computation is a finite LIST of operations
-- and we can read anything we like off it without executing it.

data AuditOp a
  = Say String a
  | Charge Int a

instance Functor AuditOp where
  fmap f (Say s c) = Say s (f c)
  fmap f (Charge n c) = Charge n (f c)

type AuditM a = Free AuditOp a

say :: String -> AuditM ()
say s = Free (Say s (Pure ()))

charge :: Int -> AuditM ()
charge n = Free (Charge n (Pure ()))

order :: AuditM ()
order = do say "open tab"; charge 40
           charge 25; say "close tab"

-- Static: no execution, no state, no IO.
totalCost :: AuditM a -> Int
totalCost (Pure _) = 0
totalCost (Free (Say _ c)) = totalCost c
totalCost (Free (Charge n c)) = n + totalCost c

transcript :: AuditM a -> [String]
transcript (Pure _) = []
transcript (Free (Say s c)) = s : transcript c
transcript (Free (Charge _ c)) = transcript c

runAudit :: AuditM a -> IO a
runAudit (Pure x) = pure x
runAudit (Free (Say s c)) = do
  putStrLn s
  runAudit c
runAudit (Free (Charge n c)) = do
  putStrLn ("charge " ++ show n)
  runAudit c

-- The ceiling.  Add ONE operation that returns a value the computation
-- branches on, and it stops being a list and becomes a tree that we cannot
-- walk without supplying inputs:
--
--        data AuditOp a = ... | AskBudget (Int -> a)
--
-- 'totalCost' can then no longer be written: to get past 'AskBudget k' you
-- must choose an Int and run 'k' on it.  The best you can do is analyse the
-- prefix up to the first such operation, or analyse one path at a time.
-- This is the price of 'get' -- and, in the limit, of (>>=) itself.

--------------------------------------------------------------------------
-- Suspended computations are ordinary values
--------------------------------------------------------------------------

type EventName = String

type EventValue = Int

type Event = (EventName, EventValue)

data EventOp a
  = WaitFor EventName (EventValue -> a)
  | LogMsg String a

instance Functor EventOp where
  fmap f (WaitFor s c) = WaitFor s (f . c)
  fmap f (LogMsg s c) = LogMsg s (f c)

type EventM a = Free EventOp a

waitFor :: EventName -> EventM EventValue
waitFor s = Free (WaitFor s Pure)

logMsg :: String -> EventM ()
logMsg s = Free (LogMsg s (Pure ()))

adder, multiplier :: EventM ()
adder = do
  logMsg "starting adder"
  x <- waitFor "add"; y <- waitFor "add"
  logMsg (unwords [show x, "+", show y, "=", show (x+y)])
multiplier = do
  logMsg "starting multiplier"
  x <- waitFor "mul"; y <- waitFor "mul"
  logMsg (unwords [show x, "*", show y, "=", show (x*y)])

-- Run until the process needs a value it does not have, then STOP and hand
-- the suspended computation back as a value.
stepUntilWait :: EventM a -> IO (EventM a)
stepUntilWait (Pure x) = pure $ Pure x
stepUntilWait (Free (LogMsg s c)) = do
  putStrLn s
  stepUntilWait c
stepUntilWait w@(Free (WaitFor _ _)) = pure w

-- Hand one event to one process: if it is waiting for exactly this event,
-- resume it and run on to its next wait.  A process waiting for something
-- else is left exactly as it is.
deliver :: Event -> EventM () -> IO (EventM ())
deliver (name, val) (Free (WaitFor wanted c))
  | wanted == name = stepUntilWait (c val)
deliver _ p = pure p

-- No scheduler state anywhere: the state of each process IS the suspended
-- computation.
runEventM :: [EventM ()] -> [Event] -> IO [EventM ()]
runEventM ps [] = mapM stepUntilWait ps
runEventM ps (e : es) = do
  ps' <- mapM stepUntilWait ps
  ps'' <- mapM (deliver e) ps'
  runEventM ps'' es
