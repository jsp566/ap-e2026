{-# OPTIONS -XGADTs -fwarn-incomplete-patterns  -XRankNTypes #-}

----------------------------------------------------------------------------------------
{-| Module      : Generic multiset programming with discrimination-based joins, 
                  lazy unions, products, scaling, and counted implementation
    Copyright   : Fritz Henglein, 2011
    License     : All Rights Reserved

    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module FSet where
import Data.Ratio

-- Func a b: symbolic functions (arrows) 
data Func a b where
    Func    :: (a -> b) -> Func a b
    Id      :: Func a a
    (:.:)   :: Func b c -> Func a b -> Func a c
    Fst     :: Func (a, b) a
    Snd     :: Func (a, b) b
    (:|:)   :: Func a b -> Func c d -> Func (a, c) (b, d)
    (:+:)   :: Num a => Func (a, a) a
    (:*:)   :: Num a => Func (a, a) a
    Sq      :: Num a => Func a a

-- string representation of arrow
showFunc :: Func a b -> String
showFunc (Func _)  = "Func <fcn>"
showFunc Id        = "Id"
showFunc (g :.: f) = "(" ++ showFunc g ++ ") . (" ++ showFunc f ++ ")"
showFunc Fst       = "Fst"
showFunc Snd       = "Snd"
showFunc (f :|: g) = "(" ++ showFunc g ++ ") | (" ++ showFunc f ++ ")"
showFunc (:+:)     = "+"
showFunc (:*:)     = "*"
showFunc Sq        = "Sq"

instance Show (Func a b) where
  show = showFunc

-- ext: extension of symbolic function
ext :: Func a b -> a -> b
ext (Func g)           = g
ext Id                 = \x -> x
ext (g :.: f)          = ext g . ext f
ext Fst                = fst
ext Snd                = snd
ext (f :|: g)          = \ (x, y) -> (ext f x, ext g y)
ext (:+:)              = uncurry (+)
ext (:*:)              = uncurry (*)
ext Sq                 = (^ 2)

-- Turn binary operation into arrow
op :: (a -> b -> c) -> Func (a, b) c
op = Func . uncurry

-- MSet a: fuzzy multisets (bags) built from singleton, singleton and union, 
--         extended with symbolic product, scalar multiplication, map
data MSet a where
          -- minimal set of constructors
    O     :: MSet a                            -- the empty multiset 
    S     :: a -> MSet a                       -- singleton multiset
    U     :: MSet a -> MSet a -> MSet a        -- multiset union
          -- extended constructors
    (:.)  :: Double -> MSet a -> MSet a         -- scalar multiplication
    X     :: MSet a -> MSet b -> MSet (a, b)   -- cross-product
    (:$)  :: Func (a, b) c -> (MSet a, MSet b) -> MSet c -- map over Cartesian product

-- string representation of multiset
showMSet :: MSet a -> String
showMSet O = "O"
showMSet (S _) = "S ..."
showMSet (s `U` t) = "(" ++ showMSet s ++ ") U (" ++ showMSet t ++ ")"
showMSet (k :. s) = show k ++ " :. " ++ showMSet s
showMSet (s `X` t) = "(" ++ showMSet s ++ ") X (" ++ showMSet t ++ ")"
showMSet (f :$ (s, t)) = showFunc f ++ ":$ (" ++ showMSet s ++ ", " ++ showMSet t ++ ")"
 
instance Show (MSet a) where
  show = showMSet

-- Cardinality of multiset, may be any rational number, including negative ones
count :: MSet a -> Double
count O             = 0
count (S _)         = 1
count (s `U` t)     = count s + count t
count (r :. s)      = r * count s
count (s `X` t)     = count s * count t
count (f :$ (s, t)) = count s * count t

-- Multiset from list
mset :: [a] -> MSet a
mset = foldr (\x s -> S x `U` s) O

-- equivalent multiset expression constructed from only O, S, U or :.
red :: MSet a -> MSet a
red O = O
red (S x) = S x
red (s `U` t) = red s `U` red t
red (k :. s) = k :. red s
red (O `X` t) = O
red (s `X` O) = O
red (S x `X` S y) = S (x, y)
red (s `X` (t1 `U` t2)) = red (s `X` t1) `U` red (s `X` t2)
red ((s1 `U` s2) `X` t) = red (s1 `X` t) `U` red (s2 `X` t)
red (s `X` (t1 `X` t2)) = red (s `X` red (t1 `X` t2))
red ((s1 `X` s2) `X` t) = red (red (s1 `X` s2) `X` t)
red (s `X` (k :. t)) = k :. (s `X` t)
red ((k :. s) `X` t) = k :. (s `X` t)
red ((f :$ st) `X` u) = red (red (f :$ st) `X` u)
red (s `X` (f :$ tu)) = red (s `X` red (f :$ tu))
red (f :$ (s, t)) = f $$ red (s `X` t)

-- Map over multisets
($$) :: Func a b -> MSet a -> MSet b
f $$ O               = O
Id $$ s              = s
(g :.: f) $$ s       = g $$ (f $$ s)
f $$ S x             = S (ext f x)
f $$ (s `U` t)       = (f $$ s) `U` (f $$ t) 
f $$ (k :. s)        = k :. (f $$ s)
Fst $$ (s `X` t)     = count t :. s
Snd $$ (s `X` t)     = count s :. t
(f :|: g) $$ (s `X` t) = (f $$ s) `X` (g $$ t)
(:+:) $$ (s `X` t)   = (:+:) :$ (s, t)
(:*:) $$ (s `X` t)   = (:*:) :$ (s, t)
f $$ (s `X` t)       = f $$ red (s `X` t)
g $$ (f :$ st)       = (g :.: f) :$ st

msum :: MSet Double -> Double
msum O = 0
msum (S x) = x
msum (s `U` t) = msum s + msum t
msum (k :. s) = k * msum s
msum ((:+:) :$ (s, t)) = count t * msum s + count s * msum t
msum ((:*:) :$ (s, t)) = msum s * msum t   
msum ((Sq :.: (:+:)) :$ (s, t)) = 
   count t * msum (Sq $$ s) + count s * msum (Sq $$ t) + 2 * msum s * msum t
msum ((Sq :.: (:*:)) :$ (s, t)) = msum (Sq $$ s) * msum (Sq $$ t)
msum (f :$ (s, t)) = msum (f $$ red (s `X` t))

-- multiset reduce 
mreduce :: (a -> a -> a, Double -> a -> a, a) -> MSet a -> a
mreduce (a, m, z) O = z
mreduce (a, m, z) (S x) = x
mreduce (a, m, z) (s `U` t) = mreduce (a, m, z) s `a` mreduce (a, m, z) t
mreduce (a, m, z) (k :. s) = k `m` (mreduce (a, m, z) s)
mreduce (a, m, z) s = mreduce (a, m, z) (red s)

-- push weights to leaves
push :: MSet a -> MSet (a, Double)
push O = O
push (S x) = S (x, 1.0)
push (s `U` t) = push s `U` push t
push (r :. s) = Func (\(x, w) -> (x, r * w)) $$ push s 
push s = push (red s)  

-- prepend S xs : Prepend a list representation of S to xs
prepend :: MSet a -> [a] -> [a]
prepend O ys = ys
prepend (S xw) ys = xw : ys
prepend (s `U` t) ys = prepend s (prepend t ys)
prepend s ys = error "Illegal input: Only O, S and U constructors allowed"

-- Some normalized list representation of multiset
list :: MSet a -> [(a, Double)]
list s = prepend (push s) []

-- multiset union
munion :: MSet (MSet a) -> MSet a
munion = mreduce (U, (:.), O)

-- Probability spaces

type Prob a = MSet a -- count must be 1
type Probability = Double -- must be in [0..1]

-- uniform distribution, argument must be nonempty
uniform :: [a] -> Prob a
uniform xs = (1 / fromIntegral (length xs)) :. mset xs

-- Independent product
dprod :: Prob a -> Prob b -> Prob (a, b)
dprod = X

-- djoin
djoin :: Prob (Prob a) -> Prob a
djoin O = O
djoin (S x) = x
djoin (s `U` t) = djoin s `U` djoin t
djoin (r :. s) = r :. djoin s
djoin s = djoin (red s)

-- Alternative, using mreduce
djoin' :: Prob (Prob a) -> Prob a
djoin' = mreduce (U, (:.), O)

-- Probabilistic choice 
choice :: Probability -> Prob a -> Prob a -> Prob a
choice p s t = (p :. s) `U` ((1 - p) :. t)
     
-- Alternative, using djoin
choice' :: Probability -> Prob a -> Prob a -> Prob a
choice' p s t = djoin ((p :. S s) `U` ((1 - p) :. S t))

-- Random variables implemented by symbolic functions
type RV a b = Func a b

-- Push-forward probability space induced by random variable
dpush :: RV a b -> Prob a -> Prob b
dpush = ($$)

-- identity random variable
_X :: RV a a
_X = Id

-- Lifted squaring
sq :: Num a => RV a a
sq = Sq

-- Measure (size) of probability distribution (should always be 1)
measure :: Prob a -> Double
measure = count

-- Expected value of random variable over probability space
expectation :: RV a Double -> Prob a -> Double
expectation rv p = msum (dpush rv p)

-- Variance of random variable over probability space
variance :: RV a Double -> Prob a -> Double
variance rv p = expectation (Sq :.: rv) p - (expectation rv p)^2

-- Examples

die :: Prob Double
die = uniform [1..6]

-- Multiple dice
dice :: Int -> Prob Double
dice 1 = die
dice n = dpush (:+:)  (die `dprod` dice (n-1))


-- Tests
s1 = mset [3, 5, 19]
s2 = mset [11, 29, 31, 47]
s3 = mset (take 10 (repeat s2))
s4 = s1 `dprod` (4 :. s2)
s5 = Id $$ s4
s6 = op (+) $$ (s1 `dprod` s2)
s7 = op (+) $$ (s2 `dprod` s6)
s8 = (4100 :. s2) `U` s7 `U` s6
-- s9 = Sq $$ s8
s10 = Func (^2) $$ s8

t1 = mset [1 .. 20] :: MSet Integer
t2 = (:+:) $$ (t1 `X` t1)
t3 = (:+:) $$ (t1 `X` t2)
t4 = (:+:) $$ (t2 `X` t2)
t5 = (:+:) $$ (t2 `X` t3)
t6 = (:+:) $$ (t3 `X` t3)
t7 = 1000 :. t3
t8 = 1000 :. t7

mrev O = O
mrev (S x) = S x
mrev (s `U` t) = mrev t `U` mrev s
mrev (k :. s) = k :. mrev s
mrev (s `X` t) = mrev (s `dprod` t)
{- mrev (s :$ t) = mrev ((:+:) :$ (s, t))

lprod xs ys = [ (x, y) | x <- xs, y <- ys ]

l1 = [1 .. 20] :: [Integer]
l2 = map (uncurry (+)) (l1 `lprod` l1)
l3 = map (uncurry (+)) (l1 `lprod` l2)
l4 = map (uncurry (+)) (l2 `lprod` l2)
l5 = map (uncurry (+)) (l2 `lprod` l3)
l6 = map (uncurry (+)) (l3 `lprod` l3)
l7 = 1000 `timesl` l3
l8 = 1000 `timesl` l7

l99 = foldr (.) id (map (:) l6) []

rev [] = []
rev (x : xs) = rev xs ++ [x]

timesl :: Integer -> [a] -> [a]
timesl 0 xs = []
timesl 1 xs = xs
timesl n xs = xs ++ timesl (n-1) xs
-}