{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs #-}

----------------------------------------------------------------------------------------
{-| Module      : Relational algebra, with discrimination-based joins and lazy products
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved

    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module SQL4 where
import Order 
import Equiv 
import Disc (disc, part, reps)
-- import Disc2 (disc2, part2)

-- Data types

-- MSet a: bags (multisets) of elements of type a, implemented with lazy union and cross-product
data MSet a where
    MSet  :: [a] -> MSet a                   -- multiset from list 
    U     :: MSet a -> MSet a -> MSet a      -- union
    X     :: MSet a -> MSet b -> MSet (a, b) -- product

-- Empty multiset
empty = MSet []

-- Prepend some list representation of a multiset to a list
prepend :: MSet a -> [a] -> [a]
prepend (MSet xs) zs = xs ++ zs
prepend (s `U` t) zs = prepend s (prepend t zs)
prepend (s `X` t) zs = foldr (\x us -> foldr (\y vs -> (x, y) : vs) us ys) zs xs
      where xs = list s
            ys = list t
-- Some list representation of a multiset, depends on multiset representation
list :: MSet a -> [a]
list s = prepend s []

-- Cardinality of multiset 
count :: MSet a -> Int
count (MSet xs) = length xs
count (s `U` t) = count s + count t
count (s `X` t) = count s * count t

-- Tree size of multiset, depends on multiset representation
size :: MSet a -> Int
size (MSet xs) = length xs
size (s `U` t) = size s + size t + 1
size (s `X` t) = size s + size t + 1

-- Performable functions
data Func a b where 
    Func :: (a -> b) -> Func a b                        -- from ordinary function
    Par  :: Func a b -> Func c d -> Func (a, c) (b, d)  -- parallel composition
    Fst  :: Func (a, b) a                               -- first component of pair
    Snd  :: Func (a, b) b                               -- second component of pair

-- ext p: functional extension of projection p
ext :: Func a b -> a -> b
ext (Func f) x         = f x
ext (Par f1 f2) (x, y) = (ext f1 x, ext f2 y)
ext Fst (x, _)         = x
ext Snd (_, y)         = y

-- Apply function to each element of a multiset
perform :: Func a b -> MSet a -> MSet b
perform f (s `U` t)         = perform f s `U` perform f t
perform (Par f g) (s `X` t) = perform f s `X` perform g t
perform Fst (s `X` t)       = count t `times` s
perform Snd (s `X` t)       = count s `times` t
perform f s                 = MSet (map (ext f) (list s)) -- default clause

-- Union of k copies of s for k >= 0
times :: Int -> MSet a -> MSet a
times 0 s = empty
times 1 s = s
times n s = s `U` times (n - 1) s

-- Predicates
data Pred a where 
    Pred :: (a -> Bool) -> Pred a             -- from Boolean-valued function
    TT   :: Pred a                            -- constant true
    FF   :: Pred a                            -- constant false
    SAnd :: Pred a -> Pred a -> Pred a        -- sequential conjunction
    PAnd :: Pred a -> Pred b -> Pred (a, b)   -- parallel conjunction
    SOr  :: Pred a -> Pred a -> Pred a        -- sequential disjunction
    POr  :: Pred a -> Pred b -> Pred (a, b)   -- parallel disjunction
    Is   :: (a -> k, b -> k) -> Equiv k -> Pred (a, b) -- join condition

-- Predicate as Boolean-valued function
sat :: Pred a -> a -> Bool
sat (Pred f) x             = f x
sat TT _                   = True
sat FF _                   = False
sat (p1 `SAnd` p2) x       = (sat p1 x && sat p2 x)
sat (p1 `PAnd` p2) (x, y)  = sat p1 x && sat p2 y
sat (p1 `SOr` p2) x        = sat p1 x || sat p2 x
sat (p1 `POr` p2) (x, y)   = sat p1 x || sat p2 y
sat ((f, g) `Is` e) (x, y) = eq e (f x) (g y)


-- select s p : select (filter) all elements of s that satisfy p
select :: Pred a -> MSet a -> MSet a
select TT s                      = s
select FF s                      = empty
select p (s `U` t)               = select p s `U` select p t
select (p `SAnd` q) s            = select q (select p s)
select (p `SOr` q) s             = select p s `U` select q s
select (p `PAnd` q)  (s `X` t)   = select p s `X` select q t
select (p `POr` q) (s `X` t)     = (select p s `X` t) `U` (s `X` select q t)
select ((f, g) `Is` e) (s `X` t) = djoin (f, g) e (s, t)
select p s                       = MSet (filter (sat p) (list s)) -- default
 
-- djoin f1 e f2 s1 s2 = { (x1, x2) : x1 in s1, x2 in s2 | f1(x1) is e-equiv with f2(x2) }
djoin :: (a -> k, b -> k) -> Equiv k -> (MSet a, MSet b) -> MSet (a, b)
djoin (f, g) e (s, t) =  
     foldr (\(vs, ws) s -> (MSet vs `X` MSet ws) `U` s) empty fprods
     where xs = [ (f r, Left r) | r <- list s ]
           ys = [ (g r, Right r) | r <- list t ]
           bs = disc e (xs ++ ys)
           fprods = map split bs
           split :: [Either a b] -> ([a], [b])
           split [] = ([], [])
           split (v : vs) = let (lefts, rights) = split vs
                            in case v of Left v' -> (v' : lefts, rights) 
                                         Right v' -> (lefts, v' : rights) 

-- diff s1 (e, s2): Return elements in s1 that do not have an e-equivalent element in s2
diff :: Equiv k -> MSet k -> MSet k -> MSet k
diff e s t = foldr include empty bs
   where 
   xs = [ (x, Left x) | x <- list s ]
   ys = [ (y, Right y) | y <- list t ]
   bs = disc e (ys ++ xs)
   untag (Left x) = x
   untag (Right _) = error "Impossible: Erroneously tagged value"
   include b@(Left x : _) s =  MSet (map untag b) `U` s
   include b@(Right y : _) s = s
   include [] s = error "Impossible: Application to empty block"

-- Group (partition) by equivalence class e
groupBy :: Equiv a ->  MSet a -> MSet (MSet a)
groupBy e s = MSet (map MSet (part e (list s)))

-- Replace e-equivalent elements by a single e-equivalence class representative
coalesceBy :: Equiv a -> MSet a -> MSet a
coalesceBy e s = MSet (reps e (list s))

-- sortBy r: Sort by ordering relation r (should be total order)
orderBy :: Order a -> MSet a -> [a]
orderBy r s = csort r (list s)

-- having 
having :: MSet (MSet a) -> Pred (MSet a) -> MSet (MSet a)
having s p = select p s

-- Fold 
fold :: (a -> b -> b) -> b -> MSet a -> b
fold f n = foldr f n . list -- default implementation

-- Reduce multiset with commutative monoid (f, n)
reduce :: (a -> a -> a) -> a -> MSet a -> a
reduce = fold -- default implementation

-- selectFromWhere : SQL style SELECT on single table (set)
selectFromWhere :: Func a b -> MSet a -> Pred a -> MSet b
selectFromWhere f s p = perform f (select p s)

-- Examples:

s1 = MSet [1, 2, 3]
s2 = MSet [10, 25, 30, 40]

mod2 = MapE (`mod` 2) (NatE 2)
jcond = (id, id) `Is` mod2

res = list $ select jcond (s1 `X` s2)


