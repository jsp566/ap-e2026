{-# LANGUAGE GADTs #-}
----------------------------------------------------------------------------------------
{-| Module      : Order -- denotations of total preorders
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module Order where
import Data.Bits
import Data.Char
import Data.List (sortBy, groupBy)

-- Order T: Representations of ordering relations over type T
data Order t where
     NatO      :: Int -> Order Int  
     TrivO     :: Order t      
     SumL     :: Order t1 -> Order t2 -> Order (Either t1 t2)
     ProdL    :: Order t1 -> Order t2 -> Order (t1, t2)
     ListL    :: Order t -> Order [t]
     MapO     ::  (t1 -> t2) -> Order t2 -> Order t1 
     BagO     :: Order t -> Order [t]
     SetO     :: Order t -> Order [t]
     Inv      ::  Order t -> Order t 

-- lte r : characteristic function (inequality test) denoted by order denotation r
lte :: Order t -> t -> t -> Bool
lte r x y = ordVal == LT || ordVal == EQ
    where ordVal = comp r x y

-- comp r : comparison function denoted by r
comp :: Order t -> t -> t -> Ordering
comp (NatO n) x y = if 0 <= x && x <= n && 0 <= y && y <= n
                   then compare x y
                   else error "Argument out of range"
comp TrivO _ _ = EQ
comp (SumL r1 _) (Left x) (Left y) = comp r1 x y
comp (SumL _ _) (Left _) (Right _) = LT
comp (SumL _ _) (Right _) (Left _) = GT
comp (SumL _ r2) (Right x) (Right y) = comp r2 x y
comp (ProdL r1 r2) (x1, x2) (y1, y2) = 
   case comp r1 x1 y1 of LT -> LT
                         EQ -> comp r2 x2 y2
                         GT -> GT
comp (ListL r) [] [] = EQ
comp (ListL r) [] _ = LT
comp (ListL r) _ [] = GT
comp (ListL r) (x : xs) (y : ys) = 
   case comp r x y of LT -> LT
                      EQ -> comp (ListL r) xs ys
                      GT -> GT
comp (MapO f r) x y = comp r (f x) (f y)
comp (BagO r) xs ys = comp (MapO (csort r) (ListL r)) xs ys
comp (SetO r) xs ys = comp (MapO (cusort r) (ListL r)) xs ys
comp (Inv r) x y = comp r y x

-- sort r : comparison-based sorting function
csort :: Order t -> [t] -> [t]
csort r = sortBy (comp r)

-- usort r : comparison-based unique-sorting function (equivalent keys are eliminated)
cusort :: Order t -> [t] -> [t]
cusort r = map head . groupBy (lte (Inv r)) . sortBy (comp r)

instance Show (Order t) where
  show (NatO n) = "(NatO " ++ show n ++ ")"
  show TrivO = "TrivO"
  show (SumL r1 r2) = "(SumL " ++ show r1 ++ " " ++ show r2 ++ ")"
  show (ProdL r1 r2) = "(ProdL " ++ show r1 ++ " " ++ show r2 ++ ")"
  show (MapO _ r) = "(MapO " ++ "<fcn> " ++ show r ++ ")"
  show (ListL r) = "(ListL " ++ show r ++ ")"
  show (BagO r) = "(BagO " ++ show r ++ ")"
  show (SetO r) = "(SetO " ++ show r ++ ")"
  show (Inv r) = "(Inv " ++ show r ++ ")"

-- Definitions of standard orderings

-- ordUnit: standard order on unit type
ordUnit :: Order ()
ordUnit = TrivO

-- bool: standard order on Bool: False < True
ordBool :: Order Bool
ordBool = MapO bool2sum (SumL ordUnit ordUnit)
  where bool2sum :: Bool -> Either () ()
        bool2sum False = Left ()
        bool2sum True = Right ()

-- ordNat8 : standard ascending order on Int, restricted to 8-bit nonnegative integers
ordNat8 :: Order Int
ordNat8 = NatO 255

-- ordNat16 : standard ascending order on Int, restricted to 16-bit nonnegative integers
ordNat16 :: Order Int
ordNat16 = NatO 65535

-- ordNat31 : standard ascending order on Int, restricted to 31-bit nonnegative integers
ordNat31 :: Order Int
ordNat31 = MapO splitW (ProdL ordNat16 ordNat16)

-- splitW : split 32-bit integer into nonnegative integers represented by upper and lower 16 bits
splitW :: Int -> (Int, Int)
splitW x = (shiftR x 16 .&. 65535, x .&. 65535)

-- ordInt32 : standard ascending order on Int
ordInt32 :: Order Int 
ordInt32 = MapO (splitW . (+ (-2147483648))) (ProdL ordNat16 ordNat16)

-- ordChar8 : standard alphabetic order on Char, restricted to 8-bit characters 
ordChar8 :: Order Char        
ordChar8 = MapO ord ordNat8

-- ordChar16 : standard alphabetic order on Char, restricted to 16-bit characters 
ordChar16 :: Order Char        
ordChar16 = MapO ord ordNat16

-- fromList: unfold-part of isomorphism between [t] and Either () (t, [t])
fromList :: [t] -> Either () (t, [t])
fromList []       = Left ()
fromList (x : xs) = Right (x, xs)

-- Lexicographic ordering on lists, with elements ordered according to r
listL :: Order t -> Order [t]
listL r = MapO fromList (SumL ordUnit (ProdL r (listL r)))

-- ordString8 : standard alphabetic order on String, restricted to 8-bit characters
ordString8 :: Order String
ordString8 = ListL ordChar8

-- Ordering of tagged values, but with Right elements first
sumR :: Order t1 -> Order t2 -> Order (Either t1 t2)
sumR r1 r2 = Inv (SumL (Inv r1) (Inv r2))

-- Alternative:
sumR' r1 r2 = MapO flip (SumL r2 r1)
   where flip :: Either t1 t2 -> Either t2 t1
         flip (Left x) = Right x
         flip (Right y) = Left y

-- List-ordering with empty list last:
listR :: Order t -> Order [t]
listR r = MapO fromList (sumR' ordUnit (ProdL r (listR r)))

-- Lexicographic ordering on pairs, but with second component dominant
pairR :: Order t1 -> Order t2 -> Order (t1, t2)
pairR r1 r2 = MapO swap (ProdL r2 r1)
    where swap :: (t1, t2) -> (t2, t1)
          swap (x, y) = (y, x)

-- Refinement of one ordering by another: 
-- x (Refine r1 r2) y <=> x (r1) y and (y (r1) x => x (r2) y)
refine :: Order t -> Order t -> Order t
refine r1 r2 = MapO dup (ProdL r1 r2)
    where dup x = (x, x)

-- simplify: Algebraic simplification of order representations
simplify :: Order t -> Order t
simplify r@(NatO _) = r
simplify TrivO = TrivO
simplify (SumL r1 r2) = SumL (simplify r1) (simplify r2)
simplify (ProdL r1 r2) = ProdL (simplify r1) (simplify r2)
simplify (MapO f (MapO g r)) = simplify (MapO (g . f) r)
simplify (MapO f r) = MapO f (simplify r)
simplify (ListL r) = ListL (simplify r)
simplify (BagO r) = BagO (simplify r)
simplify (SetO r) = SetO (simplify r)
simplify r@(Inv (NatO _)) = r
simplify (Inv TrivO) = TrivO
simplify (Inv (SumL r1 r2)) = sumR' (simplify (Inv r1)) (simplify (Inv r2))
simplify (Inv (ProdL r1 r2)) = ProdL (simplify (Inv r1)) (simplify (Inv r2))
simplify (Inv (MapO f (MapO g r))) = simplify (Inv (MapO (g . f) r))
simplify (Inv (MapO f r)) = MapO f (simplify (Inv r))
simplify (Inv (ListL r)) = listR (simplify (Inv r))
simplify (Inv (BagO r)) = Inv (BagO (simplify r))
simplify (Inv (SetO r)) = Inv (SetO (simplify r))
simplify (Inv (Inv r)) = simplify r

-- Additional examples of order representations

-- evenOdd: even number < odd number, 
evenOdd :: Order Int 
evenOdd = MapO (`mod` 2) (NatO 2)

-- Dershowitz-Manna multiset ordering
multiset :: Order t -> Order [t]
multiset r = MapO (csort (Inv r)) (ListL r)

-- Dershowitz-Manna style set ordering 
set :: Order t -> Order [t]
set r = MapO (cusort (Inv r)) (ListL r)

-- UTree: unlabeled trees
newtype UTree = UTree { unUTree :: [UTree] } deriving Show

-- utree: Standard ordering on unlabeled trees
utree = MapO unUTree (ListL utree)

-- utreeMS: Multiset ordering on unlabeled trees
utreeMS = MapO unUTree (multiset utreeMS)

-- Tree v: v-labeled trees
data Tree v = Node { unNode :: (v, [Tree v]) }

-- tree: Standard order on trees
tree :: Order t -> Order (Tree t)
tree r = MapO unNode (ProdL r (ListL (tree r)))

-- treeK: Level-k cut-off order (subtrees at level >= k are considered equivalent)
treeK :: Int -> Order t -> Order (Tree t)
treeK 0 _ = TrivO
treeK k r = MapO unNode (ProdL r (ListL (treeK (k-1) r)))

-- flipFlop: orders lists by lexicographic ordering on last, 
-- first, next-to-last, second, etc., elements of lists
flipFlop :: Order t -> Order [t]
flipFlop r = 
  MapO (fromList . reverse) (SumL ordUnit (ProdL r (flipFlop r)))


-- Examples:
-- genFullTree n: Full binary tree of depth n
genFullTree n | n <= 0 = UTree []
genFullTree n | otherwise = UTree [genFullTree (n-1), genFullTree (n-1)]

genLeftTree n | n <= 0 = UTree []
genLeftTree n | otherwise = UTree [genLeftTree (n-1), genLeftTree 0]
