----------------------------------------------------------------------------------------
{-| Module      : Comparison-based sorting
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module QDisc where

pivot lte [] = error "Empty argument to pivot"
pivot lte [x] = x
pivot lte [x, y] = x
pivot lte [x, y, z] = 
   if x `lte` y
      then if y `lte` z 
           then y
           else if x `lte` z
                then z
                else x
       else if y `lte` z
            then if x `lte` z
                 then x
                 else z
            else y
pivot lte (x : xs) = pivot lte [x, mid, last sndHalf] 
    where (mid : sndHalf) = drop ((length xs - 1) `div` 2) xs

partition lte p (lefts, equivs, rights) [] = (lefts, equivs, rights)
partition lte p (lefts, equivs, rights) (x : xs) =
   if x `lte` p 
   then if p `lte` x 
        then partition lte p (lefts, x : equivs, rights) xs
        else partition lte p (x : lefts, equivs, rights) xs
   else partition lte p (lefts, equivs, x : rights) xs

qsort :: (a -> a -> Bool) -> [a] -> [[a]]
qsort lte [] = []
qsort lte [x] = [[x]]
qsort lte xs = qsort lte lefts ++ (equivs : qsort lte rights)
   where (lefts, equivs, rights) = partition lte (pivot lte xs) ([], [], []) xs

qdisc :: (a -> a -> Bool) -> [(a, b)] -> [[b]]      
qdisc lte xs = [ map snd block |  block <- qsort lte' xs ]
   where lte' (x1, y1) (x2, y2) = lte x1 x2