----------------------------------------------------------------------------------------
{-| Module      : Comparison-based sorting
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module MDisc where
import Data.List (splitAt, groupBy)

merge lte [] ys = ys
merge lte xs [] = xs
merge lte (x : xs) (y : ys) = 
    if x `lte` y
    then x : merge lte xs (y : ys)
    else y : merge lte (x : xs) ys

msort lte [] = []
msort lte [x] = [x]
msort lte xs = merge lte (msort lte lefts) (msort lte rights)
    where (lefts, rights) = splitAt (length xs `div` 2) xs

mdisc lte xs = map (map snd) (groupBy lteInv' (msort lte' xs))
   where lte' (x1, y1) (x2, y2) = lte x1 x2
         lteInv' x y = lte' y x
