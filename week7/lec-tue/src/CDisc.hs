----------------------------------------------------------------------------------------
{-| Module      : Comparison-based sorting
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module CDisc where

import Data.List (sortBy, groupBy)

comp lte x y = if lte x y 
               then if lte y x
                    then EQ
                    else LT
               else GT

cdisc :: (a -> a -> Bool) -> [(a, b)] -> [[b]]      
cdisc lte xs = map (map snd) (groupBy lteInv' (sortBy (comp lte') xs))
   where lte' (x1, y1) (x2, y2) = lte x1 x2
         lteInv' x y = lte' y x
           