--------------------------------------------------------------------------------
{-| Module      : Discriminators for numbers
    Copyright   : Fritz Henglein, 2010
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
--------------------------------------------------------------------------------
module NatDiscFA where

import Data.Array

-- discNat16: Multiset discriminator on [0..n], using functional arrays
-- Based on functional (immutable) arrays
discNatFA :: Int -> [(Int, v)] -> [[v]]
discNatFA n xs =
   map (reverse . (table !)) (reverse keys)
   where 
   (table, keys) = 
     foldl (\ (table, keys)  (k, v) -> 
                     case table ! k of
                       [] -> (table // [(k, [v])], k : keys)
                       vs -> (table // [(k, v : vs)], keys))
                 (listArray (0, n) (repeat []), [])
                 xs
             
