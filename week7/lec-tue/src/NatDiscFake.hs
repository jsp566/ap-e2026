--------------------------------------------------------------------------------
{-| Module      : Discriminators for numbers
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
--------------------------------------------------------------------------------
module NatDiscFake where

-- discNat xs : fake discriminator, does not use an array (for performance comparison)
discNat16 :: [(Int, v)] -> [[v]]
discNat16 xs =
    filter (not . null) [lowVals, highVals]
    where
       midpoint = 32768
       (lowVals, highVals) = foldr (\(k, v) (lte, gt) -> 
                                   if k <= midpoint then (v : lte, gt) else (lte, v : gt)) 
                                   ([], []) xs
