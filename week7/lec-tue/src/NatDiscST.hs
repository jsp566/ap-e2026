--------------------------------------------------------------------------------
{-| Module      : Discriminators for numbers
    Copyright   : Fritz Henglein, 2010
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
--------------------------------------------------------------------------------
module NatDiscST where

import Control.Monad.ST
import Control.Monad
import Data.Array.ST

-- discNatST n: Multiset discriminator on [0..n], using ST monad.
-- Each call allocates, initializes and uses a new table T[0..65535].  
discNatST :: Int -> [(Int, v)] -> [[v]]
discNatST n xs =
  runST (
    do table <- newArray (0, n) [] :: ST s (STArray s Int [v])
       ks <- foldM (\keys (k, v) ->
                     do vs <- readArray table k
                        writeArray table k (v : vs)
                        if null vs then return (k:keys) else return keys)
             [] xs
       foldM (\vss k -> do elems <- readArray table k
                           return (reverse elems : vss) )
             [] ks 
   )
