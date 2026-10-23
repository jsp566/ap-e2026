--------------------------------------------------------------------------------
{-| Module      : Discriminators for numbers
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
--------------------------------------------------------------------------------
module NatDisc where

import Control.Monad
import Data.Array.IO
import System.IO.Unsafe

-- discNat size : Returns basic multiset discriminator with table of given size, 
-- preallocated and initialized. Calls reuse the same table.  In contrast to bucket sorting, 
-- discNat iterates over the list of keys of added, not the the whole array. 
-- WARNING: Code is neither thread-safe nor reentrant; must be called with fully evaluated keys
{-# NOINLINE discNat #-} 
discNat :: Int -> [(Int, v)] -> [[v]]
discNat size =
  unsafePerformIO (
  do table <- newArray (0, size) [] :: IO (IOArray Int [v])
     let discNat' xs = unsafePerformIO (
            do ks <- foldM (\keys (k, v) -> 
                               do vs <- readArray table k
                                  case vs of [] -> do writeArray table k [v]
                                                      return (k : keys)
                                             _ ->  do writeArray table k (v : vs)
                                                      return keys)
                            [] xs
               foldM (\vss k -> do elems <- readArray table k
                                   writeArray table k []
                                   return (reverse elems : vss)) 
                       [] ks )
     return discNat' ) 

-- Basic multiset discriminator with table of size 2^16 = 65536.  
-- Keys must have type Int and be in range [0 .. 2^16-1]
discNat16 :: [(Int, v)] -> [[v]]
discNat16 = discNat 65536

