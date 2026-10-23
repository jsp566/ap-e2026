{-# LANGUAGE GADTs, RankNTypes #-}
----------------------------------------------------------------------------------------
{-| Module      : Sorting discriminators
    Copyright   : Fritz Henglein, 2010
    License     : All Rights Reserved

    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module SDisc where

import Order
import Data.Array (accumArray, elems)

type Disc k = forall v. [(k, v)] -> [[v]]

-- Generic definition of stable sorting discriminators 
sdisc :: Order k -> Disc k

sdisc _ []               = []
sdisc _ [(_, v)]         = [[v]]
sdisc (NatO n) xs         = sdiscNat n xs
sdisc TrivO xs            = [[ v | (_, v) <- xs ]]
sdisc (SumL r1 r2) xs    = 
   sdisc r1 [ (k, v) | (Left k, v) <- xs ] ++ sdisc r2 [ (k, v) | (Right k, v) <- xs ]
sdisc (ProdL r1 r2) xs   = [ vs | ys <- sdisc r1 [ (k1, (k2, v)) | ((k1, k2),  v) <- xs ],
                                  vs <- sdisc r2 ys ]
sdisc (MapO f r) xs      = sdisc r [ (f k, v) | (k, v) <- xs ] 
sdisc (ListL r) xs       = sdisc (listL r) xs
sdisc (BagO r) xs        = sdiscColl updateBag r xs
sdisc (SetO r) xs        = sdiscColl updateSet r xs
sdisc (Inv r) xs         = reverse (sdisc r xs)

-- Collection order discriminator
sdiscColl :: ([Int] -> Int -> [Int]) -> Order k -> Disc [k]
sdiscColl update r xss = sdisc (listL (NatO (length keyNumBlocks - 1))) yss
     where
     (kss, vs)           = unzip xss
     elemKeyNumAssocs    = groupNum kss
     keyNumBlocks        = sdisc r elemKeyNumAssocs
     keyNumElemNumAssocs = groupNum keyNumBlocks
     sigs                = bdiscNat (length kss) update keyNumElemNumAssocs
     yss                 = zip sigs vs

-- Bucket-sorting discriminator
sdiscNat :: Int -> [(Int, v)] -> [[v]]
sdiscNat n xs = filter (not . null) (bdiscNat n updateBag xs)

-- bdiscNat n update [(k1, v1), ...]:
-- initialize bucket table T[0..n-1];
-- for each i, write update T[ki] vi into T[ki];
-- return [ T[i] | i <- [0..n-1] ]
bdiscNat :: Int -> ([v] -> v -> [v]) -> [(Int, v)] -> [[v]]
bdiscNat n update xs = map reverse (elems (accumArray update [] (0, n) xs))

updateBag vs v = v : vs
updateSet [] w = [w]
updateSet vs@(v : _) w = if v == w then vs else w : vs

-- Group number representation of list of groups:
-- e.g. groupNum [[k01, k02], [k11, k12, k13], [k21]] = 
--     [(k01, 0), (k02, 0), (k11, 1), (k12, 1), (k13, 1), (k21, 2)]
groupNum :: [[k]] -> [(k, Int)]
groupNum xss = [ p | (xs, i) <- zip xss [0..], p <- zip xs (repeat i) ]

      
-- Sorted partitioning from sorted discrimination
spart :: Order t -> [t] -> [[t]]
spart r xs = sdisc r [ (x, x) | x <- xs ]

-- Discriminative sorting from sorted partitioning
dsort :: Order t -> [t] -> [t]
dsort r xs = [ y | ys <- spart r xs, y <- ys ]

-- Unique sorting from ordered partitioning
dusort :: Order t -> [t] -> [t]
dusort r xs = [ head ys | ys <- spart r xs ]

-- Comparator defined from ordered discriminator
dcomp :: Order t -> t -> t -> Ordering
dcomp r x y = case sdisc r [(x, LT), (y, GT)] of {
                [_] -> EQ ;
                (o : _) : _ -> o ;
                _ -> error "Impossible: 0 or more than two elements in result of sdisc" }