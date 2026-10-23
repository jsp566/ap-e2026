{-# LANGUAGE GADTs, RankNTypes #-}

----------------------------------------------------------------------------------------
{-| Module      : Discriminators
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module Disc where
import Order
import Equiv 
import NatDisc (discNat16)
import SDisc (groupNum, updateBag, updateSet, bdiscNat, dsort)

-- Generic equivalence discriminator
disc :: Equiv k -> [(k, v)] -> [[v]]
disc _ []               = []
disc _ [(_, v)]         = [[v]]
disc (NatE n) xs        = 
   if n < 65536 then discNat16 xs else disc eqInt32 xs 
disc TrivE xs           = [map snd xs]
disc (SumE e1 e2) xs    = disc e1 [ (k, v) | (Left k, v) <- xs ] ++ 
                          disc e2 [ (k, v) | (Right k, v) <- xs ]
disc (ProdE e1 e2) xs   = 
   [ vs | ys <-disc e1 [ (k1, (k2, v)) | ((k1, k2), v) <- xs ],
          vs <- disc e2 ys ]
disc (MapE f e) xs      = disc e [ (f k, v) | (k, v) <- xs ] 
disc (ListE e) xs       = disc (listE e) xs
disc (BagE e) xs        = discColl updateBag e xs
disc (SetE e) xs        = discColl updateSet e xs

-- Collection equivalence discriminator
discColl :: ([Int] -> Int -> [Int]) -> Equiv k -> [([k], v)] -> [[v]]
discColl update e xss = disc (listE (NatE (length keyNumBlocks - 1))) yss
     where
     (kss, vs)   = unzip xss                   -- key lists and values unzipped 
     elemKeyNumAssocs = groupNum kss
     keyNumBlocks   = disc e elemKeyNumAssocs  -- membership groups, one group per e-class 
     keyNumElemNumAssocs = groupNum keyNumBlocks
     sigs        = bdiscNat (length kss) update keyNumElemNumAssocs
     yss         = zip sigs vs            -- signature lists and values (re)zipped

-- Return true if equivalence representation contains SumE or ProdE
reqPostProc :: Equiv t -> Bool
reqPostProc (NatE _) = False
reqPostProc TrivE = False
reqPostProc (SumE _ _) = True
reqPostProc (ProdE _ _) = True
reqPostProc (MapE _ e) = reqPostProc e
reqPostProc (ListE _) = True
reqPostProc (BagE _) = True
reqPostProc (SetE _) = True

-- edisc : Fully abstract equivalence discriminator, with selective postprocessing
edisc :: Equiv k -> [(k, v)] -> [[v]]
edisc e xs | reqPostProc e = edisc' e xs  
  where reqPostProc :: Equiv t -> Bool
        reqPostProc (NatE _) = False
        reqPostProc TrivE = False
        reqPostProc (SumE _ _) = True
        reqPostProc (ProdE _ _) = True
        reqPostProc (MapE _ e) = reqPostProc e
        reqPostProc (ListE _) = True
        reqPostProc (BagE _) = True
        reqPostProc (SetE _) = True
edisc e xs | otherwise = disc e xs

-- edisc' : fully abstract equivalence discriminator, with postprocessing
edisc' :: Equiv k -> [(k, v)] -> [[v]]
edisc' e xs = map (map snd) 
                  (dsort (ListL (ProdL (NatO (length xs)) TrivO))
                         (disc e xs'))
     where xs' = map relabel (zip xs ([0..] :: [Int]))
           relabel ((x, y), z) = (x, (z, y))

-- Partitioning from discrimination

part :: Equiv t -> [t] -> [[t]]
part e xs = disc e [ (x, x) | x <- xs ]

epart :: Equiv t -> [t] -> [[t]]
epart e xs = edisc e [ (x, x) | x <- xs ]

-- Representatives from partitioning

reps :: Equiv t -> [t] -> [t]
reps e xs = [ head ys | ys <- part e xs ]

ereps :: Equiv t -> [t] -> [t]
ereps e xs = [ head ys | ys <- epart e xs ]