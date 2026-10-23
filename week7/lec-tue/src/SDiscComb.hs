{-# LANGUAGE Rank2Types #-}
----------------------------------------------------------------------------------------
{-| Module      : Sorting discriminators
    Copyright   : Fritz Henglein, 2010
    License     : All Rights Reserved

    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module SDiscComb where

import Order
import Data.Array (accumArray, elems)
import SDisc
import Data.Char

sdiscTriv :: Disc k
sdiscTriv [] = []
sdiscTriv [(_, v)] = [[v]]
sdiscTriv xs = [[ v | (_, v) <- xs ]]

sdiscInt :: Disc Int
sdiscInt [] = []
sdiscInt [(_, v)] = [[v]]
sdiscInt xs = sdiscMapO (splitW . (+ (-2147483648)))
                        (sdiscProdL (sdiscNat 65535) (sdiscNat 65535)) xs

sdiscSumL :: Disc k1 -> Disc k2 -> Disc (Either k1 k2)
sdiscSumL d1 d2 [] = []
sdiscSumL d1 d2 [(_, v)] = [[v]]
sdiscSumL d1 d2 xs  =
    d1 [ (k1, v1) | (Left k1, v1) <- xs ] ++ d2 [ (k2, v2) | (Right k2, v2) <- xs ]

sdiscSumL' d1 d2 xs = d1 lefts ++ d2 rights
   where (lefts, rights) = split xs 

sdiscProdL :: Disc k1 -> Disc k2 -> Disc (k1, k2)
sdiscProdL d1 d2 [] = []
sdiscProdL d1 d2 [(_, v)] = [[v]]
sdiscProdL d1 d2 xs = 
 [ vs | ys <- d1 [ (k1, (k2, v)) | ((k1, k2), v) <- xs ],
        vs <- d2 ys ]

sdiscMapO :: (k1 -> k2) -> Disc k2 -> Disc k1
sdiscMapO f d [] = []
sdiscMapO f d [(_, v)] = [[v]]
sdiscMapO f d xs  = d [ (f k, v) | (k, v) <- xs ] 

sdiscListL :: Disc k -> Disc [k]
sdiscListL d [] = []
sdiscListL d [(_, v)] = [[v]]
sdiscListL d xs = case nilVals of
                    [] -> bs
                    _  -> nilVals : bs
   where splitL [] = ([], [])
         splitL ((ks, v) : xs) =
            case ks of
              [] -> (v : nilVals, pairVals)
              (k : ks') -> (nilVals, (k, (ks', v)) : pairVals)
            where (nilVals, pairVals) = splitL xs
         (nilVals, pairVals) = splitL xs
         bs = [ vs | ys <- d pairVals, vs <- sdiscListL d ys ]

sdiscBagO :: Disc k -> Disc [k]
sdiscBagO d [] = []
sdiscBagO d [(_, v)] = [[v]]
sdiscBagO d xs = sdiscCollO updateBag d xs

sdiscSetO :: Disc k -> Disc [k]
sdiscSetO d [] = []
sdiscSetO d [(_, v)] = [[v]]
sdiscSetO d xs = sdiscCollO updateSet d xs

sdiscCollO :: ([Int] -> Int -> [Int]) -> Disc k -> Disc [k]
sdiscCollO update d xss = sdiscListL (sdiscNat (length keyNumBlocks)) yss
     where
     (kss, vs)           = unzip xss
     elemKeyNumAssocs    = groupNum kss
     keyNumBlocks        = d elemKeyNumAssocs
     keyNumElemNumAssocs = groupNum keyNumBlocks
     sigs                = bdiscNat (length kss) update keyNumElemNumAssocs
     yss                 = zip sigs vs

sdiscInv :: Disc k -> Disc k
sdiscInv d []       = []
sdiscInv d [(_, v)] = [[v]]
sdiscInv d xs       = reverse (d xs)

sdiscChar8 :: Disc Char
sdiscChar8 = sdiscMapO ord (sdiscNat 65535)

sdiscString8 :: Disc String
sdiscString8 = sdiscListL sdiscChar8

-- Split tagged key/value pairs into respective lists
split :: [(Either k1 k2, v)] -> ([(k1, v)], [(k2, v)])
split [] = ([], [])
split ((k, v) : xs) = 
   case k of
     Left k' -> ((k', v) : lefts, rights)
     Right k'' -> (lefts, (k'', v) : rights)
   where (lefts, rights) = split xs
