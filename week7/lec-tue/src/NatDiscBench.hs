{-# LANGUAGE GADTs #-}
{- Benchmarking of different versions of integer discrimination using criterion.
   Compile with the command: ghc -O3 -W --make NatDiscBench.hs -o NatDiscBench -}

import qualified Criterion.Main as C

import Equiv
import System.Random
import qualified NatDisc as NDIO
import qualified NatDiscST as NDST



-- Specialised versions of disc for various versions of NatDisc

discIO :: Equiv k -> [(k, v)] -> [[v]]
discIO _ []               = []
discIO _ [(_, v)]         = [[v]]
discIO (NatE n) xs        = NDIO.discNat16 xs 
discIO TrivE xs           = [map snd xs]
discIO (SumE e1 e2) xs    = discIO e1 [ (k, v) | (Left k, v) <- xs ] ++ 
                            discIO e2 [ (k, v) | (Right k, v) <- xs ]
discIO (ProdE e1 e2) xs   = 
   [ vs | ys <-discIO e1 [ (k1, (k2, v)) | ((k1, k2), v) <- xs ],
          vs <- discIO e2 ys ]
discIO (MapE f e) xs      = discIO e [ (f k, v) | (k, v) <- xs ] 
discIO (ListE e) xs       = discIO (listE e) xs

discST :: Equiv k -> [(k, v)] -> [[v]]
discST _ []               = []
discST _ [(_, v)]         = [[v]]
discST (NatE n) xs        = NDST.discNatST 65536 xs 
discST TrivE xs           = [map snd xs]
discST (SumE e1 e2) xs    = discST e1 [ (k, v) | (Left k, v) <- xs ] ++ 
                            discST e2 [ (k, v) | (Right k, v) <- xs ]
discST (ProdE e1 e2) xs   = 
   [ vs | ys <-discST e1 [ (k1, (k2, v)) | ((k1, k2), v) <- xs ],
          vs <- discST e2 ys ]
discST (MapE f e) xs      = discST e [ (f k, v) | (k, v) <- xs ] 
discST (ListE e) xs       = discST (listE e) xs


-- randList k m : generate a list of length m containing random integers from the range [0..k-1]
randList :: Int -> Int -> IO [Int]
randList _ 0 = return []
randList k m = do r <- getStdRandom (randomR (0, k-1))
                  rs <- randList k (m-1)
                  return $ r : rs

-- randLists k m n : generate a list of length n containing lists of
-- length [0..m-1] with random integers in the range [0..k-1]
randLists :: Int -> Int -> Int -> IO [[Int]]
randLists _ _ n | n <= 0 = return []
randLists k m n = do size <- getStdRandom (randomR (0, m))
                     fst <- randList k size
                     rest <- randLists k m (n - size)
                     return $ fst : rest


benchs xs = [ C.bench (name ++ show n) $ C.whnf (length . f (ListE eqNat16)) x |  
              (name, f) <- [("discIO ", discIO), ("discST ", discST)], (n,x) <- xs ]

main :: IO()
main = do 
  ks <- sequence $ [randLists k m n| n <- ns]
  let xs = zip ns [zip k [1..] | k <- ks]
  C.defaultMain $ benchs xs
  where
    k = 256
    m = 10000
    ns = [-- 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000,
      1000000,  2000000, 4000000, 8000000, 16000000, 32000000]
