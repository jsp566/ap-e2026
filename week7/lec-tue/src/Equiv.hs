{-# LANGUAGE GADTs #-}

----------------------------------------------------------------------------------------
{-| Module      : Equiv -- denotations of equivalence relations
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module Equiv where
import Data.Char (ord)
import Data.Bits
import Foreign.Ptr
import Foreign.StablePtr
import System.IO.Unsafe
import Order

-- Equiv T: denotations of total preorders over type T
data Equiv t where
     NatE     :: Int -> Equiv Int
     TrivE    :: Equiv t
     SumE     :: Equiv t1 -> Equiv t2 -> Equiv (Either t1 t2)
     ProdE    :: Equiv t1 -> Equiv t2 -> Equiv (t1, t2)
     MapE     :: (t1 -> t2) -> Equiv t2 -> Equiv t1
     ListE    :: Equiv t -> Equiv [t]
     BagE     :: Equiv t -> Equiv [t]
     SetE     :: Equiv t -> Equiv [t]

-- eq e : Characteristic function (equivalence/equality test) denoted by equivalence denotation e
eq :: Equiv t -> t -> t -> Bool
eq (NatE n) x y = if 0 <= x && x <= n && 0 <= y && y <= n
                  then (x == y)
                  else error "Argument out of range"
eq TrivE _ _ = True
eq (SumE e1 _) (Left x) (Left y) = eq e1 x y
eq (SumE _ _) (Left _) (Right _) = False
eq (SumE _ _) (Right _) (Left _) = False
eq (SumE _ e2) (Right x) (Right y) = eq e2 x y
eq (ProdE e1 e2) (x1, x2) (y1, y2) =
   eq e1 x1 y1 && eq e2 x2 y2
eq (MapE f e) x y = eq e (f x) (f y)
eq (ListE _) [] [] = True
eq (ListE _) [] _ = False
eq (ListE _) _ [] = False
eq (ListE e) (x : xs) (y : ys) =
   eq e x y && eq (ListE e) xs ys
eq (BagE _) [] [] = True
eq (BagE _) [] (_ : _) = False
eq (BagE e) (x : xs') ys =
   case delete e x ys of Just ys' -> eq (BagE e) xs' ys'
                         Nothing -> False
   where
    delete :: Equiv t -> t -> [t] -> Maybe [t]
    delete e v = subtract' []
       where subtract' _ [] = Nothing
             subtract' accum (x : xs) =
                if eq e x v then Just (accum ++ xs)
                            else subtract' (x : accum) xs
eq (SetE e) xs ys =
  all (member e xs) ys && all (member e ys) xs
  where member :: Equiv t -> [t] -> t -> Bool
        member _ [] _ = False
        member e (x : xs) v = eq e v x || member e xs v

instance Show (Equiv t) where
  show (NatE n) = "(NatE " ++ show n ++ ")"
  show TrivE = "TrivE"
  show (SumE e1 e2) = "(SumE " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (ProdE e1 e2) = "(ProdE " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (MapE _ r) = "(MapE " ++ "<fcn> " ++ show r ++ ")"
  show (ListE e) = "(ListE " ++ show e ++ ")"
  show (BagE e) = "(BagE " ++ show e ++ ")"
  show (SetE e) = "(BagE " ++ show e ++ ")"

-- Definitions of standard equivalence relations

-- Standard equality relations on primitive ordered types
eqUnit :: Equiv ()
eqUnit = TrivE

eqBool :: Equiv Bool
eqBool = MapE bool2sum (SumE eqUnit eqUnit)
  where bool2sum :: Bool -> Either () ()
        bool2sum False = Left ()
        bool2sum True = Right ()

-- Alternative:
eqBool' :: Equiv Bool
eqBool' = MapE bool2nat (NatE 1)
  where bool2nat :: Bool -> Int
        bool2nat False = 0
        bool2nat True = 1

eqNat8 :: Equiv Int
eqNat8 = NatE 255

eqNat16 :: Equiv Int
eqNat16 = NatE 65535

eqInt32 :: Equiv Int
eqInt32 = MapE splitW (ProdE eqNat16 eqNat16)
  where splitW :: Int -> (Int, Int)
        splitW x = (shiftR x 16 .&. 65535, x .&. 65535)

eqChar8 :: Equiv Char
eqChar8 = MapE ord eqNat8

eqChar16 :: Equiv Char
eqChar16 = MapE ord eqNat16

listE :: Equiv t -> Equiv [t]
listE e = MapE fromList (SumE eqUnit (ProdE e (listE e)))
  where fromList :: [t] -> Either () (t, [t])
        fromList []       = Left ()
        fromList (x : xs) = Right (x, xs)

eqString8 :: Equiv String
eqString8 = ListE eqChar8

-- Equality relation on IORefs (uses Haskell extension breaking abstraction of references)
eqRef :: Eq v => Equiv v
eqRef = MapE ref2int eqInt32
  where ref2int :: Eq v => v -> Int
        ref2int x = unsafePerformIO (
                    do { sptr <- newStablePtr x ;
                         return (fromIntegral (ptrToIntPtr (castStablePtrToPtr sptr))) } )

andE :: Equiv t -> Equiv t -> Equiv t
andE e1 e2 = MapE (\x -> (x, x)) (ProdE e1 e2)

-- Equivalence relation induced by ordering relation
equiv :: Order t -> Equiv t
equiv (NatO n) = NatE n
equiv TrivO = TrivE
equiv (SumL r1 r2) = SumE (equiv r1) (equiv r2)
equiv (ProdL r1 r2) = ProdE (equiv r1) (equiv r2)
equiv (MapO f r) = MapE f (equiv r)
equiv (ListL r) = ListE (equiv r)
equiv (BagO r) = BagE (equiv r)
equiv (SetO r) = SetE (equiv r)
equiv (Inv r) = equiv r