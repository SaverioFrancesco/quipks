{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dtmc where

import           Complex
import           Data.Matrix hiding (identity, matrix, zero, (<->), (<|>))
import qualified Data.Matrix
import           Expr
import BitQubitId

import QMatrixQuipks


vector :: (Num a) => Integer -> (Integer -> a) -> Matrix a
vector n f = matrix n 1 ( \x y -> ( f x )) 

zeroVector :: (Num a) => Matrix a
zeroVector = vector 2 (\ x -> ( 0 ))

e1C :: (Num a) => Matrix a
e1C  = vector 2 (\ x -> if x==1 then 1 else 0)

e2C :: (Num a) => Matrix a
e2C  = vector 2 (\ x -> if x==2 then 1 else 0)

log2 :: Int -> Int
log2 1 = 0
log2 2 = 1
log2 n
    | n < 1     = error "Argument of logarithm must be positive"
    | otherwise = fst $ doLog 2 1
      where
        m = 2*n-1
        doLog base acc
            | base*acc > m = (0, acc)
            | otherwise = case doLog (base*base) acc of
                            (e, a) | base*a > m -> (2*e, a)
                                   | otherwise  -> (2*e+1,a*base)

unQubitId = log2.downcast.toSize 

initialState 1 = e1C
initialState n = e1C `kronecker` (initialState (n-1)) 


---O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O

gateMulti l1 m n = m1 * gate * m1 --identityOnNQB n
    where m1= swap_multi perm
          perm= l2 ++ [  i | i <- [0..n-1], not $ any (\x-> x==i) l2 ]
          l2= map ( \x-> x-1) l1
          gate= unaryGateAt 1 m n
          
------------------------------------------------------------------------------------------------------------
--swap_multi is an implementation of matlab code for swap by Linda A.
toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]

toBinL x n = [0 | i <- [ ( (length l)+1).. n]] ++ l
        where l = toBin x

toDec lx = foldl (\acc x -> acc * 2 + x) 0 lx

permute xl [] = []
permute xl perm@(y:yl) = (xl!!y) : (permute xl yl)  

swap_multi::[Int]->Matrix (Complex Float)
swap_multi t = fromLists  $ map ins origin 
    where origin= [ toDec $ permute ( toBinL (i-1) (length t) )  t | i<-[1..2^(length t)] ]
          ins k = (\l-> ( take (k) (repeat 0) ) ++ l) . ( 1.0 :).(\l -> l ++ (take ( (2^(length t))-1-k) (repeat 0) ) ) $ []

---------------------------------------------------------------------------------------------------------------------

normalizeStateVector vm =  fmap (\x-> x /scaling_factor ) vm
        where scaling_factor = sqrt $ foldl (+) 0 (map (\x-> x*x) $ toList vm)  


identityOnNQB n = identity $ 2^n

rep 0 = identity 1
rep 1 = identity 2
rep n = (identity 2) `kronecker` ( rep (n-1))

unaryGateAt 1 m num = m `kronecker`  ( rep (num - 1))  
unaryGateAt q_id m num = if q_id== num 
                         then  rep (num-1) `kronecker` m
                         else  rep (q_id-1)  `kronecker` m `kronecker`  (  rep (num - q_id)) 

multiGateAtBottom m dim num =   ( rep (num - dim)) `kronecker` m
--multiGateAt q_id m num = if q_id== num 
--                         then rep (num-(log2 $ length m))  `kronecker` m 
--                         else rep (q_id-(log2 $ length m))  `kronecker` m `kronecker` (  rep (num - q_id))

cGateMulti l1 l3 m n = m1 * gate * m1
  where
          m1= swap_multi perm
          perm=  [  i | i <- [0..n-1], not $ any (\x-> x==i) l2 ] ++ l2
          l2= map ( \x-> x-1) $ lapp
          lapp = l1 ++ l3
          cm = cGate (length l3) m
          gate = multiGateAtBottom cm (1+(length l3)) n



cGate :: (Fractional a, Floating a, Num a) => Int -> Matrix (Complex a) -> Matrix (Complex  a)
cGate n_controls gate = (identity (2^(n) -2) <|> zero (2^(n) -2) 2) <-> (zero 2 (2^(n) -2) <|> gate)
    where n= n_controls+1


---O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O

getProbabilityOfZERO :: (Num a) => Int -> Matrix a -> a
getProbabilityOfZERO i m = foldl  (\a j -> (j^2)+a ) 0 ([vect!!u  | u <- positions])
                where{-i in 1..leng -} 
                    vect = ( take (nrows m) $  toList $ transpose m ) 
                    leng= log2 $ nrows m
                    positions = [  ind | ind <-[0..( (nrows m) -1)], even (div ind (2^(leng- i)))  ]
                                        
getProbabilityOfONE :: (Num a) => Int -> Matrix a -> a
getProbabilityOfONE i m = foldl  (\a j -> (j^2)+a ) 0 ([vect!!u  | u <- positions])
                where{-i in 1..leng -}
                    vect = ( take (nrows m) $  toList $ transpose m ) 
                    leng= log2 $ nrows m
                    positions = [  ind | ind <-[0..( (nrows m) -1)], odd (div ind (2^(leng- i)))  ]



nameToMatrix :: ( Floating a, Fractional a) => String -> Matrix (Complex a)
nameToMatrix "not"  = pauliX
nameToMatrix "X"    = pauliX
nameToMatrix "Y"    = pauliY
nameToMatrix "Z"    = pauliZ
nameToMatrix "H"    = hadamard
nameToMatrix "W"    = swapSqrt
nameToMatrix "swap" = swap
nameToMatrix "cnot" = cnot
nameToMatrix "V" =  sqrtNot
nameToMatrix n      = error $ "Gate \"" ++ show n ++ "\" is not supported yet"

nameToMatrixParameterized :: (FromDouble a, Floating a) => String -> Double -> Matrix (Complex a)
nameToMatrixParameterized "R(2pi/%)" n = phaseShift (2 * pi / fromDouble n)
nameToMatrixParameterized n       _ = error $ "Parameterized gate \"" ++ show n ++ "\" is not supported yet"


