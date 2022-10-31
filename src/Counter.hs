{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Counter (atMost) where

import Base

data Vcounter = R Int Int
    deriving (Eq, Show)

atMost :: AtMost Vcounter
atMost (k, n) = 
    let n = length literals
        x i = literals !! (i-1)
        e1 = [ [not' x i, (True, Aux $ R i 1)] | i <- [1 .. n-1] ]
        e2 = [ [(False, Aux $ R 1 j)] | j <- [2..k] ]
        e3 = [ [(False, Aux $ R (i-1) j), (True, Aux $ R i j)]
                | i <- [2 .. n-1]
                , j <- [1..k]
             ]
        e4 = [ [not' x i, (False, Aux $ R (i-1) (j-1)), (True, Aux $ R i j)]
                | i <- [2 .. n-1]
                , j <- [2..k]
             ]
        e5 = [ [not' x i, (False {- True on the paper -}, Aux $ R (i-1) k)]
                | i <- [2 {- 1 on the paper -} .. n]
             ] 
    in  foldr1 (++) [e1, e2, e3, e4, e5]
