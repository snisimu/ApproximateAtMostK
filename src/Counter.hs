{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Counter (atMost) where

import Base

data Vaux = R Int Int
    deriving (Eq, Show)

atMost :: KN -> CNFwith Vaux
atMost (k, n) = 
    let e1 = [ [(False, X i), (True, VarAux $ R i 1)] | i <- [1 .. n-1] ]
        e2 = [ [(False, VarAux $ R 1 j)] | j <- [2..k] ]
        e3 = [ [(False, VarAux $ R (i-1) j), (True, VarAux $ R i j)]
                | i <- [2 .. n-1]
                , j <- [1..k]
             ]
        e4 = [ [(False, X i), (False, VarAux $ R (i-1) (j-1)), (True, VarAux $ R i j)]
                | i <- [2 .. n-1]
                , j <- [2..k]
             ]
        e5 = [ [(False, X i), (False {- True on the paper -}, VarAux $ R (i-1) k)]
                | i <- [2 {- 1 on the paper -} .. n]
             ] 
    in  foldr1 (++) [e1, e2, e3, e4, e5]
