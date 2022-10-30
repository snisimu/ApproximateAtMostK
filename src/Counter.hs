{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Counter (atMost) where

import Base

data Vaux = R Int Int
    deriving (Eq, Show)

atMost :: KN -> CNFwith Vaux
atMost (k, n) = 
    let e1 = flip map [1 .. n-1] \i ->
                [(False, X i), (True, VarAux $ R i 1)]
        e2 = flip map [2..k] \j -> 
                [(False, VarAux $ R 1 j)]
        e3 = flip concatMap [2 .. n-1] \i ->
                flip map [1..k] \j ->
                    [(False, VarAux $ R (i-1) j), (True, VarAux $ R i j)]
        e4 = flip concatMap [2 .. n-1] \i ->
                flip map [2..k] \j ->
                    [(False, X i), (False, VarAux $ R (i-1) (j-1)), (True, VarAux $ R i j)]
        e5 = flip map [2 {- 1 on the paper -} .. n] \i ->
                [(False, X i), (False {- True on the paper -}, VarAux $ R (i-1) k)]
    in  foldr1 (++) [e1, e2, e3, e4, e5]
