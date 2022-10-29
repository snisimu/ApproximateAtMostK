{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Binary(atMost) where

import Base

data Vaux
    = B Int Int
    | T Int Int
    deriving (Eq, Show)

atMost :: KN -> CNFwith Vaux
atMost (k, n) = 
    let theMax i = max 1 $ k - n + i
        theMin i = min i k
        log2n = head $ filter (\i -> n <= 2^i) [1..] -- floor (logBase 2 n) + if ..
        sFor i = allFTssOf log2n !! (i - 1)
        phi i g j = (sFor i !! (j - 1), VarAux $ B g j)
        a = flip map [1..n] \i ->
                (:) (False, X i) $
                    flip map [theMax i .. theMin i] \g -> (True, VarAux $ T g i)
        b = flip concatMap [1..n] \i ->
                flip concatMap [theMax i .. theMin i] \g ->
                    flip map [1..log2n] \j -> [(False, VarAux $ T g i), phi i g j]
    in  a ++ b
