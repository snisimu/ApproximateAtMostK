{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Binary (atMost) where

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
                (False, X i) :
                    [ (True, VarAux $ T g i) | g <- [theMax i .. theMin i] ]
        b = [ [(False, VarAux $ T g i), phi i g j]
             | i <- [1..n]
             , g <- [theMax i .. theMin i]
             , j <- [1..log2n]
            ]
    in  a ++ b
