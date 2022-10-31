{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Binary (atMost) where

import Prelude hiding (not)

import Base

data Vbinary
    = B Int Int
    | T Int Int
    deriving (Eq, Show)

atMost :: AtMost Vbinary
atMost literals k = 
    let n = length literals
        x i = literals !! (i-1)
        t g i = (True, Aux $ T g i)
        theMax i = max 1 $ k - n + i
        theMin i = min i k
        log2n = head $ filter (\i -> n <= 2^i) [1..] -- floor (logBase 2 n) + if ..
        sFor i = allFTssOf log2n !! (i - 1)
        phi i g j = (sFor i !! (j - 1), Aux $ B g j)
        p1 = flip map [1..n] \i ->
                x i : [ t g i | g <- [theMax i .. theMin i] ]
        p2 =
            [ [not $ t g i, phi i g j]
            | i <- [1..n]
            , g <- [theMax i .. theMin i]
            , j <- [1..log2n]
            ]
    in  p1 ++ p2
