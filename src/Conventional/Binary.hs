{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Conventional.Binary (binary) where

import Prelude hiding (not)

import Base

binary :: NumberConstraint
binary vScope xs k = 
    let n = length xs
        x i = xs !! (i-1)
        t g i = (True, vScope $ T g i)
        theMax i = max 1 $ k - n + i
        theMin i = min i k
        log2n = head $ filter (\i -> n <= 2^i) [1..] -- floor (logBase 2 n) + if ..
        s i = allCombinationssOf [False, True] log2n !! (i - 1)
        phi i g j = (s i !! (j - 1), vScope $ B g j)
    in  [ not (x i) : [ t g i | g <- [theMax i .. theMin i] ]
        | i <- [1..n]
        ]
        ++
        [ [not $ t g i, phi i g j]
        | i <- [1..n]
        , g <- [theMax i .. theMin i]
        , j <- [1..log2n]
        ]
