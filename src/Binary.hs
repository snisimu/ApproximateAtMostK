{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Binary (binary) where

import Prelude hiding (not)

import Base

data Vbinary
    = B Int Int
    | T Int Int
    deriving (Eq, Show)

binary :: NumberConstraint a Vbinary
binary literals k = 
    let literal's = lifts literals
        n = length literal's
        x i = literal's !! (i-1)
        t g i = (True, Aux $ Right $ T g i)
        theMax i = max 1 $ k - n + i
        theMin i = min i k
        log2n = head $ filter (\i -> n <= 2^i) [1..] -- floor (logBase 2 n) + if ..
        s i = allFTssOf log2n !! (i - 1)
        phi i g j = (s i !! (j - 1), Aux $ Right $ B g j)
    in  [ not (x i) : [ t g i | g <- [theMax i .. theMin i] ]
        | i <- [1..n]
        ]
        ++
        [ [not $ t g i, phi i g j]
        | i <- [1..n]
        , g <- [theMax i .. theMin i]
        , j <- [1..log2n]
        ]
