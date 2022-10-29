{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Binary where

import Base

data Vnew
    = B Int Int
    | T Int Int
    deriving (Eq, Show)

atMost :: KN -> CNF Vnew
atMost (k, n) = cnf
    where
    theMax i = max 1 $ k - n + i
    theMin i = min i k
    log2n = head $ filter (\i -> n <= 2^i) [1..] -- floor (logBase 2 n) + if ..
    sOf i = allFTssOf log2n !! (i - 1)
    phi i g j = (sOf i !! (j + 1), VarNew $ B g j)
    cnf = 
        let a = flip map [1..n] \i ->
                    (:) (False, X i) $
                        flip map [theMax i .. theMin i] \g -> (True, VarNew $ T g i)
            b = flip concatMap [1..n] \i ->
                    flip concatMap [theMax i .. theMin i] \g ->
                        flip map [1..log2n] \j -> [(False, VarNew $ T g i), phi i g j]
        in  a ++ b
