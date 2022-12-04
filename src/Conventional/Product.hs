{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Conventional.Product (productWith) where

import Prelude hiding (not, product)

import System.Exit

import Control.Monad

import Data.List hiding (product)

import Base
import Conventional.Binomial

xvOn :: KN -> Int -> [Int]
xvOn (k, _) = \case
    1 -> replicate (k+1) 1
    i -> 
        let (d, j) = (i-2) `divMod` (k+1)
            a = d + 2
        in  replicate j 1 ++ [a] ++ replicate (k-j) 1

productWith :: NumberConstraint -> NumberConstraint
productWith atMost vScope xs k = if length xs <= k+1
    then atMost (vScope . Scope ("prod:final")) xs k
    else
        let n = length xs
            xv = xvOn (k, n)
            x i = xs !! (i-1)
            a d xvi =
                let (xva, xvb) = splitAt d $ xvi
                    xv'd = init xva ++ xvb
                in  (True, vScope $ A d xv'd)
        in  [ [not $ x i, a d $ xv i] | d <- [1 .. k+1], i <- [1..n] ]
            ++ concat
                [ productWith atMost (vScope . Scope ("prod" ++ show d)) (nub [ a d $ xv i | i <- [1..n] ]) k
                | d <- [1..k+1]
                ]

{-
productWith' :: NumberConstraint -> NumberConstraint
productWith' atMost vScope xs k = 
    let n = length xs
        xv = xvOn (k, n)
        x i = xs !! (i-1)
        a d xvi =
            let (xva, xvb) = splitAt d $ xvi
                xv'd = init xva ++ xvb
            in  (True, vScope $ A d xv'd)
    in  [ [not $ x i, a d $ xv i] | d <- [1 .. k+1], i <- [1..n] ]
        ++ concat
            [ atMost (vScope . Scope ("prod" ++ show d)) (nub [ a d $ xv i | i <- [1..n] ]) k
            | d <- [1..k+1]
            ]
-}
