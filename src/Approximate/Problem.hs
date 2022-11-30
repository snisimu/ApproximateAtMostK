{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Problem where

import Control.Monad

import Text.Printf

a = 2
pN = 10/100
pK = 1/100

solutionNum (k,n) i =
    let n' = fromInteger $ toInteger n
        k' = fromInteger $ toInteger k
        i' = fromInteger $ toInteger i
        p' = (pN-pK) / (n'-k') * i' - (pN-pK) / (n'-k') * n' + pN
        p = if p' < 0
            then 0
            else p'
    in  p * solutionNumAll
    -- > forM_ [0..n] $ \i -> putStrLn (show i ++ ": " ++ printf "%.8f" (pOf i))
