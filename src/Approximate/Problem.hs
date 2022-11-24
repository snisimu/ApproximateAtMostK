{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Problem where

import Control.Monad

import Text.Printf

a = 2
pN = 10/100
pK = 1/100

n = 20
k = n `div` 2

pOf i =
    let n' = fromInteger $ toInteger n
        k' = fromInteger $ toInteger k
        i' = fromInteger $ toInteger i
        x = (pN-pK) / (n'-k') * i' - (pN-pK) / (n'-k') * n' + pN
    in  if x < 0
            then 0
            else x
    -- > forM_ [0..n] $ \i -> putStrLn (show i ++ ": " ++ printf "%.8f" (pOf i))
