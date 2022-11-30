{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Problem where

import Control.Monad

import Text.Printf

import Base

a = 2 -- need?
pN = 10/100
pK = 1/100

solutionNums :: KN -> [Int]
solutionNums (k,n) =
    let limitSolutionNum = 10000 :: Int
        n' = fromInteger $ toInteger n
        k' = fromInteger $ toInteger k
        p i =
            let i' = fromInteger $ toInteger i
                p' = (pN-pK) / (n'-k') * i' - (pN-pK) / (n'-k') * n' + pN
            in  if p' < 0
                then 0
                else p'
        nn = combinationNum False (n, n)
        n's = flip map [0..n] \i -> floor $ p i * fromInteger (toInteger $ combinationNum False (i, n))
        n'max = maximum n's
        r = if limitSolutionNum < n'max
            then fromInteger (toInteger limitSolutionNum) / fromInteger (toInteger n'max)
            else 1
    in  map (floor . (*) r . fromInteger . toInteger) n's

generateProblem :: KN -> IO () 
generateProblem (k, n) = do
    let ns = solutionNums (k, n)
    cnfs <- concat <$> for [0..n] \k -> do
        let n = ns !! k
            defNewSolution s's = do

            defineSolution s's = do
                if n <= length s's
                    then return s's
                    else do
                        s <- defNewSolution s's
                        defineSolution $ s : s's
        ss <- defineSolution []

        return cnfs