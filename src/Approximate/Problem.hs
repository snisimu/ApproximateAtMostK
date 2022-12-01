{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Approximate.Problem where

import Control.Monad

import Text.Printf

import Base
import Approximate.Lib

pN = 10/100
pK = 1/100

solutionNums :: KN -> [Int]
solutionNums (k,n) =
    let limitSolutionNum :: Int
        limitSolutionNum = 10000
        n' = fromInteger $ toInteger n
        k' = fromInteger $ toInteger k
        p i =
            let i' = fromInteger $ toInteger i
                p' = (pN-pK) / (n'-k') * i' - (pN-pK) / (n'-k') * n' + pN
            in  if p' < 0
                then 0
                else p'
        -- nn = combinationNum False (n, n)
        n's = flip map [0..n] \i -> floor $ p i * fromInteger (toInteger $ combinationNum True (i, n))
        n'max = maximum n's
        r = if limitSolutionNum < n'max
            then fromInteger (toInteger limitSolutionNum) / fromInteger (toInteger n'max)
            else 1
    in  map (floor . (*) r . fromInteger . toInteger) n's

generateProblem :: KN -> IO () 
generateProblem (k, n) = do
    let nums = solutionNums (k, n)
    cnfs <- concat <$> forM [0..n] \k -> do
        let num = nums !! k
            newSolution :: [[Int]] -> IO [Int]
            newSolution js's = do
                i <- random0toLT $ combinationNum True (k, n)
                let js = (combinations [0..n-1] k) !! i
                if notElem js js's
                    then return js
                    else newSolution js's
            defineSolution :: [[Int]] -> IO [[Int]]
            defineSolution js's = do
                if num <= length js's
                    then return js's
                    else do
                        js <- newSolution js's
                        -- print js -- [debug]
                        defineSolution $ js : js's
        jss <- defineSolution []
        let blss = map (trueIndicesToBools n) jss
            litss = flip map blss \bls ->
                flip map (zip [1..n] bls) \(i, bl) -> (bl, X i)
        -- putStrLn $ show k ++ ": " ++ show litss -- [debug]
        return $ distribution litss
    print $ take 30 cnfs
    print $ length cnfs
