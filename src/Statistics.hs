{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Statistics where

import Control.Monad

import Text.Printf

import Base
import Lib
import Evaluation

solutionNumsForExampleRate :: KN -> Float -> [(Int, Int)]
solutionNumsForExampleRate (k, n) r = reverse $
  flip map (zip (reverse [1..k]) $ iterate (/10) r) \(k', r') ->
    let m = combinationNum True (k', n)
    in  (k', floor $ r' * fromInteger (toInteger m))

exampleRate :: ParameterCNF -> Float -> IO Float
exampleRate paramCNF r = do
  let (k, n) = knOfSpace paramCNF
  let kNums = solutionNumsForExampleRate (k, n) r
  -- print kNums -- [debug]
  ps <- forM kNums \(k', num) -> do
    p <- solutionSpaceRate False (Right k') paramCNF
    return $ (1-p)^num
  -- print ps -- [debug]
  let rate = 1 - product ps
  return rate
  -- > exampleRate ((([(2,2),(2,2)],2),2),(3,3)) 0.01 -- (5,10)
  -- > exampleRate ((([(2,2),(2,3)],2),2),(2,2)) 0.01 -- (10,20)

nonZeroTo99 :: ParameterCNF -> IO (Float, Float)
nonZeroTo99 paramCNF = do
  pNonZero <- seekNZ 0.0001
  p99 <- seek99 0.01
  return (pNonZero, p99)
  where
    seekNZ r = do
      p <- exampleRate paramCNF r
      if 0 < p
        then return r
        else seekNZ $ r + 0.0001
    seek99 r = do
      p <- exampleRate paramCNF r
      if 0.99 < p
        then return r
        else seek99 $ r + 0.01

exampleRates :: ParameterCNF -> Float -> Int -> IO ()
exampleRates paramCNF d nIterate = do
  forM_ [1..nIterate] \i -> do
    let r = d * fromInteger (toInteger i)
    rate <- exampleRate paramCNF r
    putStrLn $ printf "%.8f" r ++ ": " ++ show rate
  -- > exampleRates ((([(2,2),(2,3)],2),2),(2,2)) 0.000005 20
