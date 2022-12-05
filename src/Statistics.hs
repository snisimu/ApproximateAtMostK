{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Statistics where

import Control.Monad

import Base
import Lib
import Evaluation

solutionNumsForExampleRate :: KN -> Float -> [(Int, Int)]
solutionNumsForExampleRate (k, n) r = reverse $
  flip map (zip (reverse [1..k]) $ iterate (/10) r) \(k', r') ->
    let m = combinationNum True (k', n)
    in  (k', floor $ r' * fromInteger (toInteger m))

exampleRate :: ParameterCNF -> Float -> IO ()
exampleRate paramCNF r = do
  let (k, n) = knOfSpace paramCNF
  let kNums = solutionNumsForExampleRate (k, n) r
  -- print kNums -- [debug]
  ps <- forM kNums \(k', num) -> do
    p <- solutionSpaceRate False (Right k') paramCNF
    return $ (1-p)^num
  -- print ps -- [debug]
  let rate = 1 - product ps
  print rate
  -- > exampleRate ((([(2,3)],2),3),(1,1)) 10 -- (5,10)
  -- > forM_ [1..10] $ \i -> exampleRate ((([(2,3)],2),3),(1,1)) $ 0.01 * fromInteger i
