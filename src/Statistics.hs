{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Statistics where

import Base
import Lib
import Evaluation

solutionNumsForActualRate :: KN -> Float -> [(Int, Int)]
solutionNumsForActualRate (k,n) r = reverse $
  flip map (zip (reverse [0..k]) $ iterate (/10) r) \(k', r') ->
    let m = combinationNum True (k', n)
    in  (k', floor $ r' * fromInteger (toInteger m))

