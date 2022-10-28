module Pairwise(atMost) where

import Base

atMost :: Int -> Int -> Result ()
atMost k n = Result
    { newvars = []
    , cnf = map ((,) False) $ combinations $ map X [1..n] $ n - k + 1
    }
