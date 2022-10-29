module Binomial(atMost) where

import Base

atMost :: KN -> Result ()
atMost (k, n) = Result
    { newVars = []
    , cnf = map (zip $ repeat False) $ combinations (map X [1..n]) $ n - k + 1
    }
