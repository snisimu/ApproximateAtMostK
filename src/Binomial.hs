module Binomial where

import Base

atMost :: KN -> CNF ()
atMost (k, n) = map (zip $ repeat False) $
    combinations (map X [1..n]) $ n - k + 1
