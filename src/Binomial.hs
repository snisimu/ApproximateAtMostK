module Binomial (atMost) where

import Base

atMost :: KN -> CNFwith ()
atMost (k, n) = map (zip $ repeat False) $
    combinations (map X [1..n]) $ n - k + 1
