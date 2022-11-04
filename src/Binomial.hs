module Binomial (binomial) where
         
import Prelude hiding (not)

import Base

binomial :: NumberConstraint
binomial xs k = combinations (map not xs) $ k + 1

finallyBinomial :: [Literal] -> Int -> NumberConstraint -> CNF
finallyBinomial xs k atMost = if shouldUseBinomial
    then binomial xs k
    else atMost xs k
    where
        n = length xs
        shouldUseBinomial -- under 300 clauses
            =  n <= 10
            || n <= 13 && k <= 2
            || n <= 25 && k <= 1
