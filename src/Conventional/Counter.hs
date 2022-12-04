{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Conventional.Counter (counter) where
         
import Prelude hiding (not)

import Base
import Conventional.Binomial

counter :: NumberConstraint
counter vScope xs 0 = binomial vScope xs 0
counter vScope xs k = 
    let n = length xs
        x i = xs !! (i-1)
        r i j = (True, vScope $ R i j)
    in  [ [not $ x i, r i 1] | i <- [1..n-1] ] -- (1)
        ++
        [ [not $ r 1 j] | j <- [2..k] ] -- (2)
        ++
        [ [not $ r (i-1) j, r i j] -- (3)
        | i <- [2..n-1]
        , j <- [1..k]
        ]
        ++
        [ [not $ x i, not $ r (i-1) (j-1), r i j] -- (4)
        | i <- [2..n-1]
        , j <- [2..k]
        ]
        ++
        [ [not $ x i, not {- opposite on the paper -} $ r (i-1) k] -- (5)
        | i <- [2 {- 1 on the paper -} .. n]
        ] 
