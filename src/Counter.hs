{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Counter (counter) where
         
import Prelude hiding (not)

import Base

data Vcounter = R Int Int
    deriving (Eq, Show)

counter :: NumberConstraint Vcounter
counter literals k = 
    let n = length literals
        x i = literals !! (i-1)
        r i j = (True, Aux $ R i j)
    in  [ [not $ x i, r i 1] | i <- [1 .. n-1] ]
        ++
        [ [not $ r 1 j] | j <- [2..k] ]
        ++
        [ [not $ r (i-1) j, r i j]
        | i <- [2 .. n-1]
        , j <- [1..k]
        ]
        ++
        [ [not $ x i, not $ r (i-1) (j-1), r i j]
        | i <- [2 .. n-1]
        , j <- [2..k]
        ]
        ++
        [ [not $ x i, not {- opposite on the paper -} $ r (i-1) k]
        | i <- [2 {- 1 on the paper -} .. n]
        ] 
