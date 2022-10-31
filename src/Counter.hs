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
    in  [ [not $ x i, (True, Aux $ R i 1)] | i <- [1 .. n-1] ]
        ++
        [ [(False, Aux $ R 1 j)] | j <- [2..k] ]
        ++
        [ [(False, Aux $ R (i-1) j), (True, Aux $ R i j)]
        | i <- [2 .. n-1]
        , j <- [1..k]
        ]
        ++
        [ [not $ x i, (False, Aux $ R (i-1) (j-1)), (True, Aux $ R i j)]
        | i <- [2 .. n-1]
        , j <- [2..k]
        ]
        ++
        [ [not $ x i, (False {- True on the paper -}, Aux $ R (i-1) k)]
        | i <- [2 {- 1 on the paper -} .. n]
        ] 
