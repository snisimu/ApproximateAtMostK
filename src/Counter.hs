{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Counter (counter) where
         
import Prelude hiding (not)

import Base

data Vcounter = R Int Int
    deriving (Eq, Show)

counter :: NumberConstraint a Vcounter
counter literals k = 
    let n = length literals
        x i = liftLeft $ literals !! (i-1)
        r i j = (True, Right $ R i j)
    in  [ [not $ x i, r i 1] | i <- [1 .. n-1] ] -- (1)
        ++
        [ [not $ r 1 j] | j <- [2..k] ] -- (2)
        ++
        [ [not $ r (i-1) j, r i j] -- (3)
        | i <- [2 .. n-1]
        , j <- [1..k]
        ]
        ++
        [ [not $ x i, not $ r (i-1) (j-1), r i j] -- (4)
        | i <- [2 .. n-1]
        , j <- [2..k]
        ]
        ++
        [ [not $ x i, not {- opposite on the paper -} $ r (i-1) k] -- (5)
        | i <- [2 {- 1 on the paper -} .. n]
        ] 
