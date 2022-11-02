{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Product (product) where

import Prelude hiding (not)

import Base

data Vproduct
    = A Int [Int]
    deriving (Eq, Show)

data ProductArg =
    { paKN :: KN
    , paPs :: [Int]
    , paX :: Int -> [Int]
    }
    deriving (Eq, Show)

makeProductArg :: KN -> [Int] -> (Int -> [Int]) -> IO ProductArg
makeProductArg (k, n) ps x = do


product :: ProductArg -> NumberConstraint a Vproduct
product prodArg literals k = 
    let (k', n') = paKN prodArg
        ps = paPs prodArg
        x = paX prodArg
        n = length literals
    in  case k' == k && n' == n of True ->
            let 
