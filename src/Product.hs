{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Product (product) where

import Prelude hiding (not)

import Base

data Vproduct
    = A Int [Int]
    deriving (Eq, Show)

product :: NumberConstraint (Either a Vcommander) b -> Int -> NumberConstraint a (Either (String, b) Vcommander)    