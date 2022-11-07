{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Commander (commander) where

import Prelude hiding (not)

import Control.Monad

import Data.Maybe

import Base
import Binomial

mFor :: [Literal] -> Int
mFor xs = length xs `div` 10 + 1

commander :: NumberConstraint
commander x's k' = command [] (Just $ mFor x's) x's k'
    where
    command :: [ScopeID] -> Maybe Int -> NumberConstraint
    command sIDs mbM xs k = 
        let m = fromMaybe (mFor xs) mbM
        in  if m <= 1
            then binomial xs k
            else 
                let hss = divideInto m [1..n]
                    g = length hss
                    n = length xs
                    c i j = (True, C sIDs i j)
                    c1 = flip concatMap [1..g] \i -> 
                            let lits
                                  =  [ xs !! (h-1) | h <- hss !! (i-1) ]
                                  ++ [ not $ c i j | j <- [1..k] ]
                            in  command ("c1AM" : sIDs) Nothing lits k
                                ++ atLeastBy (command ("c1AL" : sIDs) Nothing) lits k
                    c2 =
                        [ [not $ c i j, c i (j+1)]
                        | i <- [1..g]
                        , j <- [1..k-1]
                        ]
                    c3 = command ("c3" : sIDs) (Just $ m - 1) [ c i j | i <- [1..g], j <- [1..k] ] k
                in  c1 ++ c2 ++ c3
