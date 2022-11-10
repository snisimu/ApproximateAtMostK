{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Commander (commander) where

import Prelude hiding (not)

import Control.Monad

import Data.Maybe

import Base
import Binomial

sFor :: KN -> Int
sFor (k, n) = k + n `div` 3

commander :: NumberConstraint
commander = command Nothing
    where
    command :: Maybe Int -> NumberConstraint
    command mbM vScope xs k =
        let m :: Int
            m =
                let (a, b) = length xs `divMod` sFor (k, length xs)
                    m' = a + if 0 < b then 1 else 0
                in  maybe m' (min m') mbM
        in  if m <= 1
            then binomial xs k
            else
                let n = length xs
                    hss = divideInto m [1..n]
                    g = length hss
                    c i j = (True, vScope $ C sIDs i j)
                    mbMnext = Just $ m-1
                    vScope' sID = vScope . Scope ("comm:" ++ sID)
                    c1 = flip concatMap [1..g] \i -> 
                            let lits
                                    =  [ xs !! (h-1) | h <- hss !! (i-1) ]
                                    ++ [ not $ c i j | j <- [1..k] ]
                            in  command mbMnext (vScope' "c1AM") lits k
                                ++ atLeastBy (command mbMnext (vScope' "c1AL")) lits k
                    c2 =
                        [ [not $ c i j, c i (j+1)]
                        | i <- [1..g]
                        , j <- [1..k-1]
                        ]
                    c3 =
                        let lits = [ c i j | i <- [1..g], j <- [1..k] ]
                        in  command mbMnext (vScope' "c3") lits k
                in  c1 ++ c2 ++ c3
