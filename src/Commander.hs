{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Commander (commander) where

import Prelude hiding (not)

import Control.Monad

import Data.Maybe

import Base
import Binomial

s :: Int
s = 3

commander :: NumberConstraint
commander x's k' = command [] Nothing x's k'
    where
    command :: [ScopeID] -> Maybe Int -> NumberConstraint
    command sIDs mbM xs k =
        let m :: Int
            m =
                let (a, b) = length xs `divMod` s
                    m' = a + if 0 < b then 1 else 0
                in  maybe m' (min m') mbM
        in  if m <= 1
            then binomial xs k
            else
                let hss = divideInto m [1..n]
                    g = length hss
                    n = length xs
                    c i j = (True, C sIDs i j)
                    mbMnext = Just $ m-1
                    c1 = flip concatMap [1..g] \i -> 
                            let lits
                                    =  [ xs !! (h-1) | h <- hss !! (i-1) ]
                                    ++ [ not $ c i j | j <- [1..k] ]
                            in  command ("c1AM" : sIDs) mbMnext lits k
                                ++ atLeastBy (command ("c1AL" : sIDs) mbMnext) lits k
                    c2 =
                        [ [not $ c i j, c i (j+1)]
                        | i <- [1..g]
                        , j <- [1..k-1]
                        ]
                    c3 =
                        let lits = [ c i j | i <- [1..g], j <- [1..k] ]
                        in  command ("c3" : sIDs) mbMnext lits k
                in  c1 ++ c2 ++ c3
